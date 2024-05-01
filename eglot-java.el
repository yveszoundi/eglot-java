;;; eglot-java.el --- Java extension for the eglot LSP client  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2024 Yves Zoundi

;; Version: 1.35
;; Author: Yves Zoundi <yves_zoundi@hotmail.com> and contributors
;; Maintainer: Yves Zoundi <yves_zoundi@hotmail.com>
;; URL: https://github.com/yveszoundi/eglot-java
;; Keywords: convenience, languages
;; Package-Requires: ((emacs "26.1") (eglot "1.0") (jsonrpc "1.0.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Java extension for the eglot LSP client.
;;
;; Some of the key features include the following:
;; - Automatic installation of the Eclipse JDT LSP server (latest milestone release).
;; - Ability to pass JVM arguments to the Eclipse JDT LSP server (eglot-java-eclipse-jdt-args)
;; - Wizards for Spring, Micronaut, Quarkus, Vert.x, Maven and Gradle project creation
;; - Generic build command support for Maven and Gradle projects
;; - Basic JUnit tests support
;;
;; eglot-java dynamically modifies the "eglot-server-programs" variable,
;; you can change that behavior with the variable "eglot-java-eglot-server-programs-manual-updates"
;; - you may prefer using directly default jdtls Python script, eglot-java doesn't use that (eglot defaults to jdtls)
;; - eglot-java calls the relevant Java command directly, both for historical reasons and for potentially avoiding any Python dependency (Windows, Mac OS)
;;
;; If you're having issues with Gradle projects (auto-completion), ensure that you're using the gradle wrapper in your projects:
;; - The root cause is likely JVM incompatibilities with the bundled Eclipse Gradle version
;; - Check your default JDK version
;; - If using a recent Eclipse JDT LS snapshot, check its bundled gradle version:  https://github.com/eclipse/buildship/blob/master/org.gradle.toolingapi/META-INF/MANIFEST.MF
;; - Check the gradle compatibility matrix: https://docs.gradle.org/current/userguide/compatibility.html
;; - Use the gradle wrapper to ensure that you always have a compatible matching JVM version
;;   - Edit directly your gradle/wrapper/gradle-wrapper.properties
;;   - or download the matching Gradle version for your JVM and run: gradle wrapper
;;
;; Below is a sample configuration for your emacs init file
;;
;; (add-hook 'java-mode-hook 'eglot-java-mode)
;; (with-eval-after-load 'eglot-java
;;   (define-key eglot-java-mode-map (kbd "C-c l n") #'eglot-java-file-new)
;;   (define-key eglot-java-mode-map (kbd "C-c l x") #'eglot-java-run-main)
;;   (define-key eglot-java-mode-map (kbd "C-c l t") #'eglot-java-run-test)
;;   (define-key eglot-java-mode-map (kbd "C-c l N") #'eglot-java-project-new)
;;   (define-key eglot-java-mode-map (kbd "C-c l T") #'eglot-java-project-build-task)
;;   (define-key eglot-java-mode-map (kbd "C-c l R") #'eglot-java-project-build-refresh))
;;
;; Sometimes you may want to add/modify LSP server initialization settings.
;; JDT LS settings documentation: https://github.com/eclipse-jdtls/eclipse.jdt.ls/wiki/Running-the-JAVA-LS-server-from-the-command-line#initialize-request
;; - For basic flexibility, you can control the ":settings" node of the LSP server configuration via the variable "eglot-workspace-configuration".
;; - For greater flexibility, you can leverage the "eglot-java-user-init-opts-fn" variable.
;;   - You'll need to bind the value of the "eglot-java-user-init-opts-fn" with your own callback function.
;;   - You'll need to return a property list of valid JDT LS settings (merged with defaults)
;;
;; (setq eglot-java-user-init-opts-fn 'custom-eglot-java-init-opts)
;; (defun custom-eglot-java-init-opts (server eglot-java-eclipse-jdt)
;;   "Custom options that will be merged with default settings."
;;   '(:bundles ["/home/me/.emacs.d/lsp-bundles/com.microsoft.java.debug.plugin-0.50.0.jar"]
;;     :settings
;;     (:java
;;      (:format
;;       (:settings
;;        (:url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml")
;;        :enabled t)))))
;;
;; The behavior of the "eglot-java-run-test" function depends on the cursor location:
;; - If there's an enclosing method at the current cursor location, that specific test method will run
;; - Otherwise, all the tests in the current file will be executed
;;
;; You can upgrade an existing LSP server installation with the "eglot-java-upgrade-lsp-server" function.
;; You can upgrade an existing JUnit jar installation with the "eglot-java-upgrade-junit-jar" function.

;;; Code:

(require 'project)
(require 'eglot)
(require 'cl-lib)
(require 'url)
(require 'json)

(defgroup eglot-java
    nil
  "Interaction with a Java language server via eglot."
  :prefix "eglot-java-"
  :group 'eglot
  :link '(url-link :tag "Github" "https://github.com/yveszoundi/eglot-java"))

(defcustom eglot-java-eglot-server-programs-manual-updates
  nil
  "Do not try to automatically update eglot-server-programs"
  :type 'boolean
  :group 'eglot-java)

(defcustom eglot-java-eclipse-jdt-args
  '("-Xmx1G"
    "--add-modules=ALL-SYSTEM"
    "--add-opens"
    "java.base/java.util=ALL-UNNAMED"
    "--add-opens"
    "java.base/java.lang=ALL-UNNAMED")
  "Eclipse JDT JVM arguments."
  :type '(repeat string)
  :risky t
  :group 'eglot-java)

(defcustom eglot-java-eclipse-jdt-config-directory
  nil
  "Custom folder containing the JDT LS config.ini file to use."
  :type 'directory
  :group 'eglot-java)

(defcustom eglot-java-java-home
  nil
  "Custom JAVA_HOME location."
  :type 'directory
  :group 'eglot-java)

(defcustom eglot-java-java-program
  nil
  "Custom Java executable path location."
  :type 'file
  :group 'eglot-java)

(defcustom eglot-java-gradle-version
  nil
  "Gradle version to use if the wrapper is missing or disabled."
  :type 'file
  :group 'eglot-java)

(defcustom eglot-java-gradle-wrapper-enable
  t
  "Enable Gradle wrapper support."
  :type 'boolean
  :group 'eglot-java)

(defcustom eglot-java-eclipse-jdt-cache-directory
  (locate-user-emacs-file "eglot-java-eclipse-jdt-cache")
  "Eclipse JDT cache directory."
  :type 'string
  :group 'eglot-java)

(defcustom eglot-java-eclipse-jdt-ls-dl-metadata-url
  "https://formulae.brew.sh/api/formula/jdtls.json"
  "URL to fetch Eclipse JDT language server packaging information."
  :type 'string
  :risky t
  :group 'eglot-java)

(defcustom eglot-java-maven-repo-root-url
  "https://repo1.maven.org/maven2"
  "Maven repository root URL."
  :type 'string
  :risky t
  :group 'eglot-java)

(defcustom eglot-java-server-install-dir
  (concat user-emacs-directory "share/eclipse.jdt.ls")
  "Location of the Eclipse Java language server installation."
  :type 'directory
  :group 'eglot-java)

(defcustom eglot-java-junit-platform-console-standalone-jar
  (concat user-emacs-directory "share/junit-platform-console-standalone/junit-platform-console-standalone.jar")
  "Location of the vscode test runner."
  :type 'file
  :group 'eglot-java)

(defcustom eglot-java-workspace-folder
  (expand-file-name "~")
  "Java projects default folder."
  :type 'string
  :group 'eglot-java)

(defcustom eglot-java-user-init-opts-fn
  nil
  "User-defined function returning a property list of LSP server initialization options. The callback accepts 2 parameters (server eglot-java-eclipse-jdt)."
  :type 'function
  :group 'eglot-java)

(defcustom eglot-java-run-main-args
  nil
  "Arguments for the main class. List of strings."
  :type '(repeat string)
  :group 'eglot-java)

(defcustom eglot-java-run-main-jvm-args
  nil
  "JVM arguments for running the main class. List of strings."
  :type '(repeat string)
  :group 'eglot-java)

(defcustom eglot-java-run-main-env
  nil
  "Environment for running main. List of strings of the form ENVVARNAME=VALUE."
  :type '(repeat string)
  :group 'eglot-java)

(defcustom eglot-java-run-test-jvm-args
  nil
  "JVM arguments for running tests. List of strings."
  :type '(repeat string)
  :group 'eglot-java)

(defcustom eglot-java-run-test-env
  nil
  "Environment for running tests. List of strings of the form ENVVARNAME=VALUE."
  :type '(repeat string)
  :group 'eglot-java)

(defcustom eglot-java-debug-jvm-arg
  "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=localhost:8000"
  "JVM argument to start the debugger."
  :type 'string
  :group 'eglot-java)

(defcustom eglot-java-file-new-ask-type t
  "Whether `eglot-java-file-new' asks for the type of new file, or inserts only
the package name."
  :type 'boolean
  :group 'eglot-java)

(defconst eglot-java-filename-build-maven            "pom.xml"                   "Maven build file name.")
(defconst eglot-java-filename-build-gradle-groovy    "build.gradle"              "Gradle build file name with Groovy.")
(defconst eglot-java-filename-build-gradle-kotlin    "build.gradle.kts"          "Gradle build file name with Kotlin.")
(defconst eglot-java-filename-version-jdtls          ".eglot-java-jdtls-version" "JDT LS version file name.")
(defconst eglot-java-filename-version-junit          ".eglot-java-junit-version" "JUnit version file name.")
(defconst eglot-java-spring-io-excluded-input-params '("_links" "dependencies") "Excluded spring starter input parameters.")

(defvar eglot-java-starterkits-info-by-starterkit-name
      #s(hash-table test equal
                    data ("maven"     ()
                          "gradle"    ()
                          "spring"    (:url "https://start.spring.io"     :metadata #s(hash-table test equal))
                          "micronaut" (:url "https://launch.micronaut.io" :metadata #s(hash-table test equal))
                          "quarkus"   (:url "https://code.quarkus.io"     :metadata #s(hash-table test equal))
                          "vertx"     (:url "https://start.vertx.io"      :metadata #s(hash-table test equal))))
      "Starter kits metadata for new project wizards.")

(defvar eglot-java-project-new-directory nil "The newly created java project directory location.")
(make-variable-buffer-local 'eglot-java-project-new-directory)

(declare-function tar-untar-buffer "tar-mode" ())
(declare-function xml-get-children "xml" (node child-name))

(make-obsolete-variable 'eglot-java-eclipse-jdt-ls-download-url               'eglot-java-eclipse-jdt-ls-dl-metadata-url "1.11")
(make-obsolete-variable 'eglot-java-junit-platform-console-standalone-jar-url 'eglot-java-maven-repo-root-url            "1.14")

(defclass eglot-java-eclipse-jdt (eglot-lsp-server) ()
  :documentation "Eclipse's Java Development Tools Language Server.")

(defun eglot-java--find-java-program-from-alternatives ()
  "Find the location of the java executable in the following order:
- Check if eglot-eglot-java-java-program is set
- Check if the java program is already in the user PATH environment variable
- Check if eglot-java-java-home is set and use eglot-java-java-home/bin/java
- Check if JAVA_HOME is set and use JAVA_HOME/bin/java"
  (if eglot-java-java-program
      eglot-java-java-program
    (let ((java-executable (executable-find "java"))
          (java-home       (or eglot-java-java-home (getenv "JAVA_HOME"))))
      (if java-executable
          java-executable
        (if java-home
            (eglot-java--make-path java-home "bin" "java")
          (user-error "Cannot find java executable location!
Please pick one of the following options.
- Customize the value of eglot-java-java-program.
- Or add the folder containing the java executable to your PATH environment variable.
- Or define the JAVA_HOME environment variable."))))))

(defun eglot-java--version< (V1 V2)
  "Return t if version V1 is lower (older) than V2.
This leverages some built-in emacs version comparison functions behind the scenes.
This allows additional suffixes in versions for milestones or snapshots. e.g., 1.0.0-m1 vs. 1.0.0."
  (let* ((v1-parts (split-string V1 "-"))
         (v2-parts (split-string V2 "-"))
         (first-v1 (car v1-parts))
         (first-v2 (car v2-parts))
         (last-v1  (or (cadr v1-parts) ""))
         (last-v2  (or (cadr v2-parts) "")))
    (if (version< first-v1 first-v2)
        t
      (when (and (version= first-v1 first-v2)
                 (not (string= last-v1 last-v2)))
        (if (string= "" last-v2)
            t
          (when (not (string= "" last-v1))
            (string< last-v1 last-v2)))))))

;; Copied from https://emacs.stackexchange.com/questions/5436/function-to-merge-two-property-lists
(defun eglot-java--plist-merge (&rest plists)
  "Create a single property list from all plists in PLISTS.
The process starts by copying the first list, and then setting properties
from the other lists.  Settings in the last list are the most significant
ones and overrule settings in the other lists."
  (let ((rtn (copy-sequence (pop plists)))
        p v ls)
    (while plists
      (setq ls (pop plists))
      (while ls
        (setq p (pop ls) v (pop ls))
        (setq rtn (plist-put rtn p v))))
    rtn))

(cl-defmethod eglot-initialization-options ((server eglot-java-eclipse-jdt))
  "Passes through required jdt initialization options."
  (let ((settings-plist `(:extendedClientCapabilities
                          (:classFileContentsSupport t)
                          :workspaceFolders
                          [,@(cl-delete-duplicates
                              (mapcar #'eglot--path-to-uri
                                      (let* ((root (project-root (eglot--project server))))
                                        (cons root
                                              (mapcar
                                               #'file-name-directory
                                               (append
                                                (file-expand-wildcards (concat root "/*" eglot-java-filename-build-maven))
                                                (file-expand-wildcards (concat root "/*" eglot-java-filename-build-gradle-groovy))
                                                (file-expand-wildcards (concat root "/*" eglot-java-filename-build-gradle-kotlin))
                                                (file-expand-wildcards (concat root "/.project")))))))
                              :test #'string=)]
                          ,@(if-let ((home (or eglot-java-java-home
                                               (getenv "JAVA_HOME")
                                               (ignore-errors
                                                 (expand-file-name
                                                  ".."
                                                  (file-name-directory
                                                   (file-chase-links (executable-find "javac"))))))))
                                `(:settings (:java
                                             (:home ,home)
                                             :import
                                             (:gradle (:enabled t)
                                                      ,@(when eglot-java-gradle-version
                                                          (:version eglot-java-gradle-version))
                                                      :wrapper (:enabled ,(or eglot-java-gradle-wrapper-enable :json-false)))))
                              (ignore (eglot--warn "JAVA_HOME environment is not set"))))))
    (if eglot-java-user-init-opts-fn
        (if (fboundp eglot-java-user-init-opts-fn)
            (let ((user-opts (funcall eglot-java-user-init-opts-fn server eglot-java-eclipse-jdt)))
              (eglot-java--plist-merge settings-plist user-opts))
          (warn (format "The LSP settings initialization function is not bound! %s!" eglot-java-user-init-opts-fn)))
      settings-plist)))

(defun eglot-java--eclipse-jdt-contact (interactive)
  "Return a contact for connecting to eclipse.jdt.ls server, as a cons cell.
If INTERACTIVE, prompt user for details."
  (cl-labels
      ((is-the-jar
           (path)
         (and (string-match-p
               "org\\.eclipse\\.equinox\\.launcher_.*\\.jar$"
               (file-name-nondirectory path))
              (file-exists-p path))))
    (let* ((classpath (or (getenv "CLASSPATH") path-separator))
           (cp-jar (cl-find-if #'is-the-jar (split-string classpath path-separator)))
           (jar cp-jar)
           (dir
            (cond
              (jar (file-name-as-directory
                    (expand-file-name ".." (file-name-directory jar))))
              (interactive
               (expand-file-name
                (read-directory-name
                 (concat "Path to eclipse.jdt.ls directory (could not"
                         " find it in CLASSPATH): ")
                 nil nil t)))
              (t (error "Could not find eclipse.jdt.ls jar in CLASSPATH"))))
           (repodir
            (concat dir
                    "org.eclipse.jdt.ls.product/target/repository/"))
           (repodir (if (file-directory-p repodir) repodir dir))
           (config
            (if eglot-java-eclipse-jdt-config-directory
                (file-name-as-directory (expand-file-name eglot-java-eclipse-jdt-config-directory))
              (concat
               repodir
               (cond
                ((string= system-type "darwin") "config_mac")
                ((string= system-type "windows-nt") "config_win")
                (t "config_linux")))))
           (workspace
            (expand-file-name (md5 (project-root (eglot--current-project)))
                              eglot-java-eclipse-jdt-cache-directory)))
      (unless jar
        (setq jar
              (cl-find-if #'is-the-jar
                          (directory-files-recursively repodir "^org.eclipse.equinox.launcher_.*.jar$" t))))
      (unless (and jar (file-exists-p jar) (file-directory-p config))
        (error "Could not find required eclipse.jdt.ls files (build required?)"))
      (when (and interactive (not cp-jar)
                 (y-or-n-p (concat "Add path to the server program "
                                   "to CLASSPATH environment variable?")))
        (setenv "CLASSPATH" (concat (getenv "CLASSPATH") path-separator jar)))
      (unless (file-directory-p workspace)
        (make-directory workspace t))
      (cons 'eglot-java-eclipse-jdt
            (nconc
             (list
              (eglot-java--find-java-program-from-alternatives)
              "-Declipse.application=org.eclipse.jdt.ls.core.id1"
              "-Dosgi.bundles.defaultStartLevel=4"
              "-Declipse.product=org.eclipse.jdt.ls.core.product")
             eglot-java-eclipse-jdt-args
             (list
              "-jar" jar
              "-configuration" config
              "-data" workspace))))))

(cl-defmethod eglot-execute-command
    ((_server eglot-java-eclipse-jdt) (_cmd (eql java.apply.workspaceEdit)) arguments)
  "Eclipse JDT breaks spec and replies with edits as arguments."
  (mapc #'eglot--apply-workspace-edit arguments))

(defun eglot-java--file-read-trim (file-to-read)
  "Read contents of a file into a string removing any new lines or whitespace.
FILE-TO-READ is the file to parse."
  (with-temp-buffer
    (insert-file-contents file-to-read)
    (replace-regexp-in-string "[:blank:]" ""
                              (replace-regexp-in-string "[\r\n|\n$]" ""
                                                        (buffer-string)))))

(defun eglot-java--download-file (source-url dest-location)
  "Download a file from a URL at SOURCE-URL and save it to file at DEST-LOCATION."
  (let* ((dest-dir     (file-name-directory dest-location))
         (dest-abspath (expand-file-name dest-location)))
    (unless (file-exists-p dest-dir)
      (make-directory dest-dir t))
    (message "Downloading %s\n to %s." source-url dest-abspath)
    (url-copy-file  source-url dest-abspath t)))

(defun eglot-java--project-try (dir)
  "Return project instance if DIR is part of a Java project.
Otherwise returns nil"
  (let ((root (or (locate-dominating-file dir eglot-java-filename-build-maven)
                  (locate-dominating-file dir ".project")
                  (locate-dominating-file dir eglot-java-filename-build-gradle-groovy)
                  (locate-dominating-file dir eglot-java-filename-build-gradle-kotlin))))
    (and root (cons 'java root))))

(cl-defmethod project-root ((project (head java)))
  "Get the root of a JAVA PROJECT."
  (cdr project))

(defun eglot-java--find-equinox-launcher ()
  "Find the equinox jar launcher in the LSP plugins directory."
  (let* ((install-dir           (file-name-as-directory
                                 (expand-file-name eglot-java-server-install-dir)))
         (equinox-launcher-jar  (car (directory-files-recursively install-dir
                                                      "^org.eclipse.equinox.launcher_.*.jar$"
                                                      t))))
    (when (not equinox-launcher-jar)
      (user-error "Could not find Eclipse OSGI jar launcher!"))
    (expand-file-name equinox-launcher-jar install-dir)))

(defun eglot-java--eclipse-contact (_interactive)
  "Setup the classpath in an INTERACTIVE fashion."
  (let ((cp (getenv "CLASSPATH")))
    (setenv "CLASSPATH" (concat cp path-separator (eglot-java--find-equinox-launcher)))
    (unwind-protect
         (eglot-java--eclipse-jdt-contact nil)
      (setenv "CLASSPATH" cp))))

(defun eglot-java--project-name-maven (root)
  "Return the name of a Maven project in the folder ROOT.
This extracts the project name from the Maven POM (artifactId)."
  (let* ((pom    (expand-file-name eglot-java-filename-build-maven root))
         (xml    (xml-parse-file pom))
         (parent (car xml)))
    (caddar (xml-get-children parent 'artifactId))))

(defun eglot-java--project-gradle-p (root)
  "Check if a project stored in the folder ROOT is using Gradle as build tool."
  (or (file-exists-p (expand-file-name eglot-java-filename-build-gradle-groovy
                                       (file-name-as-directory root)))
      (file-exists-p (expand-file-name eglot-java-filename-build-gradle-kotlin
                                       (file-name-as-directory root)))))

(defun eglot-java--project-name-gradle (root)
  "Return the name of a Gradle project in the folder ROOT.
If a settings.gradle file exists, it'll be parsed to extract the project name.
Otherwise the basename of the folder ROOT will be returned."
  (let* ((build-file-groovy (expand-file-name "settings.gradle" root))
         (build-file-kotlin (expand-file-name "settings.gradle.kts" root))
         (build-file (if (file-exists-p build-file-groovy)
                         build-file-groovy
                       build-file-kotlin)))
    (if (file-exists-p build-file)
        (let ((gradle-settings (condition-case nil
                                   (with-temp-buffer
                                     (insert-file-contents build-file)
                                     (goto-char (point-min))
                                     (search-forward "rootProject.name")
                                     (search-forward "=")
                                     (buffer-substring (point) (line-end-position)))
                                 (error nil))))
          (if gradle-settings
              (string-trim (cl-reduce
                            (lambda (acc item)
                              (replace-regexp-in-string item "" acc))
                            '("'" "\"")
                            :initial-value gradle-settings))
            (file-name-nondirectory (directory-file-name (file-name-directory build-file)))))
      (file-name-nondirectory (directory-file-name (file-name-directory build-file))))))

(defun eglot-java--make-path (root-dir &rest path-elements)
  "Compose a path from a base folder ROOT-DIR and a set of items PATH-ELEMENTS."
  (let ((new-path          (expand-file-name root-dir))
        (new-path-elements path-elements))
    (dolist (p new-path-elements)
      (setq new-path (concat (file-name-as-directory new-path) p)))
    new-path))

(defun eglot-java--project-name (root)
  "Return the Java project name stored in a given folder ROOT."
  (if (eglot-java--project-gradle-p root)
      (eglot-java--project-name-gradle root)
    (eglot-java--project-name-maven root)))

(defun eglot-java--file--test-p (file-path)
  "Tell if a file located at FILE-PATH is a test class."
  (eglot-execute-command
   (eglot-java--find-server)
   "java.project.isTestFile"
   (vector (eglot--path-to-uri file-path))))

(defun eglot-java--project-classpath (filename scope)
  "Return the classpath for a given FILENAME and SCOPE."
  (plist-get (eglot-execute-command (eglot-java--find-server)
                                    "java.project.getClasspaths"
                                    (vector (eglot--path-to-uri filename)
                                            (json-encode `(("scope" . ,scope)))))
             :classpaths))

(defun eglot-java-file-new ()
  "Create a new class."
  (interactive)
  (let* ((class-by-type     #s(hash-table
                               size 6
                               test equal
                               data ("Class"      "public class %s {\n\n}"
                                                  "Record"     "public record %s () {}"
                                                  "Enum"       "public enum %s {\n\n}"
                                                  "Interface"  "public interface %s {\n\n}"
                                                  "Annotation" "public @interface %s {\n\n}"
                                                  "Test"       "import org.junit.jupiter.api.Assertions;\n
import org.junit.jupiter.api.Test;\n\npublic class %s {\n\n}")))
         (source-list       (eglot-execute-command
                             (eglot-java--find-server)
                             "java.project.listSourcePaths" (list)))
         (source-paths      (mapcar
                             #'identity
                             (car  (cl-remove-if-not #'vectorp source-list))))
         (display-paths     (mapcar (lambda (e)
                                      (plist-get e :displayPath))
                                    source-paths))
         (selected-path     (completing-read "Source path : " display-paths))
         (fqcn              (read-string "Class name: "
                                         (let ((package (eglot-java--symbol-name-for-type (eglot-java--document-symbols) "Package")))
                                           (unless (string-empty-p package)
                                             (concat package ".")))))
         (class-type        (when eglot-java-file-new-ask-type
                                (completing-read "Type: " (hash-table-keys class-by-type))))
         (selected-source   (car (cl-remove-if-not
                                  (lambda (e)
                                    (string= (plist-get e :displayPath) selected-path))
                                  source-paths )))
         (path-elements     (split-string fqcn "\\."))
         (new-paths         (butlast path-elements))
         (dest-folder       (file-name-as-directory
                             (expand-file-name
                              (mapconcat #'identity new-paths "/")
                              (plist-get selected-source :path))))
         (simple-class-name (car (last path-elements))))
    (unless (file-exists-p dest-folder)
      (make-directory dest-folder t))
    (find-file
     (concat (file-name-as-directory dest-folder)
             (format "%s.java" simple-class-name)))
    (save-buffer)
    (when new-paths
      (insert "package " (mapconcat #'identity new-paths ".") ";\n\n"))
    (when class-type
      (insert (format (gethash class-type class-by-type)
                      simple-class-name)))
    (save-buffer)))

(defun eglot-java--do-find-nearest-class-at-point (acc syms idx elt-type cursor-location)
  "Find the nearest class at the current cursor location given a list of parameters.
ACC represents the string accumulator holding the class name hierarchy, not prefixed with the package name.
SYMS represents the document symbols.
IDX represents the current index being traversed in the list of document symbols.
ELT-TYPE represents the symbol type name to find, which is 'Class'.
CURSOR-LOCATION represents a property list with the line information."
  (if (> (length syms) idx)
      (let* ((elt          (aref syms idx))
             (elt-children (plist-get elt :children))
             (elem-kind    (plist-get elt :kind))
             (elem-type    (cdr (assoc elem-kind eglot--symbol-kind-names))))
        (if (and (string= elem-type elt-type)
                 (>= (plist-get cursor-location :line) (plist-get (plist-get (plist-get elt :selectionRange) :start) :line))
                 (<= (plist-get cursor-location :line) (plist-get (plist-get (plist-get elt :range) :end) :line)))
            (let ((newacc (if acc
                              (format "%s.%s" acc (plist-get elt :name))
                            (plist-get elt :name))))
              (eglot-java--do-find-nearest-class-at-point newacc elt-children 0 elt-type cursor-location))
          (eglot-java--do-find-nearest-class-at-point acc syms (+ 1 idx) elt-type cursor-location)))
    acc))

(defun eglot-java--do-find-nearest-method-at-point (syms idx elt-type cursor-location)
  "Find the nearest method at the current cursor location given a list of parameters.
SYMS represents the document symbols.
IDX represents the current index being traversed in the list of document symbols.
ELT-TYPE represents the symbol type name to find which is 'Method'.
CURSOR-LOCATION represents a property list with the line information."
  (when (> (length syms) idx)
    (let* ((elt          (aref syms idx))
           (elt-children (plist-get elt :children))
           (elem-kind    (plist-get elt :kind))
           (elem-type    (cdr (assoc elem-kind eglot--symbol-kind-names))))
      (if (and (string= elem-type elt-type)
               (>= (plist-get cursor-location :line) (plist-get (plist-get (plist-get elt :selectionRange) :start) :line))
               (<= (plist-get cursor-location :line) (plist-get (plist-get (plist-get elt :range) :end)   :line)))
          (substring (plist-get elt :name)
                     0
                     (- (plist-get (plist-get (plist-get elt :selectionRange) :end)   :character)
                        (plist-get (plist-get (plist-get elt :selectionRange) :start) :character)))
        (let ((res (eglot-java--do-find-nearest-method-at-point elt-children 0 elt-type cursor-location)))
          (if res
              res
            (eglot-java--do-find-nearest-method-at-point syms (+ 1 idx) elt-type cursor-location)))))))

(defun eglot-java--find-nearest-method-at-point ()
  "Find the nearest method at the current cursor location."
  (let* ((syms         (eglot-java--document-symbols))
         (line-current (- (line-number-at-pos) 1))
         (package-name (eglot-java--symbol-name-for-type syms "Package"))
         (class-name   (eglot-java--do-find-nearest-class-at-point nil syms 0 "Class" (list :line line-current))))
    (when class-name
      (when-let ((method-name (eglot-java--do-find-nearest-method-at-point syms 0 "Method" (list :line line-current))))
        (format "%s%s%s#%s"
                package-name
                (if (= (length package-name) 0)
                    ""
                  ".")
                class-name
                method-name)))))

(defun eglot-java--record-version-info (ver-number dest-file)
  "Store a version VER-NUMBER to a file DEST-FILE."
  (with-temp-file dest-file
    (insert (format "%s\n" ver-number))))

;; Implementation copied from https://stackoverflow.com/questions/13337969/how-do-i-parse-xml-from-a-url-in-emacs
(defun eglot-java--parse-xml-buffer (&optional buffer)
  "Parse an XML buffer, and return a XML tree as list.
BUFFER may be a buffer or the name of an existing buffer.
If BUFFER is omitted, current-buffer is parsed."
  (or buffer
      (setq buffer (current-buffer)))
  (set-buffer buffer)
  (set-buffer-multibyte t)
  (let ((start (save-excursion
                 (goto-char (point-min))
                 (and (re-search-forward "<\?xml" (point-max) t)
                      (match-beginning 0)))))
    (and start
         (xml-parse-region start (point-max)))))

(defun eglot-java--fetch-junit-latest-version (metadata-xml-url)
  "Fetch the latest version for the Maven artifact junit-platform-console-standalone.
METADATA-XML-URL is the Maven URL containing a maven-metadata.xml file for the artifact junit-platform-console-standalone."
  (with-temp-buffer
    (url-insert-file-contents metadata-xml-url)
    (caddar (xml-get-children
             (car (xml-get-children (car (eglot-java--parse-xml-buffer)) 'versioning))
             'release))))

(defun eglot-java--find-latest-junit-metadata (maven-repo-root-url)
  "Returns the latest JUnit jar URL given a Maven repository root URL.
  MAVEN-REPO-ROOT-URL The Maven repository URL such as https://repo1.maven.org/maven2."
  (let* ((artifact-id    "junit-platform-console-standalone")
         (maven-root-url (if (string-match-p "/\\'" maven-repo-root-url)
                             (format "%sorg/junit/platform/%s" maven-repo-root-url artifact-id)
                           (format "%s/org/junit/platform/%s" maven-repo-root-url artifact-id)))
         (junit-version (eglot-java--fetch-junit-latest-version (format "%s/%s" maven-root-url "maven-metadata.xml"))))
    (list :download-version junit-version
          :download-url     (format "%s/%s/%s-%s.jar"
                                    maven-root-url
                                    junit-version
                                    artifact-id
                                    junit-version))))

(defun eglot-java--install-junit-jar (junit-jar-path)
  "Install the JUnit standalone console jar file at a location JUNIT-JAR-PATH"
  (let* ((download-metadata (eglot-java--find-latest-junit-metadata eglot-java-maven-repo-root-url))
         (download-version  (plist-get download-metadata :download-version))
         (download-url      (plist-get download-metadata :download-url))
         (version-file-dir  (file-name-directory (expand-file-name junit-jar-path)))
         (version-file      (expand-file-name eglot-java-filename-version-junit version-file-dir)))
    (eglot-java--download-file download-url (expand-file-name junit-jar-path))
    (eglot-java--record-version-info download-version version-file)))

(defun eglot-java-run-test (debug)
  "Run a test class or method (at point). With a prefix argument the
JVM is started in debug mode."
  (interactive "P")
  (let* ((default-directory    (project-root (project-current t)))
         (fqcn                 (or (eglot-java--find-nearest-method-at-point) (eglot-java--class-fqcn)))
         (cp                   (eglot-java--project-classpath (buffer-file-name) "test"))
         (current-file-is-test (not (equal ':json-false (eglot-java--file--test-p (buffer-file-name))))))
    (unless (file-exists-p (expand-file-name eglot-java-junit-platform-console-standalone-jar))
      (eglot-java--install-junit-jar eglot-java-junit-platform-console-standalone-jar))
    (if current-file-is-test
        (let ((compilation-environment eglot-java-run-test-env))
          (compile
           (concat (eglot-java--find-java-program-from-alternatives)
                   (when debug (concat " " eglot-java-debug-jvm-arg))
                   " "
                   (mapconcat #'identity eglot-java-run-test-jvm-args " ")
                   " -jar "
                   "\""
                   (expand-file-name eglot-java-junit-platform-console-standalone-jar)
                   "\""
                   " execute"
                   (if (string-match-p "#" fqcn)
                       " -m "
                     " -c ")
                   fqcn
                   " -cp "
                   "\""
                   (mapconcat #'expand-file-name cp path-separator)
                   "\""
                   )))
      (user-error "No test found in current file! Is the file saved?"))))

(defun eglot-java-run-main (debug)
  "Run a main class. With a prefix argument the JVM is started in
debug mode."
  (interactive "P")
  (let* ((default-directory (project-root (project-current t)))
         (fqcn              (eglot-java--class-fqcn))
         (cp                (eglot-java--project-classpath (buffer-file-name) "runtime")))
    (if fqcn
        (let ((compilation-environment eglot-java-run-main-env))
          (compile
           (concat (eglot-java--find-java-program-from-alternatives)
                   (when debug (concat " " eglot-java-debug-jvm-arg))
                   " "
                   (mapconcat #'identity eglot-java-run-main-jvm-args " ")
                   " -cp "
                   "\"" (mapconcat #'expand-file-name cp path-separator) "\""
                   " "
                   fqcn
                   " "
                   (mapconcat #'identity eglot-java-run-main-args " "))))
      (user-error "No main method found in this file! Is the file saved?"))))

(defun eglot-java--class-fqcn ()
  "Return the fully qualified name of a given class."
  (let* ((document-symbols (eglot-java--document-symbols))
         (package-name     (eglot-java--symbol-name-for-type document-symbols "Package"))
         (class-name       (eglot-java--symbol-name-for-type document-symbols "Class"))
         (package-suffix   (if (string= "" package-name)
                               package-name
                             ".")))
    (concat package-name package-suffix class-name)))

(defun eglot-java--symbol-name-for-type (symbols symbol-type)
  "Extract the symbol value for a given SYMBOL-TYPE from a symbol table SYMBOLS."
  (let ((symbol-details (cl-find-if
                         (lambda (elem)
                           (let* ((elem-kind (plist-get elem :kind))
                                  (elem-type (cdr (assoc elem-kind eglot--symbol-kind-names))))
                             (string= elem-type symbol-type)))
                         symbols)))
    (if symbol-details
        (plist-get symbol-details :name)
      "")))

(defun eglot-java--document-symbols ()
  "Fetch the document symbols/tokens."
  (jsonrpc-request
   (eglot-java--find-server)
   :textDocument/documentSymbol
   (list :textDocument (list :uri (eglot--path-to-uri (buffer-file-name))))))

(defun eglot-java--get-initializr-json (url accept-header)
  "Retrieve the Spring initializr JSON model from a given URL and ACCEPT-HEADER."
  (let ((url-request-method        "GET")
        (url-request-extra-headers `(("Accept" . ,accept-header)))
        (url-request-data          (mapconcat (lambda (arg)
                                                (concat (url-hexify-string (car arg))
                                                        "="
                                                        (url-hexify-string (cdr arg))))
                                              (list)
                                              "&")))
    (message "Downloading JSON data from %s." url)
    (eglot-java--read-json-from-url url)))

(defun eglot-java--project-new-maven ()
  "Create a new Maven project."
  (let ((mvn-project-parent-dir    (read-directory-name "Enter parent directory: "))
        (mvn-group-id              (read-string         "Enter group id: "))
        (mvn-artifact-id           (read-string         "Enter artifact id: "))
        (mvn-archetype-artifact-id (read-string         "Enter archetype artifact id: " "maven-archetype-quickstart")))
    (unless (file-exists-p mvn-project-parent-dir)
      (make-directory mvn-project-parent-dir t))
      (let* ((b        (eglot-java--build-run mvn-project-parent-dir
                                              (eglot-java--build-executable "mvn" "mvnw" mvn-project-parent-dir)
                                              (concat " archetype:generate "
                                                      " -DgroupId=" mvn-group-id
                                                      " -DartifactId=" mvn-artifact-id
                                                      " -DarchetypeArtifactId=" mvn-archetype-artifact-id
                                                      " -DinteractiveMode=false")))
             (dest-dir (expand-file-name mvn-artifact-id mvn-project-parent-dir))
             (p        (get-buffer-process b)))
        (with-current-buffer b
          (setq eglot-java-project-new-directory dest-dir))
      (set-process-sentinel p #'eglot-java--project-new-process-sentinel))))

(defun eglot-java--project-new-gradle ()
  "Create a new Gradle project."
  (let* ((gradle-project-parent-dir (read-directory-name "Enter parent directory:"))
         (gradle-project-name       (read-string         "Enter project name (no spaces): "))
         (init-dsls                 '("kotlin" "groovy"))
         (init-types                '("java-application" "java-library" "java-gradle-plugin" "basic"))
         (init-test-frameworks      '("junit-jupiter" "spock" "testng"))
         (selected-dsl              (completing-read "Select init DSL: "
                                                     init-dsls
                                                     nil
                                                     t
                                                     (car init-dsls)))
         (selected-project-type     (completing-read "Select init type: "
                                                     init-types
                                                     nil
                                                     t
                                                     (car init-types)))
         (selected-test-framework   (completing-read "Select test framework: "
                                                     init-test-frameworks
                                                     nil
                                                     t
                                                     (car init-test-frameworks)))
         (selected-project-dir      (expand-file-name gradle-project-name gradle-project-parent-dir)))

    (unless (file-exists-p selected-project-dir)
      (make-directory selected-project-dir t))

    (let ((b (eglot-java--build-run
              selected-project-dir
              (eglot-java--build-executable "gradle" "gradlew" selected-project-dir)
              (mapconcat #'identity
                         (list "init"
                               "--type"
                               selected-project-type
                               "--test-framework"
                               selected-test-framework
                               "--dsl"
                               selected-dsl)
                         " "))))
      (with-current-buffer b
        (setq eglot-java-project-new-directory selected-project-dir))

      (set-process-sentinel (get-buffer-process b) #'eglot-java--project-new-process-sentinel))))

(defun eglot-java--project-startercache-micronaut (root-url)
  "Cache Micronaut project wizard settings from a URL at ROOT-URL."
  (let* ((select-opts-response (eglot-java--get-initializr-json
                                (concat root-url "/select-options")
                                "application/json"))
         (ht (make-hash-table :test 'equal))
         (ht-opts (make-hash-table :test 'equal :size 10))
         (ht-features (make-hash-table :test 'equal :size 10)))
    (maphash (lambda (key value)
               (puthash key
                        (list :default (gethash "value" (gethash "defaultOption" value))
                              :options (mapcar (lambda (elt)
                                                 (gethash "value" elt))
                                               (gethash "options" value)))
                        ht-opts))
             select-opts-response)
    (puthash :options ht-opts ht)
    (let ((ht-features (make-hash-table :test 'equal :size 10)))
      (mapc (lambda (elt)
              (let* ((url (concat root-url "/application-types/" elt "/features"))
                     (response (eglot-java--get-initializr-json url "application/json"))
                     (app-feats (make-hash-table :test 'equal :size 10)))
                (mapc (lambda (feature)
                        (puthash (gethash "title" feature) (gethash "name" feature) app-feats))
                      (gethash "features" response))
                (puthash elt app-feats ht-features)))
            (plist-get  (gethash "type" ht-opts) :options))
      (puthash  :features ht-features ht)
      ht)))

(defun eglot-java--project-new-micronaut ()
  "Create a new Micronaut project."
  (let* ((starter-settings   (gethash "micronaut" eglot-java-starterkits-info-by-starterkit-name))
         (starter-url        (plist-get starter-settings :url))
         (starter-metadata   (plist-get starter-settings :metadata))
         (cc-opts            (gethash :options starter-metadata))
         (cc-feats           (gethash :features starter-metadata))
         (texts-opts         #s(hash-table test equal
                                           data ("name" (:title "Name" :value "demo")
                                                 "package" (:title "Base Package" :value "com.example"))))
         (texts              (make-hash-table :test 'equal))
         (selects            (make-hash-table :test 'equal))
         (feats              (list)))
    (maphash (lambda (k v)
               (puthash k
                        (read-string (format "%s : " (plist-get v :title))
                                     (plist-get v :value))
                        texts))
             texts-opts)
    (maphash (lambda (k v)
               (puthash k
                        (completing-read (format "Select %s: " k)
                                         (plist-get v :options)
                                         nil
                                         t
                                         (plist-get v :default))
                        selects))
             cc-opts)
    (let ((featz (gethash (gethash "type" selects) cc-feats)))
      (mapc (lambda (item)
              (add-to-list 'feats (gethash item featz)))
            (completing-read-multiple "Select features (comma separated, TAB to add more): "
                                      (hash-table-keys featz)
                                      nil
                                      t))
      (let ((app-type (gethash "type" selects))
            (app-name (gethash "name" texts))
            (app-pkg (gethash "package" texts))
            (new-selects (list))
            (dest-dir     (read-directory-name "Project Parent directory: "
                                               (expand-file-name
                                                eglot-java-workspace-folder))))
        (remhash "type" selects)
        (maphash (lambda (k v)
                   (add-to-list 'new-selects (list k v)))
                 selects)
        (unless (file-exists-p dest-dir)
          (make-directory dest-dir t))
        (let ((dest-file-name (expand-file-name (concat (format-time-string "%Y-%m-%d_%N") ".zip")
                                                dest-dir))
              (source-url (format "%s/create/%s/%s?%s"
                                  starter-url
                                  app-type
                                  (concat app-pkg
                                          (if (= (length app-pkg) 0)
                                              ""
                                            ".")
                                          app-name)
                                  (url-build-query-string
                                   (append new-selects
                                           (list
                                            (list "features" (mapconcat 'identity feats ","))))))))
          (url-copy-file source-url dest-file-name t)
          (dired (file-name-directory dest-file-name))
          (revert-buffer))))))

(defun eglot-java--project-startercache-spring (root-url)
  "Cache Spring project wizard settings from a URL at ROOT-URL."
  (eglot-java--get-initializr-json root-url "application/vnd.initializr.v2.2+json"))

(defun eglot-java--project-new-spring ()
  "Create a new Spring project."
  (let* ((starter-settings  (gethash "spring" eglot-java-starterkits-info-by-starterkit-name))
         (starter-url       (plist-get starter-settings :url))
         (starter-metadata  (plist-get starter-settings :metadata))
         (elems             (cl-remove-if
                             (lambda (node-name)
                               (member node-name eglot-java-spring-io-excluded-input-params))
                             (hash-table-keys starter-metadata)))
         (simple-params     (mapcar
                             (lambda (p)
                               (let ((elem-type (gethash "type" (gethash p starter-metadata))))
                                 (cond ((or (string= "single-select" elem-type)
                                            (string= "action" elem-type))
                                        (list
                                         p
                                         (completing-read
                                          (format "Select %s: " p)
                                          (mapcar
                                           (lambda (f)
                                             (gethash "id" f))
                                           (gethash "values" (gethash p
                                                                      starter-metadata)))
                                          nil t (gethash "default" (gethash p
                                                                            starter-metadata)))))
                                       ((string= "text" elem-type)
                                        (list p
                                              (read-string (format "Select %s: " p)
                                                           (gethash "default" (gethash p
                                                                                       starter-metadata))))))))
                             elems))
         (simple-deps       (completing-read-multiple "Select dependencies (comma separated, TAB to add more): "
                                                             (mapcan
                                                              (lambda (s)
                                                                (mapcar
                                                                 (lambda (f)
                                                                   (gethash "id" f))
                                                                 s))
                                                              (mapcar
                                                               (lambda (x)
                                                                 (gethash "values" x))
                                                               (gethash "values"  (gethash "dependencies" starter-metadata ))))))
         (dest-dir          (read-directory-name "Project Directory: "
                                                 (expand-file-name (cadr (assoc "artifactId" simple-params)) eglot-java-workspace-folder))))
    (unless (file-exists-p dest-dir)
      (make-directory dest-dir t))
    (let ((large-file-warning-threshold nil)
          (dest-file-name (expand-file-name (concat (format-time-string "%Y-%m-%d_%N") ".zip")
                                            dest-dir))
          (source-url     (format "%s?%s"
                                  (format "%s/starter.zip" starter-url)
                                  (url-build-query-string (append simple-params
                                                                  (list
                                                                   (list "dependencies"
                                                                         (mapconcat
                                                                          #'identity
                                                                          (nconc simple-deps)
                                                                          "," ))))))))
      (url-copy-file source-url dest-file-name t)
      (dired (file-name-directory dest-file-name))
      (revert-buffer))))

(defun eglot-java--project-startercache-quarkus (root-url)
  "Cache Quarkus project wizard settings from a URL at ROOT-URL."
  (let* ((response-streams            (eglot-java--get-initializr-json
                                       (concat root-url "/api/streams") "application/json"))
         (response-extensions         (eglot-java--get-initializr-json
                                       (concat root-url "/api/extensions?platformOnly=true") "application/json"))
         (metadata-by-quarkus-version (make-hash-table :test 'equal))
         (extension-id-by-name        (make-hash-table :test 'equal))
         (ret                         (make-hash-table :test 'equal)))
    (dolist (item response-streams)
      (let ((stream-id (gethash "key" item))
            (quarkus-version (gethash "quarkusCoreVersion" item))
            (java-versions (gethash "versions" (gethash "javaCompatibility" item))))
        (puthash quarkus-version (list :stream-id stream-id :java-versions java-versions) metadata-by-quarkus-version)))
    (dolist (item response-extensions)
      (let ((extension-id (gethash "id" item))
            (extension-name (gethash "name" item)))
        (puthash extension-name extension-id extension-id-by-name)))
    (puthash :metadata-by-quarkus-version metadata-by-quarkus-version ret)
    (puthash :extension-id-by-name extension-id-by-name ret)
    ret))

(defun eglot-java--project-new-quarkus ()
  "Create a new Quarkus project."
  (let* ((starter-settings            (gethash "quarkus" eglot-java-starterkits-info-by-starterkit-name))
         (starter-url                 (plist-get starter-settings :url))
         (starter-metadata            (plist-get starter-settings :metadata))
         (metadata-by-quarkus-version (gethash :metadata-by-quarkus-version starter-metadata))
         (extension-id-by-name        (gethash :extension-id-by-name starter-metadata))
         (version                     (read-string "Version: " "1.1.0-SNAPSHOT"))
         (artifact-id                 (read-string "Artifact: " "code-with-quarkus"))
         (group-id                    (read-string "Group: " "org.acme"))
         (build-tool                  (completing-read "Build Tool: "
                                                       '("MAVEN" "GRADLE_KOTLIN_DSL" "GRADLE")
                                                       nil
                                                       t
                                                       "GRADLE_KOTLIN_DSL"))
         (quarkus-version             (completing-read "Quarkus Version: "
                                                       (hash-table-keys metadata-by-quarkus-version)
                                                       nil
                                                       t))
         (java-version                (completing-read "Java Version: "
                                                       (mapcar 'int-to-string
                                                               (plist-get (gethash quarkus-version metadata-by-quarkus-version)
                                                                          :java-versions))
                                                       nil
                                                       t))
         (extension-names             (completing-read-multiple "Select extensions (comma separated, TAB to add more): "
                                                                (hash-table-keys extension-id-by-name)
                                                                nil
                                                                t))
         (extension-ids               (mapcar (lambda (item)
                                                (list "e" (gethash item extension-id-by-name)))
                                              extension-names))
         (stream-id                   (plist-get (gethash quarkus-version metadata-by-quarkus-version)
                                                 :stream-id))
         (dest-dir                    (read-directory-name "Project Parent Directory: "
                                                                (expand-file-name eglot-java-workspace-folder))))
    (unless (file-exists-p dest-dir)
      (make-directory dest-dir t))
    (let ((large-file-warning-threshold nil)
          (dest-file-name (expand-file-name (concat (format-time-string "%Y-%m-%d_%N") ".zip")
                                            dest-dir))
          (source-url     (format "%s/api/download?%s"
                                  starter-url
                                  (mapconcat (lambda (item)
                                               (format "%s=%s" (car item) (cadr item)))
                                             (append extension-ids
                                                     (list
                                                      (list "S" stream-id)
                                                      (list "a" artifact-id)
                                                      (list "j" java-version)
                                                      (list "g" group-id)
                                                      (list "b" build-tool)
                                                      (list "cn" (file-name-nondirectory starter-url))
                                                      (list "V" version)))
                                             "&"))))
      (url-copy-file source-url dest-file-name t)
      (dired (file-name-directory dest-file-name))
      (revert-buffer))))

(defun eglot-java--project-startercache-vertx (root-url)
  "Cache Vert.x project wizard settings from a URL at ROOT-URL."
  (let* ((response-metadata (eglot-java--get-initializr-json
                             (concat root-url "/metadata") "application/json"))
         (ht-defaults       (gethash "defaults" response-metadata))
         (ht-stack          (make-hash-table :test 'equal))
         (ret               (make-hash-table :test 'equal)))
    (puthash "artifactId"        (list :type :plain-item
                                       :label "Artifact ID"
                                       :default (gethash "artifactId" ht-defaults))
             ret)
    (puthash "groupId"           (list :type :plain-item
                                       :label "Group ID"
                                       :default (gethash "groupId" ht-defaults))
             ret)
    (puthash "vertxVersion"      (list :type :select-item-basic
                                       :label   "Vert.x Version"
                                       :default (gethash "vertxVersion" ht-defaults)
                                       :options (mapcar (lambda (item)
                                                          (gethash "number" item))
                                                        (gethash "versions" response-metadata)))
             ret)
    (puthash "buildTool"         (list :type :select-item-basic
                                       :label   "Build Tool"
                                       :default (gethash "buildTool" ht-defaults)
                                       :options (mapcar #'identity
                                                        (gethash "buildTools" response-metadata)))
             ret)
    (puthash "language"          (list :type :select-item-basic
                                       :label   "Language"
                                       :default (gethash "language" ht-defaults)
                                       :options (mapcar #'identity
                                                        (gethash "languages" response-metadata)))
             ret)
    (puthash "jdkVersion"        (list :type :select-item-basic
                                       :label   "JDK Version"
                                       :default (gethash "jdkVersion" ht-defaults)
                                       :options (mapcar #'identity
                                                        (gethash "jdkVersions" response-metadata)))
             ret)
    (dolist (stack-category (gethash "stack" response-metadata))
      (dolist (stack-item (gethash "items" stack-category))
        (puthash (gethash "name" stack-item) (gethash "artifactId" stack-item) ht-stack)))
    (puthash "vertxDependencies" (list :type :select-item-complex
                                       :label   "Select stacks (comma separated, TAB to add more): "
                                       :options ht-stack)
             ret)
    ret))

(defun eglot-java--project-new-vertx ()
  "Create a new Vert.x project."
  (let* ((starter-settings  (gethash "vertx" eglot-java-starterkits-info-by-starterkit-name))
         (starter-url       (plist-get starter-settings :url))
         (starter-metadata  (plist-get starter-settings :metadata))
         (params            (make-hash-table :test 'equal))
         (proj-parent-dir   (read-directory-name "Project Parent Directory: "
                                                 (expand-file-name eglot-java-workspace-folder))))
    (maphash (lambda (k item)
               (let ((item-type (plist-get item :type)))
                 (cond ((eq item-type :select-item-basic) (puthash k (completing-read (format "%s: " (plist-get item :label))
                                                                                      (plist-get item :options)
                                                                                      nil
                                                                                      t
                                                                                      (plist-get item :default))
                                                                   params))
                       ((eq item-type :select-item-complex) (puthash k
                                                                     (mapconcat (lambda (elt)
                                                                                  (gethash elt (plist-get item :options)))
                                                                                (completing-read-multiple (format "%s: " (plist-get item :label))
                                                                                                          (hash-table-keys (plist-get item :options))
                                                                                                          nil
                                                                                                          t)
                                                                                ",")
                                                                     params))
                       (t (puthash k (read-string (format "%s: " (plist-get item :label)) (plist-get item :default) )
                                   params)))))
             starter-metadata)
    (let ((dest-dir     (expand-file-name (gethash "artifactId" params) proj-parent-dir))
          (query-params (list)))
      (unless (file-exists-p dest-dir)
        (make-directory dest-dir t))
      (maphash (lambda (k v)
                 (add-to-list 'query-params (list k v)))
               params)
      (let ((large-file-warning-threshold nil)
            (dest-file-name (expand-file-name (concat (format-time-string "%Y-%m-%d_%N") ".zip")
                                              dest-dir))
            (source-url     (format "%s/starter.zip?%s"
                                    starter-url
                                    (url-build-query-string query-params))))
        (url-copy-file source-url dest-file-name t)
        (dired (file-name-directory dest-file-name))
        (revert-buffer)))))

(defun eglot-java--project-new-process-sentinel (process event)
  "Switch to the project directory when the PROCESS finishes with a success EVENT."
  (when (string-prefix-p "finished" event)
    (switch-to-buffer (process-buffer process))
    (dired eglot-java-project-new-directory)
    (revert-buffer)))

(defun eglot-java--build-run (initial-dir cmd args)
  "Start a compilation from a direction INITIAL-DIR with a given command string CMD and its arguments string ARGS."
  (let ((mvn-cmd           (concat cmd " " args))
        (default-directory initial-dir))
    (compile mvn-cmd t)))

(defun eglot-java--build-executable(cmd cmd-wrapper-name cmd-wrapper-dir)
  "Return the command to run, either the initial command itself CMD or its wrapper equivalent (CMD-WRAPPER-NAME) if found in CMD-WRAPPER-DIR."
  (let ((cmd-wrapper-abspath (executable-find (expand-file-name
                                               cmd-wrapper-name
                                               cmd-wrapper-dir))))
    (if (and cmd-wrapper-abspath
             (file-exists-p cmd-wrapper-abspath))
        cmd-wrapper-abspath
      cmd)))

(defun eglot-java-project-build-refresh ()
  "Build the project when Maven or Gradle build files are found."
  (interactive)
  (let* ((root       (project-root (project-current)))
         (build-file (if (eglot-java--project-gradle-p root)
                         (if (file-exists-p (expand-file-name eglot-java-filename-build-gradle-kotlin root))
                             (expand-file-name eglot-java-filename-build-gradle-kotlin (file-name-as-directory root))
                           (expand-file-name eglot-java-filename-build-gradle-groovy (file-name-as-directory root)))
                       (expand-file-name eglot-java-filename-build-maven (file-name-as-directory root)))))
    (when (file-exists-p build-file)
      (jsonrpc-notify
       (eglot-java--find-server)
       :java/projectConfigurationUpdate
       (list :uri (eglot--path-to-uri build-file))))
    (jsonrpc-notify
     (eglot-java--find-server)
     :java/buildWorkspace
     (vector :json-false))))

(defun eglot-java-project-build-task ()
  "Run a new build task."
  (interactive)
  (let* ((project-dir               (project-root (project-current)))
         (goal                      (read-string "Task & Parameters: " "test"))
         (project-is-gradle-project (eglot-java--project-gradle-p project-dir))
         (build-filename            (if project-is-gradle-project
                                        (if (file-exists-p (expand-file-name eglot-java-filename-build-gradle-kotlin project-dir))
                                            eglot-java-filename-build-gradle-kotlin
                                          eglot-java-filename-build-gradle-groovy)
                                      eglot-java-filename-build-maven))
         (build-filename-flag       (if project-is-gradle-project
                                        "-b"
                                      "-f"))
         (build-cmd                 (if project-is-gradle-project
                                        (eglot-java--build-executable "gradle" "gradlew" project-dir)
                                      (eglot-java--build-executable "mvn" "mvnw" project-dir))))
    (compile (format "%s %s %s %s"
                     build-cmd
                     build-filename-flag
                     (shell-quote-argument
                      (expand-file-name build-filename project-dir))
                     goal))))

(defun eglot-java--read-json-from-url (url)
  "Fetch the LSP server download metadata in JSON format.
URL is the REST endpoint serving the download metadata in JSON format."
  (with-temp-buffer
    (url-insert-file-contents url)
    (let ((json-object-type 'hash-table)
          (json-array-type  'list)
          (json-key-type    'string))
      (json-read))))

(defun eglot-java--parse-jdtls-download-metadata (metadata)
  "Extract LSP server download metadata from a JSON response.
METADATA is the JSON API response contents."
  (let ((jdtls-download-version (gethash "stable"
                                         (gethash "versions"
                                                  metadata)))
        (jdtls-download-url     (gethash "url"
                                         (gethash "stable"
                                                  (gethash "urls"
                                                           metadata)))))
    (list :download-version jdtls-download-version
          :download-url     jdtls-download-url)))

(defun eglot-java--upgrade-junit-jar (junit-jar-path)
  "Upgrade the JUnit JAR installation for a given path at JUNIT-JAR-PATH."
  (let* ((time-cur        (current-time))
         (tmp-folder-name (format "junit-standalone-console-%d-%d" (car time-cur) (cadr time-cur)))
         (install-dir     (expand-file-name (file-name-directory junit-jar-path)))
         (install-dir-tmp (expand-file-name tmp-folder-name (temporary-file-directory)))
         (install-jar-tmp (expand-file-name (file-name-nondirectory junit-jar-path) install-dir-tmp)))
    ;; create temporary directory
    (mkdir install-dir-tmp t)
    ;; install the new version of the junit jar
    (let ((install-success (condition-case nil
                               (progn
                                 (eglot-java--install-junit-jar install-jar-tmp)
                                 t)
                             (error nil))))
      (if install-success
          ;; upon success full installation rename the temporary installation folder
          (let ((install-dir-tmp-tmp (format "%s-tmp" install-dir-tmp)))
            (rename-file (directory-file-name install-dir) (directory-file-name install-dir-tmp-tmp))
            (rename-file (directory-file-name install-dir-tmp) (directory-file-name install-dir))
            (delete-directory install-dir-tmp-tmp t)
            (message "The JUnit jar was successfully upgraded."))
        (ignore-errors
          (progn
            (message "The JUnit jar upgrade failed!")
            ;; when the installation fails, delete the temporary installation directory
            (delete-directory install-dir-tmp t)))))))

(defun eglot-java--upgrade-lsp-server (install-dir)
  "Upgrade the LSP server installation in a given directory.
INSTALL-DIR is the directory where the LSP server will be upgraded."
  (let* ((buffers-managed (cl-loop for buf in (buffer-list)
                                if (with-current-buffer buf
                                     (when eglot-java-mode
                                       buf))
                                collect buf))
         (time-cur        (current-time))
         (tmp-folder-name (format "jdtls-%d-%d" (car time-cur) (cadr time-cur)))
         (install-dir-tmp (expand-file-name tmp-folder-name (temporary-file-directory))))
    ;; create temporary directory
    (mkdir install-dir-tmp t)
    ;; shutdown folders associated to eglot-java-mode
    (dolist (buf buffers-managed)
      (with-current-buffer buf
        (ignore-errors
          (eglot-java-mode -1))))
    ;; install the new version of the LSP server
    (let ((install-success (condition-case nil
                               (progn
                                 (eglot-java--install-lsp-server install-dir-tmp)
                                 t)
                             (error nil))))
      (if install-success
          ;; upon success full installation rename the temporary installation folder
          (let ((install-dir-tmp-tmp (format "%s-tmp" install-dir-tmp)))
            (rename-file (directory-file-name install-dir) (directory-file-name install-dir-tmp-tmp))
            (rename-file (directory-file-name install-dir-tmp)  (directory-file-name install-dir))
            (delete-directory install-dir-tmp-tmp t)
            (message "The JDT LSP server was successfully upgraded.")
            ;; re-associate the jdtls server to known managed buffers
            (dolist (buf buffers-managed)
              (with-current-buffer buf
                (eglot-java-mode 1))))
        (ignore-errors
          (progn
            (message "The LSP server upgrade failed!")
            ;; when the installation fails, delete the temporary installation directory
            (delete-directory install-dir-tmp t)))))))

(defun eglot-java--install-lsp-server (destination-dir)
  "Install the Eclipse JDT LSP server.
DESTINATION-DIR is the directory where the LSP server will be installed."
  (let* ((dest-dir                     (expand-file-name destination-dir))
         (download-metadata            (eglot-java--parse-jdtls-download-metadata
                                        (eglot-java--read-json-from-url eglot-java-eclipse-jdt-ls-dl-metadata-url)))
         (download-url                 (plist-get download-metadata :download-url))
         (download-version             (plist-get download-metadata :download-version))
         (dest-filename                (file-name-nondirectory download-url))
         (dest-abspath                 (expand-file-name dest-filename dest-dir))
         (dest-versionfile             (expand-file-name eglot-java-filename-version-jdtls dest-dir))
         (large-file-warning-threshold nil))
    (message "Installing Eclipse JDT LSP server, please wait...")
    (eglot-java--download-file download-url dest-abspath)

    (message "Extracting Eclipse JDT LSP archive, please wait...")
    (let ((b (find-file dest-abspath)))
      (switch-to-buffer b)
      (goto-char (point-min))
      (tar-untar-buffer)
      (kill-buffer b))
    (delete-file dest-abspath)

    (eglot-java--record-version-info download-version dest-versionfile)

    (message "Eclipse JDT LSP server installed in folder \n\"%s\"." dest-dir)))

(defun eglot-java--ensure ()
  "Install the LSP server as needed and then turn-on eglot."
  (unless (file-exists-p (expand-file-name eglot-java-server-install-dir))
    (eglot-java--install-lsp-server eglot-java-server-install-dir))
  (eglot-ensure))

(defun eglot-java--init ()
  "Initialize the library for use with the Eclipse JDT language server."
  (progn
    (progn
      (unless eglot-java-jdt-uri-handling-patch-applied
        (eglot-java--jdthandler-patch-eglot)
        (setq eglot-java-jdt-uri-handling-patch-applied t))
    (unless eglot-java-eglot-server-programs-manual-updates
      ;; there are multiple allowed syntaxes for mode associations
      (let ((existing-java-related-assocs (mapcan (lambda (item)
                                                    (if (listp (car item))
                                                        (when (member 'java-mode (car item))
                                                          (car item))
                                                      (when (eq 'java-mode (car item))
                                                        (cons (car item) nil))))
                                                  eglot-server-programs)))
        (if existing-java-related-assocs
            (let ((eff-existing-java-related-assocs (if (= 1 (length existing-java-related-assocs))
                                                        (if (assq existing-java-related-assocs eglot-server-programs)
                                                            existing-java-related-assocs
                                                          (car existing-java-related-assocs))
                                                      existing-java-related-assocs)))
              (unless (eq 'eglot-java--eclipse-contact (assq eff-existing-java-related-assocs eglot-server-programs))
                (setcdr (assoc eff-existing-java-related-assocs eglot-server-programs) 'eglot-java--eclipse-contact)))
          (add-to-list eglot-server-programs 'java-mode 'eglot-java--eclipse-contact))))
    (unless (member 'eglot-java--project-try project-find-functions)
      (add-to-list 'project-find-functions 'eglot-java--project-try t)))))

(defvar eglot-java-mode-map (make-sparse-keymap))

(defun eglot-java--jdt-uri-handler (_operation &rest args)
  "Support Eclipse jdtls `jdt://' uri scheme."
  (let* ((uri (car args))
         (cache-dir (expand-file-name ".eglot-java" (temporary-file-directory)))
         (source-file
          (expand-file-name
           (eglot-java--make-path
            cache-dir
            (save-match-data
              (when (string-match "jdt://contents/\\(.*?\\)/\\(.*\\)\.class\\?" uri)
                (format "%s.java" (replace-regexp-in-string "/" "." (match-string 2 uri) t t))))))))
    (unless (file-readable-p source-file)
      (let ((content (jsonrpc-request (eglot-java--find-server) :java/classFileContents (list :uri uri)))
            (metadata-file (format "%s.%s.metadata"
                                   (file-name-directory source-file)
                                   (file-name-base source-file))))
        (unless (file-directory-p cache-dir) (make-directory cache-dir t))
        (with-temp-file source-file (insert content))
        (with-temp-file metadata-file (insert uri))))
    source-file))

(add-to-list 'file-name-handler-alist '("\\`jdt://" . eglot-java--jdt-uri-handler))

(defvar eglot-java-jdt-uri-handling-patch-applied nil "Whether or not JDT uri handling is already patched.")

(defun eglot-java--wrap-legacy-eglot--path-to-uri (original-fn &rest args)
  "Hack until eglot is updated.
ARGS is a list with one element, a file path or potentially a URI.
If path is a jar URI, don't parse. If it is not a jar call ORIGINAL-FN."
  (let ((path (file-truename (car args))))
    (if (equal "jdt" (url-type (url-generic-parse-url path)))
        path
      (apply original-fn args))))

(defun eglot-java--wrap-legacy-eglot--uri-to-path (original-fn &rest args)
  "Hack until eglot is updated.
ARGS is a list with one element, a URI.
If URI is a jar URI, don't parse and let the `jdthandler--file-name-handler'
handle it. If it is not a jar call ORIGINAL-FN."
  (let ((uri (car args)))
    (if (and (stringp uri)
             (string= "jdt" (url-type (url-generic-parse-url uri))))
        uri
      (apply original-fn args))))

(defun eglot-java--jdthandler-patch-eglot ()
  "Patch old versions of Eglot to work with Jdthandler."
  (interactive) ;; TODO Remove when eglot is updated in melpa
  ;; See also https://debbugs.gnu.org/cgi/bugreport.cgi?bug=58790
  ;; See also https://git.savannah.gnu.org/gitweb/?p=emacs.git;a=blob;f=lisp/progmodes/eglot.el#l1558
  (unless (or (and (advice-member-p #'eglot-java--wrap-legacy-eglot--path-to-uri 'eglot--path-to-uri)
                   (advice-member-p #'eglot-java--wrap-legacy-eglot--uri-to-path 'eglot--uri-to-path))
              (<= 29 emacs-major-version))
    (advice-add 'eglot--path-to-uri :around #'eglot-java--wrap-legacy-eglot--path-to-uri)
    (advice-add 'eglot--uri-to-path :around #'eglot-java--wrap-legacy-eglot--uri-to-path)
    (message "[eglot-java--jdthandler-patch-eglot] Eglot successfully patched.")))

(defun eglot-java--find-server ()
  "Find the LSP server of type eglot-java-eclipse-jdt for the
current project. In the strange event that there are multiple,
return the first one."
  (when-let* ((project (project-current))
              (servers (gethash project eglot--servers-by-project)))
    (cl-find-if #'eglot-java-eclipse-jdt-p servers)))

;;;###autoload
(defun eglot-java-project-new ()
  "Create a new Java project."
  (interactive)
  (let* ((project-types                  (hash-table-keys eglot-java-starterkits-info-by-starterkit-name))
         (project-type                   (completing-read "Project Type: "
                                                          project-types
                                                          nil
                                                          t
                                                          "spring"))
         (project-func-name-new          (concat "eglot-java--project-new-" project-type))
         (project-func-name-startercache (concat "eglot-java--project-startercache-" project-type))
         (starterkit-info                (gethash project-type eglot-java-starterkits-info-by-starterkit-name)))
    (when-let ((starterkit-url      (plist-get starterkit-info :url))
               (starterkit-metadata (plist-get starterkit-info :metadata)))
      (when (hash-table-empty-p starterkit-metadata)
        (let ((metadata-cache (funcall (intern project-func-name-startercache) starterkit-url)))
          (maphash (lambda (k v)
                     (puthash k v starterkit-metadata))
                   metadata-cache))))
    (funcall (intern project-func-name-new))))

;;;###autoload
(defun eglot-java-upgrade-junit-jar ()
  "Upgrade the JUnit jar installation."
  (interactive)
  (let ((junit-jar-path (expand-file-name eglot-java-junit-platform-console-standalone-jar)))
    (if (file-exists-p junit-jar-path)
        (let* ((version-file-dir (file-name-directory junit-jar-path))
               (version-file     (expand-file-name eglot-java-filename-version-junit version-file-dir)))
          (if (file-exists-p version-file)
              (let* ((version-installed (eglot-java--file-read-trim version-file))
                     (download-metadata (eglot-java--find-latest-junit-metadata eglot-java-maven-repo-root-url))
                     (version-latest    (plist-get download-metadata :download-version)))
                (if (eglot-java--version< version-installed version-latest)
                    (progn
                      (message "Upgrading the JUnit jar from version %s to %s." version-installed version-latest)
                      (eglot-java--upgrade-junit-jar junit-jar-path))
                  (message "You're already running the latest JUnit jar version (%s)!" version-installed)))
            (when (yes-or-no-p "No previous JUnit jar version recorded! Do you want install the latest version?")
              (eglot-java--upgrade-junit-jar junit-jar-path))))
      (when (yes-or-no-p "No previous JUnit jar installation found! Do you want install the latest version?")
        (eglot-java--install-junit-jar junit-jar-path)))))

;;;###autoload
(defun eglot-java-upgrade-lsp-server ()
  "Upgrade the LSP server installation."
  (interactive)
  (let ((install-dir (expand-file-name eglot-java-server-install-dir)))
    (if (file-exists-p install-dir)
        (let ((lsp-server-versionfile (expand-file-name eglot-java-filename-version-jdtls install-dir)))
          (if (file-exists-p lsp-server-versionfile)
              (let* ((version-installed (eglot-java--file-read-trim lsp-server-versionfile))
                     (download-metadata (eglot-java--parse-jdtls-download-metadata
                                         (eglot-java--read-json-from-url eglot-java-eclipse-jdt-ls-dl-metadata-url)))
                     (version-latest    (plist-get download-metadata :download-version)))
                (if (eglot-java--version< version-installed version-latest)
                    (progn
                      (message "Upgrading the JDT LSP server from version %s to %s." version-installed version-latest)
                      (eglot-java--upgrade-lsp-server install-dir))
                  (message "You're already running the latest JDT LSP server version (%s)!" version-installed)))
            (when (yes-or-no-p "No previous LSP server version recorded! Do you want install the latest stable version?")
              (eglot-java--upgrade-lsp-server install-dir))))
      (when (yes-or-no-p "No previous LSP server installation found! Do you want install the latest stable version?")
        (eglot-java--install-lsp-server install-dir)))))

;;;###autoload
(define-minor-mode eglot-java-mode
    "Toggle eglot-java-mode."
  ;; The initial value.
  :init-value nil
  ;; The indicator for the mode line.
  :lighter nil
  (progn
    (eglot-java--init)
    (if eglot-java-mode
        (eglot-java--ensure)
      (ignore-errors
        (call-interactively 'eglot-shutdown)))))

(provide 'eglot-java)
;;; eglot-java.el ends here
