;; pants-mode.el
;;
;; This goes together with pants.el, providing python-mode derived
;; syntax highlighting for pants BUILD files.

;;;###autoload
(add-to-list 'auto-mode-alist '("\\BUILD\\'" . pants-mode))

(define-derived-mode pants-mode python-mode "BUILD")

;; These two are from http://www.pantsbuild.org/build_dictionary.html
(defvar pants-mode-targets-list
  (list "alias"
        "android_binary"
        "android_dependency"
        "android_library"
        "android_resources"
        "annotation_processor"
        "benchmark"
        "contrib_plugin"
        "cpp_binary"
        "cpp_library"
        "credentials"
        "go_binary"
        "go_library"
        "go_remote_libraries"
        "go_remote_library"
        "go_thrift_library"
        "jar_library"
        "java_agent"
        "java_antlr_library"
        "java_library"
        "java_protobuf_library"
        "java_ragel_library"
        "java_tests"
        "java_thrift_library"
        "java_wire_library"
        "javac_plugin"
        "jax_ws_library"
        "jaxb_library"
        "junit_tests"
        "jvm_app"
        "jvm_binary"
        "jvm_prep_command"
        "managed_jar_dependencies"
        "netrc_credentials"
        "node_bundle"
        "node_module"
        "node_preinstalled_module"
        "node_remote_module"
        "node_test"
        "page"
        "pants_plugin"
        "prep_command"
        "python_antlr_library"
        "python_binary"
        "python_library"
        "python_requirement_library"
        "python_tests"
        "python_thrift_library"
        "remote_sources"
        "resources"
        "scala_js_binary"
        "scala_js_library"
        "scala_library"
        "scalac_plugin"
        "target"
        "unpacked_jars"))

(defvar pants-mode-targets-pattern
  (regexp-opt pants-mode-targets-list))

(defvar pants-mode-symbols-list
  (list "ConfluencePublish"
        "DirectoryReMapper"
        "Duplicate"
        "Skip"
        "Wiki"
        "artifact"
        "buildfile_path"
        "bundle"
        "contrib_setup_py"
        "developer"
        "exclude"
        "get_buildroot"
        "github"
        "globs"
        "intransitive"
        "jar"
        "jar_rules"
        "license"
        "managed_jar_libraries"
        "netrc"
        "ossrh"
        "pants_library"
        "pants_requirement"
        "pants_setup_py"
        "pants_version"
        "provided"
        "public"
        "python_artifact"
        "python_requirement"
        "python_requirements"
        "repository"
        "rglobs"
        "scala_artifact"
        "scala_jar"
        "scm"
        "scoped"
        "setup_py"
        "shading_exclude"
        "shading_exclude_package"
        "shading_keep"
        "shading_keep_package"
        "shading_relocate"
        "shading_relocate_package"
        "shading_zap"
        "shading_zap_package"
        "testing"
        "wiki_artifact"
        "zglobs"))

(defvar pants-mode-symbols-pattern
  (regexp-opt pants-mode-symbols-list))

(defvar pants-mode-keywords-list
  (list "name"
        "source"
        "sources"
        "dependencies"
        "requirements"
        "description"))

(defvar pants-mode-keywords-pattern
  (regexp-opt pants-mode-keywords-list))

(font-lock-add-keywords 'pants-mode
                        `((,pants-mode-keywords-pattern . font-lock-keyword-face)
                          (,pants-mode-targets-pattern . font-lock-function-name-face)
                          (,pants-mode-symbols-pattern . font-lock-function-name-face)))

(provide 'pants-mode)
