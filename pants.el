;;; pants.el --- A frontend for pants.

;; Copyright © 2016 Franck Cuny <franck.cuny@gmail.com>

;; Author: Franck Cuny <franck.cuny@gmail.com>
;; URL: https://github.com/fcuny/pants.el

;;; Commentary:
;;
;; This library provides an interface to `pants', a fast, scalable build system.
;; See the README for more details.

;;; Code:
(require 'compile)
(require 'python)
(require 'comint)

(defgroup pants nil
  "Major mode for editing Pants files."
  :group 'languages
  :link '(url-link "https://github.com/fcuny/pants.el"))

(defcustom pants-completion-system 'ido
  "The completion system to be used by pants."
  :group 'pants
  :type '(radio
          (const :tag "ivy" ivy)
          (const :tag "ido" ido)
          (const :tag "helm" helm)))

(defcustom pants-source-tree-root nil
  "Path to the repository."
  :group 'pants
  :type 'string)

(defcustom pants-ini "pants.ini"
  "Path to the pants.ini file to use. This variable must be set."
  :group 'pants
  :type 'string)

(defcustom pants-exec-name "pants"
  "Path to the pants executable. This variable must be set."
  :group 'pants
  :type 'string)

(defcustom pants-extra-args ""
  "Extra arguments to pass to the pants executable."
  :group 'pants
  :type 'string)

(defcustom pants-exec-args "--no-colors"
  "Arguments to the pants executable. Default is '--no-colors'"
  :group 'pants
  :type 'string)

(defcustom pants-build-file "BUILD"
  "Name of the build files. Default is 'BUILD'"
  :group 'pants
  :type 'string)

(defcustom pants-build-format-exec "buildifier"
  "Path to the executable used to format pants BUILD files."
  :group 'pants
  :type 'string)

(defcustom pants-bury-compilation-buffer nil
  "Set this variable to true to bury the compilation buffer if there's no error."
  :group 'pants
  :type 'boolean)

(defvar *pants-compilation-buffer* "*pants-compilation-buffer*")

(defvar *pants-format-errors-buffer* "*pants-format-errors*")

(defvar *pants-format-run-buffer* "*pants-format-run-buffer*")

(define-derived-mode pants-build-mode python-mode "Pants"
  "Major mode for editing Pants build files."
  :group 'pants

  (setq-local python-indent-guess-indent-offset nil)
  (setq-local python-indent-offset 4))

(defun pants--find-directory-containing-build-file (file)
  "Finds the directory containing the build file next to a give file."
  (let ((root nil)
        try)
    (while (not (or root
                    (null file)
                    (string-match locate-dominating-stop-dir-regexp file)))
      (setq try (if (stringp pants-build-file)
                   (file-exists-p (expand-file-name pants-build-file file))))
      (cond (try (setq root file))
            ((equal file (setq file (file-name-directory
                                     (directory-file-name file))))
             (setq file nil))))
    (and root (expand-file-name (file-name-as-directory root)))))

(defun pants--get-source-tree ()
  "Returns the name of the directory for the source tree, with a trailing slash."
  (file-name-as-directory pants-source-tree-root))

(defun pants--build-command ()
  "Returns the complete command to run."
  (format "%s%s %s --pants-config-files=%s%s %s"
          (pants--get-source-tree) pants-exec-name pants-extra-args (pants--get-source-tree) pants-ini pants-exec-args))

(defvar pants-scala-repl-mode-map
  (nconc (make-sparse-keymap) comint-mode-map)
  "Basic mode map for `pants-scala-repl'")

(defvar pants-scala-repl-prompt-regexp "^\\(?:scala>\\)"
  "Prompt for `pants-scala-repl'.")

(defun pants-scala-repl--initialize ()
  "Helper function to initialize pants Scala REPL"
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t))

(define-derived-mode pants-scala-repl-mode comint-mode "pants-scala-repl"
  "Major mode for `pants-scala-repl'.

\\<pants-scala-repl-mode-map>"
  nil "pants-scala-repl"
  (setq comint-prompt-regexp pants-scala-repl-prompt-regexp)
  (setq comint-prompt-read-only t)
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'paragraph-start) pants-scala-repl-prompt-regexp))

(add-hook 'pants-scala-repl-mode-hook 'pants-scala-repl--initialize)

(defun pants--scala-repl-action (target)
  "Start a Scala REPL for TARGET."
  (let* ((pants-repl-command-elements (split-string (format "%s repl %s" (pants--build-command) target)))
         (pants-executable (car pants-repl-command-elements))
         (pants-command-switches (cdr pants-repl-command-elements))
         (default-directory (pants--get-source-tree))
         (cmd-name (concat "scala:" target))
         (buffer (comint-check-proc cmd-name)))
    (pop-to-buffer-same-window
     (if (or buffer
             (not (derived-mode-p 'pants-scala-repl-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer (concat "*" cmd-name "*")))
       (current-buffer)))
    (unless buffer
      (apply 'make-comint-in-buffer cmd-name buffer pants-executable nil pants-command-switches)
      (pants-scala-repl-mode))))

(defun pants--python-repl-action (target)
  "Starts a Python REPL."
  (let ((pants-repl-command (format "%s repl %s" (pants--build-command) target))
        (default-directory (pants--get-source-tree)))
    (set (make-local-variable 'python-shell-exec-path) '(pants--get-source-tree))
    (set (make-local-variable 'python-shell-interpreter) (pants--get-source-tree))
    (set (make-local-variable 'python-shell-interpreter-args) pants-repl-command)
    (set (make-local-variable 'python-shell-prompt-detect-failure-warning) nil)
    (run-python pants-repl-command t)
    (python-shell-switch-to-shell)))

(defun pants--build-action (target)
  "Executes the `binary' command"
  (let ((compile-command (format "%s binary %s" (pants--build-command) target)))
    (pants--compile compile-command)))

(defun pants--test-action (target)
  "Executes the `test' command"
  (let ((compile-command (format "%s test %s" (pants--build-command) target)))
    (pants--compile compile-command)))

(defun pants--fmt-action (target)
  "Executes the `fmt' command"
  (let ((compile-command (format "%s fmt.isort %s" (pants--build-command) target)))
    (pants--compile compile-command)))

(defun pants--compilation-setup ()
  "Sets the local configuration for the compile buffer"
  (set (make-local-variable 'compilation-scroll-output) t)
  (set (make-local-variable 'compilation-disable-input) t)
  (set (make-local-variable 'compilation-exit-message-function)
       (lambda (status code msg)
         (when (and
                (equal status 'exit)
                (zerop code)
                (and pants-bury-compilation-buffer t))
           (bury-buffer (get-buffer *pants-compilation-buffer*))
           (replace-buffer-in-windows (get-buffer *pants-compilation-buffer*)))
         (cons msg code))))

(defun pants--compile (command)
  "Executes the compilation"
  (let ((compilation-buffer-name-function (lambda (arg) *pants-compilation-buffer*)))
    (compilation-start command 'pants-mode)))

(defun pants--complete-read (prompt choices action)
  "Generates a list of existing targets"
  (let ((default-directory (pants--get-source-tree))
        res)
    (setq res
          (cond
           ((eq pants-completion-system 'ivy)
            (if (fboundp 'ivy-read)
                (ivy-read prompt choices
                          :action (prog1 action
                                    (setq action nil)))
              (user-error "Please install ivy from https://github.com/abo-abo/swiper")))
           ((eq pants-completion-system 'helm)
            (if (fboundp 'helm)
                (helm :sources
                      (helm-make-source "Pants" 'helm-source-sync
                                        :candidates choices
                                        :action (prog1 action
                                                  (setq action nil))
                                        :buffer "*helm pants targets*"
                                        :prompt prompt))
              (user-error "Please install helm from https://github.com/emacs-helm/helm")))
           ((eq pants-completion-system 'ido)
            (ido-completing-read prompt choices))))
    (if action
        (funcall action res)
      res)))

(defun pants--get-build-file-for-current-buffer ()
  "Finds the nearest build file for the current buffer"
  (let ((build-file (pants--find-directory-containing-build-file (file-name-directory (buffer-file-name)))))
    (if build-file
        build-file
      (user-error "Could not find %s" pants-build-file))))

(defun pants--read-targets-from-cache (cache-targets current-build-file)
  "Returns a list of targets from the cache."
  (let (targets)
    (with-temp-buffer
      (let (target)
        (insert-file-contents-literally cache-targets)
        (goto-char (point-min))
        (while (re-search-forward "^\\(.+\\)$" nil t)
          (setq target (match-string 1))
          (push target targets))))
    (push (format "%s::"(string-remove-prefix (pants--get-source-tree) current-build-file))
          targets)
    targets))

(defun pants--populate-cache-targets (cache-targets current-build-file)
  "Populates the cache with the targets."
  (let ((output (get-buffer-create "*pants-list-targets*"))
        (errors (format "%s%s/%s" (temporary-file-directory) "pants-targets" "errors"))
        (default-directory (pants--get-source-tree))
        (build-buffer (current-buffer)))
    (make-directory (format "%s%s" (temporary-file-directory) "pants-targets") :parents)
    (let ((status (call-process (format "%s%s" (pants--get-source-tree) pants-exec-name) nil `(,output ,errors) nil "list" current-build-file)))
      (when (zerop status)
          (with-current-buffer output
            (write-file cache-targets)))
      (switch-to-buffer build-buffer)
      (when output (kill-buffer output))
      (when errors (delete-file errors))
      (pants--read-targets-from-cache cache-targets current-build-file))))

(defun pants--get-path-cached-targets (current-build-file)
  "Returns the path to the cached targets."
  (format "%s%s/%s" (temporary-file-directory) "pants-targets" (secure-hash 'md5 current-build-file)))

(defun pants--get-targets ()
  "Returns the targets for the current file."
  (let ((current-build-file (pants--get-build-file-for-current-buffer)))
    (let ((cache-targets (pants--get-path-cached-targets current-build-file)))
      (when (file-newer-than-file-p (format "%sBUILD" (pants--get-build-file-for-current-buffer)) cache-targets)
        (pants--populate-cache-targets cache-targets current-build-file))
      (pants--read-targets-from-cache cache-targets current-build-file))))

(defun pants--replace-build-buffer (buffer new-content)
  (with-current-buffer buffer
    (erase-buffer)
    (save-excursion
      (insert-file-contents-literally new-content)
      (save-buffer))))

(define-compilation-mode pants-mode "pants"
  (set (make-local-variable 'compilation-process-setup-function)
       'pants--compilation-setup))

;;;###autoload
(defun pants-find-build-file ()
  "Finds the build file and if it exists, open it."
  (interactive)
  (find-file (concat (pants--get-build-file-for-current-buffer) pants-build-file)))

;;;###autoload
(defun pants-run-binary ()
  "Builds a binary from a target."
  (interactive)
  (pants--complete-read "Build a binary for: " (pants--get-targets) 'pants--build-action))

;;;###autoload
(defun pants-run-python-repl ()
  "Run a Python REPL from a target."
  (interactive)
  (pants--complete-read "Run a REPL for: " (pants--get-targets) 'pants--python-repl-action))

;;;###autoload
(defun pants-run-scala-repl ()
  "Run a Scala REPL from a target."
  (interactive)
  (pants--complete-read "Run REPL for: " (pants--get-targets) 'pants--scala-repl-action))

;;;###autoload
(defun pants-run-test ()
  "Runs the tests from a target."
  (interactive)
  (pants--complete-read "Run tests for: " (pants--get-targets) 'pants--test-action))

;;;###autoload
(defun pants-run-fmt ()
  "Runs fmt on a target file to sort the import files (Python only)."
  (interactive)
  (pants--complete-read "Run fmt for: " (pants--get-targets) 'pants--fmt-action))

;;;###autoload
(defun pants-build-fmt ()
  "Format the current buffer using buildifier."
  (interactive)
  (let ((input (make-temp-file "pants-format-input"))
        (output (get-buffer-create *pants-format-run-buffer*))
        (errors (make-temp-file "pants-format-errors")))
    (unwind-protect
        (progn
          (write-region nil nil input nil 'silent-write)
          (with-current-buffer output (erase-buffer))
          (let ((status (call-process pants-build-format-exec nil `(,output ,errors) nil "-mode=fix" input)))
            (if (zerop status)
                (pants--replace-build-buffer (current-buffer) input)
              (let ((errors-buffer (get-buffer-create *pants-format-errors-buffer*)))
                (with-current-buffer errors-buffer
                  (setq buffer-read-only nil)
                  (erase-buffer)
                  (insert-file-contents-literally errors)
                  (let ((file-name (file-name-nondirectory (buffer-file-name)))
                        (regexp (rx-to-string `(sequence line-start (group ,input) ":"))))
                    (while (search-forward-regexp regexp nil t)
                      (replace-match file-name t t nil 1)))
                  (compilation-mode))
                (display-buffer errors-buffer))))))
    (when input (delete-file input))
    (when output (kill-buffer output))
    (when errors (delete-file errors))))

(provide 'pants)
