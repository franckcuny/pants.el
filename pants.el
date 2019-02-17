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

(defcustom pants-bury-compilation-buffer nil
  "Set this variable to true to bury the compilation buffer if there's no error."
  :group 'pants
  :type 'boolean)

(defvar *pants-compilation-buffer* "*pants-compilation-buffer*")

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
  (format "%s%s %s --config-override=%s%s %s"
          (pants--get-source-tree) pants-exec-name pants-extra-args (pants--get-source-tree) pants-ini pants-exec-args))

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

(defun pants--get-targets ()
  "Get the targets for the current file."
  (let ((build-command (format "%s list %s:"
                               (pants--build-command)
                               (pants--get-build-file-for-current-buffer)))
        (default-directory (pants--get-source-tree))
        targets)
    (with-temp-buffer
      (let (target)
        (insert
         (shell-command-to-string build-command))
        (goto-char (point-min))
        (while (re-search-forward "^\\(.+\\)$" nil t)
          (setq target (match-string 1))
          (push target targets))))
    (push (format "%s::" (string-remove-prefix
                          (pants--get-source-tree)
                          (pants--get-build-file-for-current-buffer)))
          targets)
    targets))

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
  "Runs a REPL from a target."
  (interactive)
  (pants--complete-read "Run a REPL for: " (pants--get-targets) 'pants--python-repl-action))

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

(provide 'pants)
