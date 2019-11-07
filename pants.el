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
(require 'subr-x)
(eval-when-compile
  (require 'cl))
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
  "Path to the pants.ini file to use."
  :group 'pants
  :type 'string)

(defcustom pants-exec-name "pants"
  "Path to the pants executable.

Must be set."
  :group 'pants
  :type 'string)

(defcustom pants-extra-args ""
  "Extra arguments to pass to the pants executable.

Must be set."
  :group 'pants
  :type 'string)

(defcustom pants-exec-args "--no-colors"
  "Arguments to the pants executable.

Default is '--no-colors'."
  :group 'pants
  :type 'string)

(defcustom pants-build-file "BUILD"
  "Name of the build files.  Default is 'BUILD'."
  :group 'pants
  :type 'string)

(define-derived-mode pants-build-mode python-mode "Pants"
  "Major mode for editing Pants build files."
  :group 'pants

  (setq-local python-indent-guess-indent-offset nil)
  (setq-local python-indent-offset 4))

(defun pants--find-directory-containing-build-file (&optional file)
  "Find the directory containing the build file next to a given FILE."
  (let ((file (or file (file-name-directory
                        (buffer-file-name))))
        (root nil)
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
  "Get the name of the directory for the source tree, with a trailing slash."
  (file-name-as-directory pants-source-tree-root))

(defun pants--build-command ()
  "Return the complete pants command to run."
  (format "%s%s %s --pants-config-files=%s%s %s"
          (pants--get-source-tree)
          pants-exec-name
          pants-extra-args
          (pants--get-source-tree)
          pants-ini
          pants-exec-args))


(defvar pants-scala-repl-mode-map
  (nconc (make-sparse-keymap) comint-mode-map)
  "Basic mode map for `pants-scala-repl'.")

(defvar pants-scala-repl-prompt-regexp "^\\(?:scala>\\)"
  "Prompt for `pants-scala-repl'.")

(defun pants-scala-repl--initialize ()
  "Helper function to initialize pants Scala REPL."
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

;; FIXME: Add pants-python-repl-options so users can specify -q etc.
(defun pants--python-repl-action (target)
  "Start a Python REPL for TARGET."
  (let ((pants-repl-command (format "%s repl %s" (pants--build-command) target))
        (default-directory (pants--get-source-tree)))
    (set (make-local-variable 'python-shell-exec-path) '(pants--get-source-tree))
    (set (make-local-variable 'python-shell-interpreter) (pants--get-source-tree))
    (set (make-local-variable 'python-shell-interpreter-args) pants-repl-command)
    (set (make-local-variable 'python-shell-prompt-detect-failure-warning) nil)
    (run-python pants-repl-command t)
    (python-shell-switch-to-shell)))

(defun pants--compile (command)
  "Compile using COMMAND."
  (set (make-local-variable 'compile-command) command)
  (compile command))

(defun pants--build-action (target)
  "Execute the `binary' command on TARGET."
  (let ((compile-command (format "%s binary %s" (pants--build-command) target)))
    (pants--compile compile-command)))

(defun pants--test-action (target)
  "Execute the `test' command on TARGET."
  (let ((compile-command (format "%s test %s" (pants--build-command) target)))
    (pants--compile compile-command)))

(defun pants--run-action (target)
  "Execute the `run' command on TARGET."
  (let ((compile-command (format "%s run %s" (pants--build-command) target)))
    (pants--compile compile-command)))

(defun pants--fmt-action (target)
  "Execute the `fmt' command on TARGET."
  (let ((compile-command (format "%s fmt %s" (pants--build-command) target)))
    (pants--compile compile-command)))

(defun pants--grep-action (target pattern)
  "Execute `grep' with PATTERN on output of pants filedeps on TARGET."
  (let* ((filedeps-command (format "%s filedeps %s" (pants--build-command) target))
         (files (replace-regexp-in-string "\n\r?" " "
                (replace-regexp-in-string (pants--get-source-tree) ""
                (replace-regexp-in-string "^WARNING.*?\n" ""
                (shell-command-to-string filedeps-command))))))
    (grep (concat "grep  -nH " pattern " " files))))

(defun pants--complete-read (prompt choices action)
  "Select from a list of target CHOICES using PROMPT selectied using ACTION."
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
            (string-join (completing-read-multiple prompt choices) " "))))
    (if action
        (funcall action res)
      res)))


(defun pants--get-build-file-for-current-buffer ()
  "Find the nearest build file for the current buffer."
  (let ((build-file (pants--find-directory-containing-build-file
                     (file-name-directory
                      (buffer-file-name)))))
    (if build-file
        build-file
      (user-error "Could not find %s" pants-build-file))))

(defun pants--read-targets-from-cache (cache-targets current-build-file)
  "Return a list of targets for CURRENT-BUILD-FILE from the CACHE-TARGETS."
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
  "Populate the CACHE-TARGETS with the targets from CURRENT-BUILD-FILE."
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
  "Return the path to the cached targets for CURRENT-BUILD-FILE."
  (format "%s%s/%s" (temporary-file-directory) "pants-targets" (secure-hash 'md5 current-build-file)))

(defun pants--get-targets ()
  "Return the targets for the current file or BUILD-FILE."
  (let ((current-build-file (pants--get-build-file-for-current-buffer)))
    (let ((cache-targets (pants--get-path-cached-targets current-build-file)))
      (when (file-newer-than-file-p (format "%sBUILD" (pants--get-build-file-for-current-buffer)) cache-targets)
        (pants--populate-cache-targets cache-targets current-build-file))
      (pants--read-targets-from-cache cache-targets current-build-file))))

;;;###autoload
(defun pants-find-build-file ()
  "Find the build file and if it exists, open it."
  (interactive)
  (find-file (concat (pants--get-build-file-for-current-buffer) pants-build-file)))

;;;###autoload
(defun pants-run-binary ()
  "Builds a binary from a target."
  (interactive)
  (pants--complete-read "Build a binary for: " (pants--get-targets) 'pants--build-action))

;;;###autoload
(defun pants-run-target ()
  "Builds binary for a target."
  (interactive)
  (pants--complete-read "Run: " (pants--get-targets) 'pants--run-action))

;;;###autoload
(defun pants-run-python-repl ()
  "Run Python REPL from a target."
  (interactive)
  (pants--complete-read "Run a REPL for: " (pants--get-targets) 'pants--python-repl-action))

;;;###autoload
(defun pants-run-scala-repl ()
  "Run Scala REPL from a target."
  (interactive)
  (pants--complete-read "Run REPL for: " (pants--get-targets) 'pants--scala-repl-action))

;;;###autoload
(defun pants-run-test ()
  "Run test for a target."
  (interactive)
  (pants--complete-read "Run tests for: " (pants--get-targets) 'pants--test-action))

;;;###autoload
(defun pants-run-fmt ()
  "Run fmt on a target."
  (interactive)
  (pants--complete-read "Run fmt for: " (pants--get-targets) 'pants--fmt-action))

;;;###autoload
(defun pants-grep (pattern)
  "Run `find-grep' with PATTERN on file list derived from PANTS."
  (interactive
   (list (read-string "Grep for what pattern: " "''")))
  (pants--complete-read "Run grep for: " (pants--get-src+test-targets)
                        (lambda (tgt) (pants--grep-action tgt pattern))))

(provide 'pants)
;;; pants.el ends here
