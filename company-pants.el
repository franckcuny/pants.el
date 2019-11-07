;; company-mode support for pants-mode
;;
;; Leverages the new keyword list(s) in pants-mode to provide trivial
;; autocompletion via company for pants BUILD files.
;;
;; Also uses company-files to autocomplete pants target paths, and
;; provides some super rudimentary parsing for BUILD files in order to
;; provide automatic completion of pants targets.

(require 'company)
(require 'company-files)
(require 'pants-mode)

(defun company-pants--current-buffer-targets ()
  (let ((targets nil)
        (break nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "name=['\"]\\([^'\"]+\\)" nil t)
        (add-to-list 'targets (match-string-no-properties 1))))
    targets))

(defun company-pants--buildfile-targets (file)
  (with-temp-buffer
    (insert-file-contents file)
    (company-pants--current-buffer-targets)))

(defun company-pants--maybe-complete-build (path)
  (if (file-directory-p path)
      (let ((build-file-path (concat path "BUILD")))
        (if (file-exists-p build-file-path)
            (company-pants--buildfile-targets build-file-path)))))

(defun company-pants--grab-existing-name ()
  (save-excursion
    (re-search-backward "[\s'\"]")
    (re-search-forward "\\(\\([^\s'\":\n\r/]+[/:]\\)+\\)" nil t)
    (replace-regexp-in-string ":" "/" (match-string-no-properties 1))))

(defun company-pants--directory-files (suffix arg)
  (mapcar (lambda (s) (replace-regexp-in-string "/$" "" s))
          (company-files--directory-files suffix arg)))

(defun company-pants-mode-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))

  (case command
    (interactive
     (company-begin-backend 'company-pants-mode-backend))

    (prefix
     (and (eq major-mode 'pants-mode)
          (company-grab-symbol)))

    (candidates
     (remove-if-not
      (lambda (c) (string-prefix-p arg c))
      (let ((suffix (concat (pants--get-source-tree)
                            (company-pants--grab-existing-name))))
        (concatenate 'list
                     (company-pants--current-buffer-targets)
                     (company-pants--maybe-complete-build suffix)
                     (company-pants--maybe-complete-build (concat suffix arg))
                     (company-pants--directory-files suffix arg)
                     pants-mode-targets-list
                     pants-mode-symbols-list
                     pants-mode-keywords-list))))))

;;;###autoload
(add-to-list 'company-backends 'company-pants-mode-backend)

(provide 'company-pants)
