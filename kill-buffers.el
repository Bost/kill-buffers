;;; kill-buffers.el --- Jump to the Last Edit Location, regardless of the file.

;; Copyright (C) 2020 - 2023 Rostislav Svoboda

;; Authors: Rostislav Svoboda <Rostislav.Svoboda@gmail.com>
;; Version: N/A
;; Package-Requires:
;; Keywords:
;; URL: https://github.com/Bost/kill-buffers

;;; Installation:
;; In the `dotspacemacs/user-config', add there:
;;   (use-package kill-buffers)
;; then, in the `dotspacemacs-additional-packages', add there:
;;   (kill-buffers :location
;;                 (recipe :fetcher github :repo "Bost/kill-buffers"))
;; or:
;;   $ git clone https://github.com/Bost/kill-buffers
;; and then
;;   (kill-buffers :location "<path/to/the/cloned-repo>")

;; (require 'goto-chg)

(defun my=kill-buffers--forcefully (regexp &optional internal-too)
  "Kill - WITHOUT ASKING - buffers whose name matches the specified REGEXP.
See the `kill-matching-buffers` for grateful killing. The optional 2nd argument
indicates whether to kill internal buffers too.

Returns the count of killed buffers."
  (let* ((buffers (remove-if-not
                   (lambda (buffer)
                     (let ((name (buffer-name buffer)))
                       (and name (not (string-equal name ""))
                            (or internal-too (/= (aref name 0) ?\s))
                            (string-match regexp name))))
                   (buffer-list))))
    (mapc 'kill-buffer buffers)
    (length buffers)))

;; TODO see also packages easy-kill, easy-kill-extras, kill-or-bury-alive, kmb,
;; omni-kill, viking-mode
(defun my=kill-buffers--force (regexp &optional internal-too)
  "Kill - WITHOUT ASKING - buffers whose name matches the specified REGEXP.
See the `kill-matching-buffers` for grateful killing. The optional 2nd argument
indicates whether to kill internal buffers too.

Returns a message with the count of killed buffers."
  (interactive "sKill buffers matching this regular expression: \nP")
  (message "%d buffer(s) killed."
           (my=kill-buffers--forcefully regexp internal-too)))

(defcustom my=magit-unwanted-modes
  '(magit-status-mode
    magit-log-mode
    magit-diff-mode
    magit-revision-mode
    magit-stash-mode
    magit-process-mode)
  "A buffer with matching major-mode is killed by my=kill-buffers--magit."
  ;; :package-version '(kill-buffers . "1.0.0")
  :type 'list)

(defun my=kill-buffers--magit ()
  "Kill all Magit buffers."
  (interactive)
  ;; (my=kill-buffers--forcefully "\*magit: .*\\|\*magit-.*")
  (save-excursion
    (let ((count 0))
      (dolist (buffer (buffer-list))
        (set-buffer buffer)
        (when (find major-mode my=magit-unwanted-modes)
          (setq count (1+ count))
          (kill-buffer buffer)))
      (message "Killed %i Magit buffer(s)." count))))

(defcustom my=all-unwanted--modes
  (append my=magit-unwanted-modes
          '(
            dired-mode
            Man-mode
            woman-mode
            ))
  "A buffer with matching major-mode is killed by my=kill-buffers--unwanted."
  ;; :package-version '(kill-buffers . "1.0.0")
  :type 'list)

(defcustom my=unwanted-buffers
  ;; prefer explicit listing of unwanted buffers
  '(
    "*Async-native-compile-log*"
    "*Backtrace*"
    "*Buffer List*"
    "*Compile-Log*"
    "*Ediff Registry*"
    "*Geiser Documentation*"
    "*Geiser Messages*"
    "*Help*"
    "*Minibuf-1*"
    "*Native-compile-Log*"
    "*Org-Babel Error Output*"
    "*Racket Logger </>*"
    "*Racket Logger*"
    "*Warnings*"
    "*WoMan-Log*"
    ;; "*bash-ls*" ; may want to restart lsp bash when killed
    "*buffer-selection*"
    "*cider-doc*"
    "*cider-error*"
    "*cider-inspect*"
    "*cider-ns-browser*"
    "*cider-ns-refresh-log*"
    "*cider-refresh-log*"
    "*company-documentation*"
    "*edn*"
    "*eslint*"
    "*eslint::stderr*"
    "*info*"
    "*lsp-log*"
    "*package-build-checkout*"
    "*quelpa-build-checkout*"
    ;; "*spacemacs*" ; needed for ~SPC f e U~ M-x configuration-layer/update-packages
    "*vc*"
    )
  "A buffer with a name from this list is killed by my=kill-buffers--unwanted."
  ;; :package-version '(kill-buffers . "1.0.0")
  :type 'list)

(defun my=kill-buffers--unwanted ()
  "Kill all unwanted buffers and delete other windows so that only one remains
displayed."
  (interactive)
  (save-excursion
    (let ((count 0))
      (dolist (buffer (buffer-list))
        (set-buffer buffer)
        ;; find out buffer's major mode: (message "%s" major-mode)
        (when (find major-mode my=all-unwanted--modes)
          (kill-buffer buffer)
          (setq count (1+ count))))
      (dolist (buffer my=unwanted-buffers)
        (when (get-buffer buffer) ; check if buffer exists
          (kill-buffer buffer)
          (setq count (1+ count))))
      ;; (delete-other-windows)
      (message "Buffer(s) killed: %i" count))))

(defun my=kill-buffers--dired ()
  "Kill all dired buffers."
  (interactive)
  (save-excursion
    (let ((count 0))
      (dolist (buffer (buffer-list))
        (set-buffer buffer)
        (when (equal major-mode 'dired-mode)
          (setq count (1+ count))
          (kill-buffer buffer)))
      (message "Killed %i dired buffer(s)." count))))

(defun my=close-buffer ()
  "Clojure repl buffer needs to invoke its own kill function"
  (interactive)
  (cond
   ((and (fboundp 'cider-repls) ;; is cider loaded?
         (member (current-buffer) (cider-repls)))
    (progn
      (message "Calling (cider-quit)")
      (cider-quit)))

   ((equal major-mode 'term-mode)
    (term-send-eof))

   (t
    (kill-buffer)))

  ;; There's no need to place the call of (balance-windows-area) outside of this
  ;; package to minimizing package dependencies, via e.g.
  ;;   (advice-add #'my=close-buffer :after balance-windows-area).
  ;; `balance-windows-area' is from windows.el, which is part of GNU Emacs.
  (balance-windows-area))

(provide 'kill-buffers)

;;; kill-buffers.el ends here
