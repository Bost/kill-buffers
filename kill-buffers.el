;;; kill-buffers.el --- Jump to the Last Edit Location, regardless of the file.

;; Copyright (C) 2020 Rostislav Svoboda

;; Authors: Rostislav Svoboda <Rostislav.Svoboda@gmail.com>
;; Version: 0.1
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
  (message "%d buffer(s) killed." (my=kill-buffers--forcefully regexp internal-too)))

(defun my=kill-buffers--magit ()
  "Kill all Magit buffers."
  (interactive)
  ;; (my=kill-buffers--forcefully "\*magit: .*\\|\*magit-.*")
  (save-excursion
    (let ((count 0))
      (dolist (buffer (buffer-list))
        (set-buffer buffer)
        (when (find major-mode '(magit-status-mode
                                 magit-log-mode
                                 magit-diff-mode
                                 magit-revision-mode
                                 magit-stash-mode
                                 magit-process-mode))
          (setq count (1+ count))
          (kill-buffer buffer)))
      (message "Killed %i Magit buffer(s)." count))))


(defun my=kill-buffers--unwanted ()
  "Kill all unwanted buffers and delete other windows so that only one remains
displayed."
  (interactive)
  (save-excursion
    (let ((count 0))
      (dolist (buffer (buffer-list))
        (set-buffer buffer)
        ;; find out buffer's major mode: (message "%s" major-mode)
        ;; prefer explicit listing of unwanted buffers
        (when (find major-mode
                    '(magit-status-mode
                      magit-log-mode
                      magit-diff-mode
                      magit-revision-mode
                      magit-stash-mode
                      magit-process-mode
                      dired-mode
                      ))
          (kill-buffer buffer)
          (setq count (1+ count))))
      (dolist (buf '(
                     "*Backtrace*"
                     "*Buffer List*"
                     "*Compile-Log*"
                     "*Compile-Log*"
                     "*Ediff Registry*"
                     "*Help*"
                     "*Minibuf-1*"
                     "*Racket Logger*"
                     "*Warnings*"
                     "*buffer-selection*"
                     "*cider-doc*"
                     "*cider-error*"
                     "*cider-inspect*"
                     "*cider-ns-browser*"
                     "*cider-refresh-log*"
                     "*edn*"
                     "*eslint*"
                     "*eslint::stderr*"
                     "*info*"
                     "*package-build-checkout*"
                     "*quelpa-build-checkout*"
                     "*spacemacs*"
                     "*vc*"
                     ))
        (when (get-buffer buf) ; check if buffer exists
          (kill-buffer buf)
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
  (if (and (fboundp 'cider-repls) ;; is cider loaded?
           (member (current-buffer) (cider-repls)))
      (progn
        (message "Calling (cider-quit)")
        (cider-quit))
    ;; (kill-buffer-and-window) works even for the emacs-server
    ;; (if server-buffer-clients
    ;;     (server-edit)
    ;;   (kill-this-buffer))
    (kill-buffer)
    ;; (kill-buffer-and-window)
    ))

(provide 'kill-buffers)

;;; kill-buffers.el ends here
