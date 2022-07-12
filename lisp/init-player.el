;; init-player.el --- Initialize player configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Player configurations.
;;

;;; Code:

(require 'init-custom)
(require 'init-funcs)

(when danny-player
  ;; Music player
  (use-package bongo
    :bind ("C-<f9>" . bongo)
    :config
    (with-eval-after-load 'dired
      (with-no-warnings
        (defun bongo-add-dired-files ()
          "Add marked files to the Bongo library."
          (interactive)
          (bongo-buffer)
          (let (file (files nil))
            (dired-map-over-marks
             (setq file (dired-get-filename)
                   files (append files (list file)))
             nil t)
            (with-bongo-library-buffer
             (mapc 'bongo-insert-file files)))
          (bongo-switch-buffers))
        (bind-key "b" #'bongo-add-dired-files dired-mode-map))))

  ;; Music Player Daemon
  ;; Built-in client for mpd
  (use-package mpc
    :ensure nil
    :bind ("s-<f9>" . mpc)
    :init
    (defun restart-mpd ()
      (interactive)
      (call-process "pkill" nil nil nil "mpd")
      (call-process "mpd")))

  ;; Simple client for mpd
  (use-package simple-mpc
    :if (executable-find "mpc")
    :commands simple-mpc-call-mpc-strings
    :functions (simple-mpc-current simple-mpc-start-timer)
    :bind ("M-<f9>" . simple-mpc)
    :init
    (setq simple-mpc-playlist-format "[[%artist% - ]%title%]|[%file%]")

    ;; Display current song in mode-line
    (defvar simple-mpc-current nil)
    (add-to-list 'global-mode-string '("" (:eval simple-mpc-current)))

    (defun simple-mpc-current ()
      "Get current song information."
      (setq simple-mpc-current
            (when-let* ((strs (simple-mpc-call-mpc-strings nil))
                        (title (nth 0 strs))
                        (info (nth 1 strs))
                        (info-strs (split-string info))
                        (state (nth 0 info-strs))
                        (time (nth 2 info-strs)))
              (propertize (format "%s%s [%s] "
                                  (and (icon-displayable-p)
                                       (pcase state
                                         ("[playing]" " ")
                                         ("[paused]" " ")
                                         (_ "")))
                                  title time)
                          'face 'font-lock-comment-face)))
      (force-mode-line-update))

    (defvar simple-mpc--timer nil)
    (defun simple-mpc-start-timer ()
      "Start simple-mpc timer to refresh current song."
      (setq simple-mpc--timer (run-with-timer 0 1 #'simple-mpc-current)))
    (defun simple-mpc-stop-timer ()
      "Stop simple-mpc timer."
      (when (timerp simple-mpc--timer)
        (cancel-timer simple-mpc--timer)))
    (simple-mpc-start-timer)))

(provide 'init-player)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-player.el ends here
