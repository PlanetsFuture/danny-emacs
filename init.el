;;; init.el --- A Fancy and Fast Emacs Configuration.	-*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary:
;;
;; Danny Emacs
;;


;;; Code:

(when (version< emacs-version "26.1")
  (error "This requires Emacs 26.1 and above!"))

;; Speed up startup
(setq auto-mode-case-fold nil)

(unless (or (daemonp) noninteractive init-file-debug)
  (let ((old-file-name-handler-alist file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist
                                           old-file-name-handler-alist)))))))

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Recover GC values after startup."
            (setq gc-cons-threshold 800000)))

;; Suppress flashing at startup
(setq-default inhibit-redisplay t
              inhibit-message t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (redisplay)))

;; Load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("site-lisp" "lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'.

Don't put large files in `site-lisp' directory, e.g. EAF.
Otherwise the startup will be very slow. "
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

;; Packages
(require 'init-package)

;; Preferences
(require 'init-basic)
(require 'init-hydra)

(require 'init-ui)
(require 'init-edit)
(require 'init-ivy)
(require 'init-company)
(require 'init-yasnippet)

(require 'init-bookmark)
(require 'init-calendar)
(require 'init-dashboard)
(require 'init-dired)
(require 'init-highlight)
(require 'init-ibuffer)
(require 'init-kill-ring)
(require 'init-persp)
(require 'init-window)
(require 'init-treemacs)

(require 'init-eshell)
(require 'init-shell)

(require 'init-markdown)
(require 'init-org)
(require 'init-reader)

(require 'init-dict)
(require 'init-docker)
(require 'init-player)
(require 'init-utils)

;; Programming
(require 'init-vcs)
(require 'init-flycheck)
(require 'init-projectile)
(require 'init-lsp)
(require 'init-ctags)

(require 'init-prog)
(require 'init-elisp)
(require 'init-c)
(require 'init-go)
(require 'init-rust)
(require 'init-python)
(require 'init-dart)
(require 'init-web)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
