;; init-dict.el --- Initialize dictionaries.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Multiple dictionaries.
;;

;;; Code:

(require 'init-const)

;; A multi dictionaries interface
(when emacs/>=27p
  (use-package fanyi
    :bind (("C-c d f" . fanyi-dwim)
           ("C-c d d" . fanyi-dwim2)
           ("C-c d h" . fanyi-from-history))
    :custom (fanyi-providers '(fanyi-haici-provider
                               fanyi-longman-provider)))

  (use-package go-translate
    :bind (("C-c d g" . gts-do-translate))
    :init (setq gts-translate-list '(("en" "zh") ("zh" "en")))))

;; Youdao Dictionary
(use-package youdao-dictionary
  :bind (("C-c y"   . my-youdao-dictionary-search-at-point)
         ("C-c d Y" . my-youdao-dictionary-search-at-point)
         ("C-c d y" . youdao-dictionary-search-async)
         :map youdao-dictionary-mode-map
         ("h"       . my-youdao-dictionary-help)
         ("?"       . my-youdao-dictionary-help))
  :init
  (setq url-automatic-caching t)
  (setq youdao-dictionary-use-chinese-word-segmentation t) ; 中文分词
  :config
  (with-no-warnings
    (with-eval-after-load 'hydra
      (defhydra youdao-dictionary-hydra (:color blue)
        ("p" youdao-dictionary-play-voice-of-current-word "play voice of current word")
        ("y" youdao-dictionary-play-voice-at-point "play voice at point")
        ("q" quit-window "quit")
        ("C-g" nil nil)
        ("h" nil nil)
        ("?" nil nil))
      (defun my-youdao-dictionary-help ()
        "Show help in `hydra'."
        (interactive)
        (let ((hydra-hint-display-type 'message))
          (youdao-dictionary-hydra/body))))

    (defun my-youdao-dictionary-search-at-point ()
      "Search word at point and display result with `posframe', `pos-tip' or buffer."
      (interactive)
      (if (posframe-workable-p)
          (youdao-dictionary-search-at-point-posframe)
        (youdao-dictionary-search-at-point)))

    (defun my-youdao-dictionary--posframe-tip (string)
      "Show STRING using `posframe-show'."
      (unless (posframe-workable-p)
        (error "Posframe not workable"))

      (if-let ((word (youdao-dictionary--region-or-word)))
          (progn
            (with-current-buffer (get-buffer-create youdao-dictionary-buffer-name)
              (let ((inhibit-read-only t))
                (erase-buffer)
                (youdao-dictionary-mode)
                (insert (propertize "\n" 'face '(:height 0.5)))
                (insert string)
                (insert (propertize "\n" 'face '(:height 0.5)))
                (set (make-local-variable 'youdao-dictionary-current-buffer-word) word)))
            (posframe-show
             youdao-dictionary-buffer-name
             :position (point)
             :left-fringe 16
             :right-fringe 16
             :max-width (/ (frame-width) 2)
             :max-height (/ (frame-height) 2)
             :background-color (face-background 'tooltip nil t)
             :internal-border-color (face-background 'posframe-border nil t)
             :internal-border-width 1)
            (unwind-protect
                (push (read-event) unread-command-events)
              (progn
                (posframe-hide youdao-dictionary-buffer-name)
                (other-frame 0)))
            (message "Nothing to look up"))))
    (advice-add #'youdao-dictionary--posframe-tip
                :override #'my-youdao-dictionary--posframe-tip)))

;; OSX dictionary
(when sys/macp
  (use-package osx-dictionary
    :bind (("C-c d i" . osx-dictionary-search-input)
           ("C-c d x" . osx-dictionary-search-pointer))))

(provide 'init-dict)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dict.el ends here
