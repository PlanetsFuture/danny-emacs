;; init-dart.el --- Initialize Dart configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Dart configurations.
;;

;;; Code:

;; Dart
(use-package dart-mode
  :defines (projectile-project-root-files-bottom-up)
  :config
  (with-eval-after-load 'projectile
    (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
    (add-to-list 'projectile-project-root-files-bottom-up "BUILD")))

(provide 'init-dart)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dart.el ends here
