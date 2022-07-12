;; init-docker.el --- Initialize docker configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Docker configurations.
;;

;;; Code:

;; Docker
(use-package docker
  :defines docker-image-run-arguments
  :bind ("C-c D" . docker)
  :init (setq docker-image-run-arguments '("-i" "-t" "--rm")
              docker-container-shell-file-name "/bin/bash"))

(use-package docker-tramp)
(use-package dockerfile-mode)

(provide 'init-docker)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-docker.el ends here
