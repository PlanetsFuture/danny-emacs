;; init-funcs.el --- Define functions.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Define functions.
;;

;;; Code:
(require 'cl-lib)

(require 'init-const)
(require 'init-custom)

;; Suppress warnings
(defvar circadian-themes)
(defvar socks-noproxy)
(defvar socks-server)

(declare-function async-inject-variables 'async)
(declare-function chart-bar-quickie 'chart)
(declare-function flycheck-buffer 'flycheck)
(declare-function flymake-start 'flymake)
(declare-function upgrade-packages 'init-package)



;; Font
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

;; Dos2Unix/Unix2Dos
(defun dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

(defun delete-carrage-returns ()
  "Delete `^M' characters in the buffer.
Same as `replace-string C-q C-m RET RET'."
  (interactive)
  (save-excursion
    (goto-char 0)
    (while (search-forward "\r" nil :noerror)
      (replace-match ""))))

;; File and buffer
(defun revert-this-buffer ()
  "Revert the current buffer."
  (interactive)
  (unless (minibuffer-window-active-p (selected-window))
    (revert-buffer t t)
    (message "Reverted this buffer")))

(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun rename-this-file (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

(defun browse-this-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

(defun copy-file-name ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (if filename
        (progn
          (kill-new filename)
          (message "Copied '%s'" filename))
      (warn "Current buffer is not attached to a file!"))))

;; Browse URL
(defun danny-webkit-browse-url (url &optional pop-buffer new-session)
  "Browse URL with xwidget-webkit' and switch or pop to the buffer.

POP-BUFFER specifies whether to pop to the buffer.
NEW-SESSION specifies whether to create a new xwidget-webkit session."
  (interactive (progn
                 (require 'browse-url)
                 (browse-url-interactive-arg "xwidget-webkit URL: ")))
  (or (featurep 'xwidget-internal)
      (user-error "Your Emacs was not compiled with xwidgets support"))
  (xwidget-webkit-browse-url url new-session)
  (let ((buf (xwidget-buffer (and (fboundp 'xwidget-webkit-current-session)
                                  (xwidget-webkit-current-session)))))
    (when (buffer-live-p buf)
      (and (eq buf (current-buffer)) (quit-window))
      (if pop-buffer
          (pop-to-buffer buf)
        (switch-to-buffer buf)))))

;; Mode line
(defun mode-line-height ()
  "Get the height of the mode-line."
  (- (elt (window-pixel-edges) 3)
     (elt (window-inside-pixel-edges) 3)
     (if (bound-and-true-p window-divider-mode)
         window-divider-default-bottom-width
       0)))

;; Reload configurations
(defun reload-init-file ()
  "Reload Emacs configurations."
  (interactive)
  (load user-init-file))
(defalias 'danny-reload-init-file #'reload-init-file)

;; Browse the homepage
(defun browse-homepage ()
  "Browse the Github page of Danny Emacs."
  (interactive)
  (browse-url danny-homepage))

;; Open custom file
(defun open-custom-file()
  "Open or create `custom-file'."
  (interactive)
  (unless (file-exists-p custom-file)
    (if (file-exists-p danny-custom-example-file)
        (copy-file danny-custom-example-file custom-file)
      (user-error "The file `%s' doesn't exist" danny-custom-example-file)))
  (find-file custom-file)
  (find-file-other-window danny-custom-post-file))

;; Misc
(defun create-scratch-buffer ()
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defun save-buffer-as-utf8 (coding-system)
  "Revert a buffer with `CODING-SYSTEM' and save as UTF-8."
  (interactive "zCoding system for visited file (default nil):")
  (revert-buffer-with-coding-system coding-system)
  (set-buffer-file-coding-system 'utf-8)
  (save-buffer))

(defun save-buffer-gbk-as-utf8 ()
  "Revert a buffer with GBK and save as UTF-8."
  (interactive)
  (save-buffer-as-utf8 'gbk))

(defun recompile-elpa ()
  "Recompile packages in elpa directory. Useful if you switch Emacs versions."
  (interactive)
  (if (fboundp 'async-byte-recompile-directory)
      (async-byte-recompile-directory package-user-dir)
    (byte-recompile-directory package-user-dir 0 t)))

(defun recompile-site-lisp ()
  "Recompile packages in site-lisp directory."
  (interactive)
  (let ((dir (locate-user-emacs-file "site-lisp")))
    (if (fboundp 'async-byte-recompile-directory)
        (async-byte-recompile-directory dir)
      (byte-recompile-directory dir 0 t))))

(defun icon-displayable-p ()
  "Return non-nil if icons are displayable."
  (and danny-icon
       (or (display-graphic-p) (daemonp))
       (or (featurep 'all-the-icons)
           (require 'all-the-icons nil t))))

(defun danny-set-variable (variable value &optional no-save)
  "Set the VARIABLE to VALUE, and return VALUE.

  Save to `custom-file' if NO-SAVE is nil."
  (customize-set-variable variable value)
  (when (and (not no-save)
             (file-writable-p custom-file))
    (with-temp-buffer
      (insert-file-contents custom-file)
      (goto-char (point-min))
      (while (re-search-forward
              (format "^[\t ]*[;]*[\t ]*(setq %s .*)" variable)
                               nil t)
  (replace-match (format "(setq %s '%s)" variable value) nil nil))
      (write-region nil nil custom-file)
      (message "Saved %s (%s) to %s" variable value custom-file))))

(define-minor-mode danny-read-mode
  "Minor Mode for better reading experience."
  :init-value nil
  :group danny
  (if danny-read-mode
      (progn
        (and (fboundp 'olivetti-mode) (olivetti-mode 1))
        (and (fboundp 'mixed-pitch-mode) (mixed-pitch-mode 1))
        (text-scale-set +2))
    (progn
      (and (fboundp 'olivetti-mode) (olivetti-mode -1))
      (and (fboundp 'mixed-pitch-mode) (mixed-pitch-mode -1))
      (text-scale-set 0))))

;; Pakcage repository (ELPA)
(defun set-package-archives (archives &optional refresh async no-save)
  "Set the package archives (ELPA).

REFRESH is non-nil, will refresh archive contents.
ASYNC specifies whether to perform the downloads in the background.
Save to `custom-file' if NO-SAVE is nil."
  (interactive
   (list
    (intern
     (ivy-read "Select package archives: "
               (mapcar #'car danny-package-archives-alist)
               :preselect (symbol-name danny-package-archives)))))
  ;; Set option
  (danny-set-variable 'danny-package-archives archives no-save)

  ;; Refresh if need
  (and refresh (package-refresh-contents async))

  (message "Set package archives to `%s'" archives))
(defalias 'danny-set-package-archives #'set-package-archives)

;; Refer to https://emacs-china.org/t/elpa/11192
(defun danny-test-package-archives (&optional no-chart)
  "Test connection speed of all package archives and display on chart.

Not displaying the chart if NO-CHART is non-nil.
Return the fastest package archive."
  (interactive)

  (let* ((durations (mapcar
                     (lambda (pair)
                       (let ((url (concat (cdr (nth 2 (cdr pair)))
                                          "archive-contents"))
                             (start (current-time)))
                         (message "Fetching %s..." url)
                         (ignore-errors
                           (url-copy-file url null-device t))
                         (float-time (time-subtract (current-time) start))))
                     danny-package-archives-alist))
         (fastest (car (nth (cl-position (apply #'min durations) durations)
                            danny-package-archives-alist))))

    ;; Display on chart
    (when (and (not no-chart)
               (require 'chart nil t)
               (require 'url nil t))
      (chart-bar-quickie
       'horizontal
       "Speed test for the ELPA mirrors"
       (mapcar (lambda (p) (symbol-name (car p))) danny-package-archives-alist)
       "ELPA"
       (mapcar (lambda (d) (* 1e3 d)) durations) "ms"))

    (message "`%s' is the fastest package archive" fastest)

    ;; Return the fastest
    fastest))

;; WORKAROUND: fix blank screen issue on macOS.
(defun fix-fullscreen-cocoa ()
  "Address blank screen issue with child-frame in fullscreen.
This issue has been addressed in 28."
  (and sys/mac-cocoa-p
       (not emacs/>=28p)
       (bound-and-true-p ns-use-native-fullscreen)
       (setq ns-use-native-fullscreen nil)))



;; Update
(defun update-config ()
  "Update Danny Emacs configurations to the latest version."
  (interactive)
  (let ((dir (expand-file-name user-emacs-directory)))
    (unless (file-exists-p dir)
      (user-error "\"%s\" doesn't exist" dir))

    (message "Updating configurations...")
    (cd dir)
    (shell-command "git pull")
    (message "Updating configurations...done")))
(defalias 'danny-update-config #'update-config)

(defvar danny--updating-packages nil)
(defun update-packages (&optional force sync)
  "Refresh package contents and update all packages.

If FORCE is non-nil, the updating process will be restarted by force.
If SYNC is non-nil, the updating process is synchronous."
  (interactive)

  (if (process-live-p danny--updating-packages)
      (when force
        (kill-process danny--updating-packages)
        (setq danny--updating-packages nil))
    (setq danny--updating-packages nil))

  (message "Updating packages...")
  (unless danny--updating-packages
    (if (and (not sync)
             (require 'async nil t))
        (setq danny--updating-packages
              (async-start
               `(lambda ()
                  ,(async-inject-variables "\\`\\(load-path\\)\\'")
                  (require 'init-funcs)
                  (require 'init-package)
                  (upgrade-packages)
                  (with-current-buffer auto-package-update-buffer-name
                    (buffer-string)))
               (lambda (result)
                 (setq danny--updating-packages nil)
                 (message "%s" result)
                 (message "Updating packages...done"))))
      (upgrade-packages)
      (message "Updating packages...done"))))
(defalias 'danny-update-packages #'update-packages)

(defvar danny--updating nil)
(defun update-config-and-packages(&optional force sync)
  "Update confgiurations and packages.

If FORCE is non-nil, the updating process will be restarted by force.
If SYNC is non-nil, the updating process is synchronous."
  (interactive "P")

  (if (process-live-p danny--updating)
      (when force
        (kill-process danny--updating)
        (setq danny--updating nil))
    (setq danny--updating nil))

  (message "Updating Danny Emacs...")
  (unless danny--updating
    (if (and (not sync)
             (require 'async nil t))
        (setq danny--updating
              (async-start
               `(lambda ()
                  ,(async-inject-variables "\\`\\(load-path\\)\\'")
                  (require 'init-funcs)
                  (require 'init-package)
                  (update-config)
                  (update-packages nil t)
                  (with-current-buffer auto-package-update-buffer-name
                    (buffer-string)))
               (lambda (result)
                 (setq danny--updating nil)
                 (message "%s" result)
                 (message "Updating Danny Emacs...done"))))
      (update-config)
      (update-packages nil t)
      (message "Updating Danny Emacs...done"))))
(defalias 'danny-update #'update-config-and-packages)

(defun update-all()
  "Update dotfiles, org files, configurations and packages to the latest."
  (interactive)
  (update-org)
  (update-dotfiles)
  (update-config-and-packages))
(defalias 'danny-update-all #'update-all)

(defun update-dotfiles ()
  "Update the dotfiles to the latest version."
  (interactive)
  (let ((dir (or (getenv "DOTFILES")
                 (expand-file-name "~/.dotfiles/"))))
    (if (file-exists-p dir)
        (progn
          (message "Updating dotfiles...")
          (cd dir)
          (shell-command "git pull")
          (message "Updating dotfiles...done"))
      (message "\"%s\" doesn't exist" dir))))
(defalias 'danny-update-dotfiles #'update-dotfiles)

(defun update-org ()
  "Update Org files to the latest version."
  (interactive)
  (let ((dir (expand-file-name "~/org/")))
    (if (file-exists-p dir)
        (progn
          (message "Updating org files...")
          (cd dir)
          (shell-command "git pull")
          (message "Updating org files...done"))
      (message "\"%s\" doesn't exist" dir))))
(defalias 'danny-update-org #'update-org)


;; Fonts
(defun danny-install-fonts ()
  "Install necessary fonts."
  (interactive)

  (let* ((font-dest (cond
                     ;; Default Linux install directories
                     ((member system-type '(gnu gnu/linux gnu/kfreebsd))
                      (concat (or (getenv "XDG_DATA_HOME")
                                  (concat (getenv "HOME") "/.local/share"))
                              "/fonts/"))
                     ;; Default MacOS install directory
                     ((eq system-type 'darwin)
                      (concat (getenv "HOME") "/Library/Fonts/"))))
         (known-dest? (stringp font-dest))
         (font-dest (or font-dest (read-directory-name "Font installation directory: " "~/"))))

    (unless (file-directory-p font-dest) (mkdir font-dest t))

    ;; Download `all-the-fonts'
    (when (bound-and-true-p all-the-icons-font-names)
      (let ((url-format "https://raw.githubusercontent.com/domtronn/all-the-icons.el/master/fonts/%s"))
        (mapc (lambda (font)
                (url-copy-file (format url-format font) (expand-file-name font font-dest) t))
              all-the-icons-font-names)))

    ;; Download `Symbola'
    ;; See https://dn-works.com/wp-content/uploads/2020/UFAS-Fonts/Symbola.zip
    (let* ((url (concat danny-homepage "/files/6135060/symbola.zip"))
           (temp-file (make-temp-file "symbola-" nil ".zip"))
           (dir (concat (file-name-directory temp-file) "/symbola/"))
           (unzip-script (cond ((executable-find "unzip")
                                (format "mkdir -p %s && unzip -qq %s -d %s"
                                        dir temp-file dir))
                               ((executable-find "powershell")
                                (format "powershell -noprofile -noninteractive \
  -nologo -ex bypass Expand-Archive -path '%s' -dest '%s'" temp-file dir))
                               (t (user-error "Unable to extract '%s' to '%s'! \
  Please check unzip, powershell or extract manually." temp-file dir)))))
      (url-copy-file url temp-file t)
      (when (file-exists-p temp-file)
        (shell-command-to-string unzip-script)
        (let* ((font-name "Symbola.otf")
               (temp-font (expand-file-name font-name dir)))
          (if (file-exists-p temp-font)
              (copy-file temp-font (expand-file-name font-name font-dest) t)
            (message "Failed to download `Symbola'!")))))

    (when known-dest?
      (message "Fonts downloaded, updating font cache... <fc-cache -f -v> ")
      (shell-command-to-string (format "fc-cache -f -v")))

    (message "Successfully %s `all-the-icons' and `Symbola' fonts to `%s'!"
             (if known-dest? "installed" "downloaded")
             font-dest)))




;; UI
(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defun run-after-load-theme-hook (&rest _)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))
(advice-add #'load-theme :after #'run-after-load-theme-hook)

(defun childframe-workable-p ()
  "Whether childframe is workable."
  (or (not (or noninteractive
               emacs-basic-display
               (not (display-graphic-p))))
      (daemonp)))

(defun childframe-completion-workable-p ()
  "Whether childframe completion is workable."
  (and (eq danny-completion-style 'childframe)
       (childframe-workable-p)))

(defun danny--theme-name (theme)
  "Return internal THEME name."
  (or (alist-get theme danny-theme-alist) theme 'doom-one))

(defun danny-compatible-theme-p (theme)
  "Check if the THEME is compatible. THEME is a symbol."
  (or (memq theme '(auto random system))
      (string-prefix-p "doom" (symbol-name (danny--theme-name theme)))))

(defun danny-dark-theme-p ()
  "Check if the current theme is a dark theme."
  (eq (frame-parameter nil 'background-mode) 'dark))

(defun danny-theme-enable-p (theme)
  "The THEME is enabled or not."
  (and theme
       (not (memq danny-theme '(auto random system)))
       (memq (danny--theme-name theme) custom-enabled-themes)))

(defun danny--load-theme (theme)
  "Disable others and enable new one."
  (when theme
    (message "Loading theme `%s'..." theme)
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t)
    (message "Loading theme `%s'...done" theme)))

(defun danny--load-system-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (danny--load-theme (danny--theme-name
                        (pcase appearance
                          ('light (cdr (assoc 'light danny-system-themes)))
                          ('dark (cdr (assoc 'dark danny-system-themes)))
                          (_ danny-theme)))))

(defun danny-load-random-theme ()
  "Load the random theme."
  (interactive)
  (let* ((themes (mapcar #'cdr danny-theme-alist))
         (theme (nth (random (length themes)) themes)))
    (if (eq theme danny-theme)
        (danny-load-random-theme)
      (danny--load-theme theme))))

(defun danny-load-theme (theme &optional no-save)
  "Load color THEME. Save to `custom-file' if NO-SAVE is nil."
  (interactive
   (list
    (intern
     (ivy-read "Load theme: "
               `(auto
                 random
                 ,(if (bound-and-true-p ns-system-appearance) 'system "")
                 ,@(mapcar #'car danny-theme-alist))
               :preselect (symbol-name danny-theme)))))
  ;; Set option
  (danny-set-variable 'danny-theme theme no-save)

  ;; Disable system theme
  (remove-hook 'ns-system-appearance-change-functions #'danny--load-system-theme)

  (pcase danny-theme
    ('auto
     ;; Time-switching themes
     (use-package circadian
       :functions circadian-setup
       :custom (circadian-themes danny-auto-themes)
       :init (circadian-setup)))
    ('system
     ;; System-appearance themes
     (if (bound-and-true-p ns-system-appearance)
         (progn
           (danny--load-system-theme ns-system-appearance)
           (add-hook 'ns-system-appearance-change-functions #'danny--load-system-theme))
       (progn
         (message "The `system' theme is unavailable on this platform. Using `default' theme...")
         (danny--load-theme (danny--theme-name 'default)))))
    ('random (danny-load-random-theme))
    (_ (danny--load-theme (danny--theme-name theme)))))



;; Frame
(defvar danny-frame--geometry nil)
(defun danny-frame--save-geometry ()
  "Save current frame's geometry."
  (setq-local danny-frame--geometry
              `((left . ,(frame-parameter nil 'left))
                (top . ,(frame-parameter nil 'top))
                (width . ,(frame-parameter nil 'width))
                (height . ,(frame-parameter nil 'height))
                (fullscreen))))

(defun danny-frame--fullscreen-p ()
  "Returns Non-nil if the frame is fullscreen."
  (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth)))

(defun danny-frame-maximize ()
  "Maximize the frame."
  (interactive)
  (danny-frame--save-geometry)
  (unless (eq (frame-parameter nil 'fullscreen) 'maximized)
    (set-frame-parameter nil 'fullscreen 'maximized)))

(defun danny-frame-restore ()
  "Restore the frame's size and position."
  (interactive)
  (modify-frame-parameters nil danny-frame--geometry))

(defun danny-frame-left-half ()
  "Put the frame to the left-half."
  (interactive)
  (unless (danny-frame--fullscreen-p)
    (danny-frame--save-geometry)
    (let* ((attr (frame-monitor-workarea))
           (width (- (/ (nth 2 attr) 2) 20))
           (height (- (nth 3 attr) 30))
           (left (nth 0 attr))
           (top (nth 1 attr)))
      (set-frame-parameter nil 'fullscreen nil)
      (set-frame-position nil left top)
      (set-frame-size nil width height t))))

(defun danny-frame-right-half ()
  "Put the frame to the right-half."
  (interactive)
  (unless (danny-frame--fullscreen-p)
    (danny-frame--save-geometry)
    (let* ((attr (frame-monitor-workarea))
           (width (- (/ (nth 2 attr) 2) 20))
           (height (- (nth 3 attr) 30))
           (left (+ (nth 0 attr) width 20))
           (top (nth 1 attr)))
      (set-frame-parameter nil 'fullscreen nil)
      (set-frame-position nil left top)
      (set-frame-size nil width height t))))

(defun danny-frame-top-half ()
  "Put the frame to the top-half."
  (interactive)
  (unless (danny-frame--fullscreen-p)
    (danny-frame--save-geometry)
    (let* ((attr (frame-monitor-workarea))
           (width (- (nth 2 attr) 20))
           (height (- (/ (nth 3 attr) 2) 30))
           (left (nth 0 attr))
           (top (nth 1 attr)))
      (set-frame-parameter nil 'fullscreen nil)
      (set-frame-position nil left top)
      (set-frame-size nil width height t))))

(defun danny-frame-bottom-half ()
  "Put the frame to the bottom-half."
  (interactive)
  (unless (danny-frame--fullscreen-p)
    (danny-frame--save-geometry)
    (let* ((attr (frame-monitor-workarea))
           (width (- (nth 2 attr) 20))
           (height (- (/ (nth 3 attr) 2) 30))
           (left (nth 0 attr))
           (top (+ (nth 1 attr) height 30)))
      (set-frame-parameter nil 'fullscreen nil)
      (set-frame-position nil left top)
      (set-frame-size nil width height t))))



;; Network Proxy
(defun proxy-http-show ()
  "Show HTTP/HTTPS proxy."
  (interactive)
  (if url-proxy-services
      (message "Current HTTP proxy is `%s'" danny-proxy)
    (message "No HTTP proxy")))

(defun proxy-http-enable ()
  "Enable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services
        `(("http" . ,danny-proxy)
          ("https" . ,danny-proxy)
          ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
  (proxy-http-show))

(defun proxy-http-disable ()
  "Disable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services nil)
  (proxy-http-show))

(defun proxy-http-toggle ()
  "Toggle HTTP/HTTPS proxy."
  (interactive)
  (if (bound-and-true-p url-proxy-services)
      (proxy-http-disable)
    (proxy-http-enable)))

(defun proxy-socks-show ()
  "Show SOCKS proxy."
  (interactive)
  (if (bound-and-true-p socks-noproxy)
      (message "Current SOCKS%d proxy is %s:%s"
               (cadddr socks-server) (cadr socks-server) (caddr socks-server))
    (message "No SOCKS proxy")))

(defun proxy-socks-enable ()
  "Enable SOCKS proxy."
  (interactive)
  (require 'socks)
  (setq url-gateway-method 'socks
        socks-noproxy '("localhost"))
  (let* ((proxy (split-string danny-socks-proxy ":"))
         (host (car proxy))
         (port (string-to-number (cadr proxy))))
    (setq socks-server `("Default server" ,host ,port 5)))
  (setenv "all_proxy" (concat "socks5://" danny-socks-proxy))
  (proxy-socks-show))

(defun proxy-socks-disable ()
  "Disable SOCKS proxy."
  (interactive)
  (setq url-gateway-method 'native
        socks-noproxy nil
        socks-server nil)
  (setenv "all_proxy" "")
  (proxy-socks-show))

(defun proxy-socks-toggle ()
  "Toggle SOCKS proxy."
  (interactive)
  (if (bound-and-true-p socks-noproxy)
      (proxy-socks-disable)
    (proxy-socks-enable)))

(provide 'init-funcs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-funcs.el ends here
