;; init-custom.el --- Define customizations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Customization.
;;

;;; Code:

(defgroup danny nil
  "Danny Emacs customization."
  :group 'convenience
  :link '(url-link :tag "Homepage" "https://github.com/PlanetsFuture/danny-emacs"))

(defcustom danny-logo (expand-file-name
                         (if (display-graphic-p) "logo.png" "banner.txt")
                         user-emacs-directory)
  "Set Danny logo. nil means official logo."
  :group 'danny
  :type 'string)

(defcustom danny-full-name user-full-name
  "Set user full name."
  :group 'danny
  :type 'string)

(defcustom danny-mail-address user-mail-address
  "Set user email address."
  :group 'danny
  :type 'string)

(defcustom danny-org-directory (expand-file-name "~/org/")
  "Set org directory."
  :group 'danny
  :type 'string)

(defcustom danny-proxy "127.0.0.1:1087"
  "Set HTTP/HTTPS proxy."
  :group 'danny
  :type 'string)

(defcustom danny-socks-proxy "127.0.0.1:1086"
  "Set SOCKS proxy."
  :group 'danny
  :type 'string)

(defcustom danny-server t
  "Enable `server-mode' or not."
  :group 'danny
  :type 'boolean)

(defcustom danny-icon (or (display-graphic-p) (daemonp))
  "Display icons or not."
  :group 'danny
  :type 'boolean)

;; Emacs Lisp Package Archive (ELPA)
;; @see https://github.com/melpa/melpa and https://elpa.emacs-china.org/.
(defcustom danny-package-archives-alist
  '((melpa    . (("gnu"    . "http://elpa.gnu.org/packages/")
                 ("nongnu" . "http://elpa.nongnu.org/nongnu/")
                 ("melpa"  . "http://melpa.org/packages/")))


    )
  "A list of the package archives."
  :group 'danny
  :type '(alist :key-type (symbol :tag "Archive group name")
                :value-type (alist :key-type (string :tag "Archive name")
                                   :value-type (string :tag "URL or directory name"))))

(defcustom danny-package-archives 'melpa
  "Set package archives from which to fetch."
  :group 'danny
  :set (lambda (symbol value)
         (set symbol value)
         (setq package-archives
               (or (alist-get value danny-package-archives-alist)
                   (error "Unknown package archives: `%s'" value))))
  :type `(choice ,@(mapcar
                    (lambda (item)
                      (let ((name (car item)))
                        (list 'const
                              :tag (capitalize (symbol-name name))
                              name)))
                    danny-package-archives-alist)))

(defcustom danny-theme-alist
  '((default . doom-one)
    (pro     . doom-monokai-pro)
    (dark    . doom-dark+)
    (light   . doom-one-light)
    (warm    . doom-solarized-light)
    (cold    . doom-city-lights)
    (day     . doom-tomorrow-day)
    (night   . doom-tomorrow-night))
  "List of themes mapped to internal themes."
  :group 'danny
  :type '(alist :key-type (symbol :tag "Theme")
                :value-type (symbol :tag "Internal theme")))

(defcustom danny-auto-themes '(("8:00"  . doom-one-light)
				                 ("19:00" . doom-one))
  "List of themes mapped to the time they should be loaded.

The keywords `:sunrise' and `:sunset' can be used for the time
if `calendar-latitude' and `calendar-longitude' are set.
For example:
  \\='((:sunrise . doom-one-light)
    (:sunset  . doom-one))"
  :group 'danny
  :type '(alist :key-type (string :tag "Time")
                :value-type (symbol :tag "Theme")))

(defcustom danny-system-themes '((light . doom-one-light)
				                   (dark  . doom-one))
  "List of themes related the system appearance.

It's only available on macOS currently."
  :group 'danny
  :type '(alist :key-type (symbol :tag "Appearance")
                :value-type (symbol :tag "Theme")))

(defcustom danny-theme 'default
  "The color theme."
  :group 'danny
  :type `(choice (const :tag "Auto" auto)
                 (const :tag "Random" random)
                 ,(if (boundp 'ns-system-appearance)
                      '(const :tag "System" system)
                    "")
                 ,@(mapcar
                    (lambda (item)
                      (let ((name (car item)))
                        (list 'const
                              :tag (capitalize (symbol-name name))
                              name)))
                    danny-theme-alist)
                 symbol))

(defcustom danny-completion-style 'childframe
  "Completion display style."
  :group 'danny
  :type '(choice (const :tag "Minibuffer" minibuffer)
                 (const :tag "Child Frame" childframe)))

(defcustom danny-dashboard (not (daemonp))
  "Display dashboard at startup or not.
If Non-nil, use dashboard, otherwise will restore previous session."
  :group 'danny
  :type 'boolean)

(defcustom danny-restore-frame-geometry t
  "Restore the frame's geometry at startup.
If Non-nil, save and restore the frame's geometry."
  :group 'danny
  :type 'boolean)

(defcustom danny-lsp 'lsp-mode
  "Set language server.

`lsp-mode': See https://github.com/emacs-lsp/lsp-mode.
`eglot': See https://github.com/joaotavora/eglot.
tags: Use tags file instead of language server. See https://github.com/universal-ctags/citre.
nil means disabled."
  :group 'danny
  :type '(choice (const :tag "LSP Mode" lsp-mode)
                 (const :tag "Eglot" eglot)
                 (const :tag "Disable" nil)))

(defcustom danny-tree-sitter nil
  "Enable `tree-sitter' or not."
  :group 'danny
  :type 'boolean)

(defcustom danny-lsp-format-on-save-ignore-modes
  '(c-mode c++-mode python-mode markdown-mode)
  "The modes that don't auto format and organize imports while saving the buffers.
`prog-mode' means ignoring all derived modes.
"
  :group 'danny
  :type '(repeat (symbol :tag "Major-Mode")))

(defcustom danny-tree-sitter t
  "Enable `tree-sitter' or not."
  :group 'danny
  :type 'boolean)

(defcustom danny-chinese-calendar nil
  "Enable Chinese calendar or not."
  :group 'danny
  :type 'boolean)

(defcustom danny-player nil
  "Enable players or not."
  :group 'danny
  :type 'boolean)

(defcustom danny-prettify-symbols-alist
  '(("lambda" . ?λ)
    ("<-"     . ?←)
    ("->"     . ?→)
    ("->>"    . ?↠)
    ("=>"     . ?⇒)
    ("map"    . ?↦)
    ("/="     . ?≠)
    ("!="     . ?≠)
    ("=="     . ?≡)
    ("<="     . ?≤)
    (">="     . ?≥)
    ("=<<"    . (?= (Br . Bl) ?≪))
    (">>="    . (?≫ (Br . Bl) ?=))
    ("<=<"    . ?↢)
    (">=>"    . ?↣)
    ("&&"     . ?∧)
    ("||"     . ?∨)
    ("not"    . ?¬))
  "A list of symbol prettifications.
Nil to use font supports ligatures."
  :group 'danny
  :type '(alist :key-type string :value-type (choice character sexp)))

(defcustom danny-prettify-org-symbols-alist
  '(("[ ]" . ?☐)
    ("[X]" . ?☑)
    ("[-]" . ?⛝)

    ("#+ARCHIVE:"     . ?📦)
    ("#+AUTHOR:"      . ?👤)
    ("#+CREATOR:"     . ?💁)
    ("#+DATE:"        . ?📆)
    ("#+DESCRIPTION:" . ?⸙)
    ("#+EMAIL:"       . ?📧)
    ("#+OPTIONS:"     . ?⛭)
    ("#+SETUPFILE:"   . ?⛮)
    ("#+TAGS:"        . ?🏷)
    ("#+TITLE:"       . ?📓)

    ("#+BEGIN_SRC"   . ?✎)
    ("#+END_SRC"     . ?□)
    ("#+BEGIN_QUOTE" . ?»)
    ("#+END_QUOTE"   . ?«)
    ("#+HEADERS"     . ?☰)
    ("#+RESULTS:"    . ?💻))
  "A list of symbol prettifications for `org-mode'."
  :group 'danny
  :type '(alist :key-type string :value-type (choice character sexp)))

;; Load `custom-file'
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(provide 'init-custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-custom.el ends here
