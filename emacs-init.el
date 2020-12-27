(setq gc-cons-threshold (* 50 1000 1000)) ;set threshold high to reduce garbage collection during some of startup

;(require 'package)
;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'terminfo-mode)

(require 'tree-sitter)
(require 'tree-sitter-langs)

;; This handles reading custom key sequences I have set in xterm
;; xterm translates ctrl+i to ESC[27;5;105~ and emacs translates it back
;; the format is: ESC[27;<modifiers>;<charcode>~
(require 'term/xterm) ;this might not be the proper way to do this... I need to ensure that xterm--push-map exists though
(xterm--push-map
 (let ((map (make-sparse-keymap)))
   ;; key binds that
   ;; xterm and/or emacs were not
   ;; brave enough
   ;; to define
   ;; note: we are keeping [?\C-@] (i.e. [0], rendered as C-@) for ctrl+space
   ;; and remapping ctrl+@ to [C-@] (rendered as <C-@>)
   ;; also, ctrl+/ sends ^_ and ctrl+_ sends C-/
   (define-key map "\e[27;5;35~"  [?\C-#] )
   (define-key map "\e[27;5;50~"  [?\C-2] )
   (define-key map "\e[27;5;51~"  [?\C-3] )
   (define-key map "\e[27;5;52~"  [?\C-4] )
   (define-key map "\e[27;5;53~"  [?\C-5] )
   (define-key map "\e[27;5;54~"  [?\C-6] )
   (define-key map "\e[27;5;55~"  [?\C-7] )
   (define-key map "\e[27;5;56~"  [?\C-8] )
   (define-key map "\e[27;5;64~"  [C-@]   )
   (define-key map "\e[27;5;91~"  [C-\[]  )
   (define-key map "\e[27;5;94~"  [?\C-^] )
   (define-key map "\e[27;5;95~"  [?\C-/] )
   (define-key map "\e[27;5;96~"  [?\C-`] )
   (define-key map "\e[27;5;105~" [C-i]   )
   (define-key map "\e[27;5;109~" [C-m]   )
   (define-key map "\e[27;5;119~" [?\C-w] )
   (define-key map "\e[27;5;123~" [?\C-{] )
   (define-key map "\e[27;5;124~" [?\C-|] )
   (define-key map "\e[27;5;125~" [?\C-}] )
   (define-key map "\e[27;5;126~" [?\C-~] )
   map)
 input-decode-map)

;; smart tabs mode
(smart-tabs-add-language-support lua lua-mode-hook
  ((lua-indent-line . lua-indent-level)
   (lua-indent-region . lua-indent-level)))

(smart-tabs-insinuate
 'c 'c++ 'java 'javascript 'cperl 'python 'ruby 'nxml ;builtin
 'lua) ;custom

(add-hook
 'emacs-startup-hook
 (lambda ()
   (message "Emacs ready in %.2f seconds with %d garbage collections."
            (float-time (time-subtract after-init-time before-init-time))
            gcs-done)
   (setq gc-cons-threshold (* 1 1000 1000)))) ;reset gc threshold


(defface line-wrap-symbol
  '((t . (:background "#88EEFF" :foreground "#0000FF")))
  "face for line wrap symbols")

(defgroup display-table
  '((display-table-wrap custom-variable)
    (display-table-vertical-border custom-variable)
    (display-table-truncation custom-variable))
  "Standard display table glyphs")

(defmacro defcustom-table-slot (varname slot doc default)
  `(defcustom ,varname
     ,default
     ,doc
     :group 'display-table
     :type '(list character face)
     :set (lambda (sym value)
            (set-display-table-slot standard-display-table ,slot (make-glyph-code (car value) (car (cdr value)))))
     :get (lambda (sym)
            (let ((glyph (display-table-slot standard-display-table ,slot)))
              (list (glyph-char glyph) (or (glyph-face glyph) 'default))))))

(defvar display-table-vertical-border '(?\| vertical-border))
(defcustom-table-slot display-table-vertical-border
  'vertical-border
  "Glyph to use for the vertical borders separating windows"
  '(?\| vertical-border))
(defvar display-table-wrap '(?\\ default))
(defcustom-table-slot display-table-wrap 'wrap
  "Glyph to use for line wrap symbols"
  '(?\\ default))
(defvar display-table-truncation '(?\$ default))
(defcustom-table-slot display-table-truncation 'truncation
  "Glyph used to indicate lines that extend offscreen"
  '(?\$ default))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-directory-alist '(("." . "~/.saves")))
 '(blink-matching-paren-on-screen nil)
 '(c-basic-offset 3)
 '(css-indent-offset 3)
 '(delete-by-moving-to-trash t)
 '(display-table-vertical-border '(9475 vertical-border))
 '(display-table-wrap '(8617 line-wrap-symbol))
 '(display-time-mode t)
 '(form-feed-line-width t)
 '(formfeed-hline-mode t)
 '(gc-cons-threshold 1000000)
 '(global-form-feed-mode nil)
 '(global-highlight-parentheses-mode nil)
 '(global-tree-sitter-mode nil)
 '(gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
 '(highlight-parentheses-background-colors '("#EEDDCC"))
 '(highlight-parentheses-colors '("#FF0000" "IndianRed2" "IndianRed4"))
 '(indent-tabs-mode t)
 '(inhibit-default-init nil)
 '(inhibit-startup-screen t)
 '(js-indent-level 3)
 '(menu-bar-mode nil)
 '(mode-line-format
   '("%e" mode-line-front-space mode-line-client mode-line-modified mode-line-front-space " " mode-line-buffer-identification "   " mode-line-position
     (vc-mode vc-mode)
     "  " mode-line-modes mode-line-misc-info mode-line-end-spaces))
 '(mode-line-position-line-format '(" L:%l"))
 '(org-src-fontify-natively t)
 '(org-src-tab-acts-natively t)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("user42" . "https://download.tuxfamily.org/user42/elpa/packages/")))
 '(package-check-signature nil)
 '(package-selected-packages
   '(formfeed-hline eimp ascii-table sm-c-mode csv-mode ## circe page-break-lines highlight-parentheses rainbow-delimiters tree-sitter-langs tree-sitter modern-cpp-font-lock web-mode project-root lsp-mode gnu-elpa-keyring-update eglot babel kotlin-mode mines smart-tabs-mode lua-mode d-mode qt-pro-mode xclip))
 '(pascal-case-indent 3)
 '(sgml-basic-offset 3)
 '(sh-basic-offset 3)
 '(sh-indentation 3)
 '(show-paren-mode t)
 '(standard-indent 3)
 '(tab-width 3)
 '(tree-sitter-hl-use-font-lock-keywords nil)
 '(tree-sitter-major-mode-language-alist
   '((agda-mode . agda)
     (sh-mode . bash)
     (c-mode . c)
     (csharp-mode . c-sharp)
     (c++-mode . cpp)
     (css-mode . css)
     (elm-mode . elm)
     (go-mode . go)
     (haskell-mode . haskell)
     (html-mode . html)
     (java-mode . java)
     (js-mode . javascript)
     (js2-mode . javascript)
     (json-mode . json)
     (jsonc-mode . json)
     (julia-mode . julia)
     (ocaml-mode . ocaml)
     (php-mode . php)
     (python-mode . python)
     (rjsx-mode . javascript)
     (ruby-mode . ruby)
     (rust-mode . rust)
     (rustic-mode . rust)
     (scala-mode . scala)
     (swift-mode . swift)
     (tuareg-mode . ocaml)
     (typescript-mode . typescript)
     (sm-c-mode . c)))
 '(visible-cursor nil)
 '(web-mode-enable-control-block-indentation nil)
 '(xclip-mode t)
 '(xterm-mouse-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:extend t :background "#FFF0F0" :foreground "#CC0000" :slant italic))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "gray55"))))
 '(font-lock-regexp-grouping-construct ((t (:inherit bold))))
 '(hl-line ((t (:extend t :background "#DDFFDD"))))
 '(mode-line ((t (:box (:line-width (1 . -1) :style released-button) :foreground "#EEEEEE" :background "gray30"))))
 '(mode-line-inactive ((t (:inherit mode-line :background "grey60" :foreground "grey0" :box (:line-width (1 . -1) :color "grey75") :weight light))))
 '(tab-bar ((t (:foreground "black" :background "grey90" :inherit variable-pitch))))
 '(vertical-border ((t (:weight extra-bold :background "aaaaaaaaa" :inherit mode-line-inactive))))
 '(web-mode-doctype-face ((t (:foreground "Snow4"))))
 '(web-mode-html-attr-name-face ((t (:inherit font-lock-variable-name-face))))
 '(web-mode-html-tag-bracket-face ((t nil)))
 '(web-mode-html-tag-face ((t (:inherit font-lock-function-name-face))))
 '(widget-field ((t (:extend t :background "#80aaff" :foreground "black")))))



;; file extensions
(add-to-list 'auto-mode-alist '("\\.pro\\'" . qt-pro-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

(setq web-mode-offsetless-elements '("body" "html" "head")) ;don't indent inside some html elements

;; Custom keybinds
(define-minor-mode my-keys-minor-mode
  "my custom keybind mode"
  :init-value t
  :lighter " :" ;this makes a :) in the mode line
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map "\C-o" 'other-window)
            (define-key map [?\M-o] 'previous-window-any-frame)
            (define-key map "\C-j" 'previous-buffer)
            (define-key map [C-i] 'next-buffer) ;not TAB!
            (define-key map [?\M-\\] 'indent-region)
            (define-key map [(control x)(o)] (lambda () (interactive))) ;unbind C-x o so I stop using it
            map))


