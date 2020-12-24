(setq gc-cons-threshold (* 50 1000 1000)) ;set threshold high to reduce garbage collection during some of startup
(setq inhibit-splash-screen t) ;disable splash screen
(setq visible-cursor nil) ;disable cursor blink
(setq backup-directory-alist '(("." . "~/.saves"))) ;don't put backups in current directory
(menu-bar-mode -1) ;hide menu bar

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3") ;idk this fixed something

;; (I shortened this part because I'm always going to have https support)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 3)
 '(css-indent-offset 3)
 '(form-feed-line-width t)
 '(global-form-feed-mode t)
 '(global-highlight-parentheses-mode nil)
 '(global-tree-sitter-mode t)
 '(highlight-parentheses-background-colors '("#EEDDCC"))
 '(highlight-parentheses-colors '("#FF0000" "IndianRed2" "IndianRed4"))
 '(indent-tabs-mode t)
 '(js-indent-level 3)
 '(mode-line-format
	'("%e" mode-line-front-space mode-line-client mode-line-modified mode-line-front-space " " mode-line-buffer-identification "   " mode-line-position
	  (vc-mode vc-mode)
	  "  " mode-line-modes mode-line-misc-info mode-line-end-spaces))
 '(mode-line-position-line-format '(" L:%l"))
 '(org-src-fontify-natively t)
 '(org-src-tab-acts-natively t)
 '(package-selected-packages
	'(csv-mode ## circe page-break-lines highlight-parentheses rainbow-delimiters tree-sitter-langs tree-sitter modern-cpp-font-lock web-mode project-root lsp-mode gnu-elpa-keyring-update eglot babel kotlin-mode mines smart-tabs-mode lua-mode d-mode qt-pro-mode xclip))
 '(pascal-case-indent 3)
 '(sgml-basic-offset 3)
 '(sh-basic-offset 3)
 '(sh-indentation 3)
 '(standard-indent 3)
 '(tab-width 3)
 '(tree-sitter-hl-use-font-lock-keywords nil)
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
 '(vertical-border ((t (:inherit mode-line-inactive :background "transparent" :weight ultra-bold))))
 '(web-mode-doctype-face ((t (:foreground "Snow4"))))
 '(web-mode-html-attr-name-face ((t (:inherit font-lock-variable-name-face))))
 '(web-mode-html-tag-bracket-face ((t nil)))
 '(web-mode-html-tag-face ((t (:inherit font-lock-function-name-face))))
 '(widget-field ((t (:extend t :background "#80aaff" :foreground "black")))))

;; file extensions
(add-to-list 'auto-mode-alist '("\\.pro\\'" . qt-pro-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

(setq web-mode-offsetless-elements '("body" "html" "head")) ;don't indent inside some html elements

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %.2f seconds with %d garbage collections."
                     (float-time (time-subtract after-init-time before-init-time))
                     gcs-done)
            (setq gc-cons-threshold (* 1 1000 1000)))) ;reset gc threshold

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

;; Change line wrap indicator from \ to a blue ↩ (arrow, if your font has it)
(defface line-wrap-symbol
  '((t . (:background "#88EEFF" :foreground "#0000FF")))
  "face for line wrap symbols")
(set-display-table-slot
 standard-display-table
 'wrap (make-glyph-code ?\↩ 'line-wrap-symbol))

(set-display-table-slot
 standard-display-table
 'vertical-border (make-glyph-code ?\┃ 'vertical-border))

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
   (define-key map "\e[27;5;105~" [C-i])
   (define-key map "\e[27;5;109~" [C-m])
   (define-key map "\e[27;5;91~" [C-\[])
   (define-key map "\e[27;5;54~" [?\C-6])
   ;; note: we are keeping [?\C-@] (i.e. [0], rendered as C-@) for ctrl+space
   ;; and remapping ctrl+@ to [C-@] (rendered as <C-@>)
   (define-key map "\e[27;5;64~" [C-@])
   (define-key map "\e[27;5;50~" [?\C-2])
   (define-key map "\e[27;5;126~" [?\C-~])
   (define-key map "\e[27;5;96~" [?\C-`])
   (define-key map "\e[27;13;27~" [?\C-\M-\e]) ;wow
   (define-key map "\e[27;9;27~" [?\M-\e])
   (define-key map "\e[27;5;27~" [?\C-\e])
   map)
 input-decode-map)

;; smart tabs mode
(smart-tabs-add-language-support lua lua-mode-hook
  ((lua-indent-line . lua-indent-level)
   (lua-indent-region . lua-indent-level)))

(smart-tabs-insinuate
 'c 'c++ 'java 'javascript 'cperl 'python 'ruby 'nxml ;builtin
 'lua) ;custom
