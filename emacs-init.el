;;(add-to-list 'load-path "~/.emacs.d/lisp/")
(setq inhibit-splash-screen t)
(setq visible-cursor nil)
(setq backup-directory-alist `(("." . "~/.saves")))
(menu-bar-mode -1)

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3") 

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
 '(indent-tabs-mode t)
 '(js-indent-level 3)
 '(org-src-fontify-natively t)
 '(org-src-tab-acts-natively t)
 '(package-selected-packages
	'(highlight-parentheses rainbow-delimiters tree-sitter-langs tree-sitter modern-cpp-font-lock web-mode project-root lsp-mode gnu-elpa-keyring-update eglot babel kotlin-mode mines smart-tabs-mode lua-mode d-mode qt-pro-mode))
 '(pascal-case-indent 3)
 '(sgml-basic-offset 3)
 '(sh-basic-offset 3)
 '(sh-indentation 3)
 '(standard-indent 3)
 '(tab-width 3)
 '(tree-sitter-hl-use-font-lock-keywords nil)
 '(web-mode-enable-control-block-indentation nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "Firebrick"))))
 '(hl-line ((t (:extend t :background "#DDFFDD"))))
 '(mode-line ((t (:box (:line-width (1 . -1) :style released-button) :foreground "white" :background "gray30"))))
 '(mode-line-inactive ((t (:inherit mode-line :background "grey60" :foreground "grey0" :box (:line-width (1 . -1) :color "grey75") :weight light))))
 '(tab-bar ((t (:foreground "black" :background "grey90" :inherit variable-pitch))))
 '(web-mode-doctype-face ((t (:foreground "Snow4"))))
 '(web-mode-html-attr-name-face ((t (:inherit font-lock-variable-name-face))))
 '(web-mode-html-tag-bracket-face ((t nil)))
 '(web-mode-html-tag-face ((t (:inherit font-lock-function-name-face))))
 '(widget-field ((t (:extend t :background "#80aaff" :foreground "black")))))

(add-to-list 'auto-mode-alist '("\\.pro\\'" . qt-pro-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(setq web-mode-offsetless-elements (list "body" "html" "head"))

(add-hook
 'emacs-startup-hook
 (lambda nil
   (message "Emacs ready in %.2f seconds with %d garbage collections."
            (float-time (time-subtract after-init-time before-init-time))
            gcs-done)))

;; (defface font-lock-arrow-face
;;   '((t . (:foreground "#A00000" :italic t)))
;;   "face to display C++ properties in")

;; (defface font-lock-separator-face
;;   '((t . (:foreground "orangered" :bold t)))
;;   "face to display C++ separators and whatever in")

;; (defface font-lock-paren-face
;;   '((t . (:foreground "#808080")))
;;   "face to display C++ parentheses in")

;; (font-lock-add-keywords 'c++-mode
;;  								`((
;;  									"\\(->\\|\\.\\)\\([A-Za-z_][A-Za-z0-9_]*\\)" 2
;;  									'font-lock-arrow-face t )))

;; (font-lock-add-keywords 'c++-mode
;;  								`((
;;  									"\\(->\\|\\.\\|=\\)" 1
;;  									'font-lock-separator-face t )))

;; (font-lock-add-keywords 'c++-mode
;;  								`((
;;  									"\\((\\|)\\)" 1
;;  									'font-lock-paren-face t )))

(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-o" 'other-window)
	 (define-key map [?\M-o] 'previous-window-any-frame)
    (define-key map "\C-j" 'previous-buffer)
	 (define-key map [C-i] 'next-buffer)
   ;; (define-key map [?\M-j] 'next-buffer)
    (define-key map [?\M-\\] 'indent-region)
    (define-key map [remap (control x)(o)] nil)
    map)
  "my-keys-minor-mode keymap.")

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " :")

(defface line-wrap-symbol
  '((t . (:background "#88EEFF" :foreground "#0000FF")))
  "face for line wrap symbols")

(set-display-table-slot
 standard-display-table
 'wrap (make-glyph-code ?\↩ 'line-wrap-symbol))
;; todo: find a symbol for this!◌

(require 'tree-sitter)
(require 'tree-sitter-langs)
(global-tree-sitter-mode)
(xterm-mouse-mode)
(global-highlight-parentheses-mode)
;;(global-hl-line-mode)

(require 'term/xterm)
(xterm--push-map
 (let ((map (make-sparse-keymap)))
	(define-key map "\e[27;5;105~" [C-i])
   map)
 input-decode-map)
