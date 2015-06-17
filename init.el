(require 'package)
(push '("marmalade" . "http://marmalade-repo.org/packages/")
      package-archives )
(push '("melpa" . "http://melpa.milkbox.net/packages/")
              package-archives)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(
    paredit
    clojure-mode
    clojure-mode-extra-font-locking
    cider
    projectile
    evil
    web-mode
    solarized-theme
    flx-ido
    geiser
    highlight-parentheses
    ))

(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq ring-bell-function 'ignore)
(setq inhibit-startup-message t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(show-paren-mode)

;; Indentation
(setq-default c-basic-indent 2)
(setq-default tab-width 2)          ;; set tw=2
(setq-default indent-tabs-mode nil) ;; set expandtab

;; auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(setq ac-auto-show-menu t)
(setq ac-auto-start t)
(setq ac-quick-help-delay 0.3)
(setq ac-quick-help-height 30)
(setq ac-show-menu-immediately-on-auto-complete t)
(ac-config-default)

(evil-mode)

;; rainbow
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; paredit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'clojure-mode-hook          #'paredit-mode)

;; cider
(add-hook 'cider-mode-hook 'eldoc-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
(setq nrepl-hide-special-buffers t)
(setq cider-repl-history-file "~/.emacs.d/cache/cider-history")
(setq cider-repl-wrap-history t)
(setq cider-repl-history-size 1000)
(setq cider-show-error-buffer nil)
(require 'icomplete)

;; projectile
(projectile-global-mode)

;; flx-ido
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; web-mode
(require 'web-mode)
(setq web-mode-css-indent-offset 2)
(setq web-mode-enable-css-colorization t)


;; disable auto-save and auto-backup
(setq auto-save-default nil)
(setq make-backup-files nil)

;; enable ruby mode
(add-to-list 'auto-mode-alist
	     '("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist
             '("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))
(setq-default tab-width 2)
(setq enh-ruby-indent-tabs-mode t)
(defvaralias 'enh-ruby-indent-level 'tab-width)
(defvaralias 'enh-ruby-hanging-indent-level 'tab-width)

;; line numbers
(global-linum-mode t)
(global-hl-line-mode +1)
(set-face-foreground 'highlight nil)

;; aliases
(defalias 'yes-or-no-p 'y-or-n-p)

(load-theme 'solarized-dark t)
(setq solarized-use-less-bold t)
;;(load-theme 'solarized t)
;;(set-frame-parameter nil 'background-mode 'dark)
;;(set-terminal-parameter nil 'background-mode 'dark)
(custom-set-faces (if (not window-system) '(default ((t (:background "nil"))))))
(set-face-attribute 'default nil :family "Inconsolata" :height 170)

;; (require 'powerline)
;; (powerline-default-theme)

;; (require 'moe-theme)
;; (setq moe-theme-highlight-buffer-id t)
;; (moe-theme-set-color 'blue)
;; (powerline-moe-theme)
;; (moe-dark)

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("(\\(fn\\)[\[[:space:]]"
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "λ")
                               nil))))))

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("\\(#\\)("
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "ƒ")
                               nil))))))

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("\\(#\\){"
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "∈")
                               nil))))))
