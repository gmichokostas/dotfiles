;; Package configs
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(when (eq system-type 'darwin)
  ;; titlebar for MacOS
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (setq ns-use-proxy-icon  nil)
  (setq frame-title-format nil)
  (setq mac-option-key-is-meta nil ;; rebind Meta key to cmd
	mac-command-key-is-meta t
	mac-command-modifier 'meta
	mac-option-modifier 'none))

;; general editor settings
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(global-display-line-numbers-mode t)
(global-hl-line-mode t)

;; UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(set-frame-font "Menlo 13")
(setq column-number-mode t)
(setq display-line-numbers-grow-only t)
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)
(show-paren-mode)


;; packages

(use-package doom-themes
  :ensure t
  :config (load-theme 'doom-one t))

(use-package cider
  :ensure t
  :config (setq cider-repl-use-pretty-printing t))

(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode)
	 ("\\.cljs\\'" . clojurescript-mode)))

(use-package ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby")

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smartparens
  :ensure t
  :hook (clojure-mode . smartparens-mode))

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package projectile
  :ensure t
  :config (projectile-mode))

(use-package company
  :ensure t
  :init (global-company-mode))

;; use shift arrow to navigate from win to win
(windmove-default-keybindings)

;; aliases
(defalias 'list-buffers 'ibuffer)
(defalias 'yes-or-no-p 'y-or-n-p)

;; disable auto-save and auto-backup
(setq auto-save-default nil)
(setq make-backup-files nil)

;; ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; move lines around
(defmacro save-column (&rest body)
  `(let ((column (current-column)))
     (unwind-protect
         (progn ,@body)
       (move-to-column column))))
(put 'save-column 'lisp-indent-function 0)

(defun move-line-up ()
  (interactive)
  (save-column
    (transpose-lines 1)
    (forward-line -2)))

(defun move-line-down ()
  (interactive)
  (save-column
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)))

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

;; move regions around
(defun move-region (start end n)
  "Move the current region up or down by N lines."
  (interactive "r\np")
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (let ((start (point)))
      (insert line-text)
      (setq deactivate-mark nil)
      (set-mark start))))

(defun move-region-up (start end n)
  "Move the current line up by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) -1 (- n))))
(defun move-region-down (start end n)
  "Move the current line down by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) 1 n)))

(global-set-key (kbd "M-C-<up>") 'move-region-up)
(global-set-key (kbd "M-C-<down>") 'move-region-down)
