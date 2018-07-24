;; Package configs
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "https://orgmode.org/elpa/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

;; MacOS related
(when (eq system-type 'darwin)
  (exec-path-from-shell-initialize)
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
(global-auto-revert-mode 1) ;; reload file when it has changed in the disk

;; Better defaults
(setq
  column-number-mode t
  display-line-numbers-grow-only t
  initial-scratch-message ""
  load-prefer-newer t
  ring-bell-function 'ignore
  select-enable-clipboard t
  inhibit-startup-screen t)

(setq-default indent-tabs-mode nil)

;; UI
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(set-frame-font "Roboto Mono for Powerline 14")
(show-paren-mode)
(global-hl-line-mode t)
(global-display-line-numbers-mode t)
(set-face-background 'hl-line "gray16")

;; packages

(use-package magit
  :ensure t)

(use-package tangotango-theme
  :ensure t
  :config
  (load-theme 'tangotango t))

(use-package cider
  :ensure t
  :config (setq cider-repl-use-pretty-printing t))

(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode)))

(use-package flymake-ruby
  :ensure t
  :hook (ruby-mode . flymake-ruby-load))

(use-package ruby-electric
  :ensure t
  :hook (ruby-mode . ruby-electric-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode . rainbow-delimiters-mode)
         (cider-repl-mode . rainbow-delimiters-mode)))

(use-package smartparens
  :ensure t
  :hook ((prog-mode . smartparens-mode)
         (cider-repl-mode . smartparens-mode)))

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package projectile
  :ensure t
  :config (projectile-mode))

(use-package company
  :ensure t
  :init (global-company-mode))

(use-package multiple-cursors
  :ensure t
  :bind (("M-." . mc/mark-next-like-this-symbol)
         ("M-," . mc/mark-previous-like-this-symbol))
  :hook (prog-mode . multiple-cursors-mode))

(use-package web-mode
  :ensure t
  :config
  (setq web-mode-extra-auto-pairs '(("erb" . (("beg" "end")))))
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-current-element-highlight t)
  :mode (("\\.html?\\'" . web-mode)
         ("\\.erb\\'" . web-mode)))

;; use shift arrow to navigate from window to window
(windmove-default-keybindings)

;; aliases
(defalias 'list-buffers 'ibuffer)
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'eb 'eval-buffer)

;; disable auto-save and auto-backup
(setq auto-save-default nil)
(setq make-backup-files nil)

;; ido
(setq ido-enable-flex-matching t
      ido-everywhere t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess)
(ido-mode 1)

;; move lines around
(defmacro save-column (&rest body)
  `(let ((column (current-column)))
     (unwind-protect
       (progn ,@body)
       (move-to-column column))))
(put 'save-column 'lisp-indent-function 0)

(defun move-line-up ()
  "Move the current line up one line."
  (interactive)
  (save-column
    (transpose-lines 1)
    (forward-line -2)))

(defun move-line-down ()
  "Move the current line down one line."
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

(global-set-key (kbd "M-p") 'clipboard-yank) ;; paste from clipboard
(global-set-key (kbd "M-;") 'comment-line) ;; rebind add comment key combo

;; smart openline
(defun smart-open-line-below (arg)
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode.
   With a prefix ARG open line above the current line."
  (interactive "P")
  (if arg
      (smart-open-line-above)
    (progn
      (move-end-of-line nil)
      (newline-and-indent))))

(defun smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "C-o") 'smart-open-line-below)
(global-set-key (kbd "M-o") 'smart-open-line-above)

;; highlight redundant whitespace
(add-hook 'prog-mode-hook (lambda ()
                            (interactive)
                            (setq show-trailing-whitespace 1)))
