;; Allow more than 800Kb cache during init
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(defun mu-set-gc-threshold ()
  "Reset `gc-cons-threshold' and `gc-cons-percentage' to their default values."
  (setq gc-cons-threshold 16777216
        gc-cons-percentage 0.1))

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
  (require 'ls-lisp)
  (setq ns-use-proxy-icon  nil)
  (setq frame-title-format nil)
  (setq mac-option-key-is-meta nil ;; rebind Meta key to cmd
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier 'none
        dired-use-ls-dired nil
        ls-lisp-use-insert-directory-program nil))

;; general editor settings
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(global-auto-revert-mode 1) ;; reload file when it has changed in the disk
(setq apropos-sort-by-scores t)

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
(setq-default frame-title-format "%f")

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
  :ensure t
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package git-gutter
  :ensure t
  :delight (git-gutter-mode)
  :hook ((prog-mode . git-gutter-mode)))

(use-package org-bullets
  :ensure t
  :hook ((org-mode . (lambda () (org-bullets-mode 1)))))

(use-package undo-tree
  :ensure t)

(use-package dumb-jump
  :ensure t
  :init
  (dumb-jump-mode)
  :bind (("C-M-o" . dumb-jump-go-other-window))
  :config
  (setq dumb-jump-selector 'ivy))(use-package delight
  :ensure t)

(use-package ibuffer-vc
  :ensure t
  :init
  :hook ((ibuffer-mode . (lambda ()
                           (ibuffer-vc-set-filter-groups-by-vc-root)
                           (unless (eq ibuffer-sorting-mode 'alphabetic)
                             (ibuffer-do-sort-by-alphabetic))))))

(use-package coffee-mode
  :ensure t
  :config
  (setq coffee-tab-width 2)
  (setq whitespace-action '(auto-cleanup))
  (setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab)))

(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "opt/sbcl/bin/sbcl"
        slime-contribs '(slime-fancy)))

(use-package geiser
  :ensure t
  :config
  (setq geiser-active-implementations '(mit)))

(use-package neotree
  :ensure t
  :bind (([f8] . neotree-toggle))
  :config
  (setq projectile-switch-project-action 'neotree-projectile-action))

(use-package yaml-mode
  :ensure t
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)))

(use-package doom-themes
  :ensure t
  :config
  (doom-themes-neotree-config)
  (setq doom-neotree-enable-folder-icons t
        doom-neotree-enable-file-icons t
        doom-neotree-enable-chevron-icons t)
  (load-theme 'doom-nord t))

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

(use-package ivy
  :ensure t
  :delight ivy-mode
  :bind (("C-c C-r" . ivy-resume))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

(use-package counsel
  :ensure t
  :bind(("M-x" . counsel-M-x)
        ("C-x C-f" . counsel-find-file)
        ("<f1> f" . counsel-describe-function)
        ("<f1> v" . counsel-describe-variable)
        ("<f1> l" . counsel-find-library)
        ("<f2> i" . counsel-info-lookup-symbol)
        ("<f2> u" . counsel-unicode-char)
        ("C-c g" . counsel-git)
        ("C-c j" . counsel-git-grep)
        ("C-c a" . counsel-ag)
        ("C-x l" . counsel-locate))
  :config
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package swiper
  :ensure t
  :bind (("\C-s" . swiper)))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package projectile
  :ensure t
  :init
  (setq projectile-completion-system 'ivy)
  :delight '(:eval (concat " " (projectile-project-name)))
  :bind (("M-p" . projectile-find-file)
         ("M-t" . projectile-toggle-between-implementation-and-test))
  :config
  (projectile-mode +1))

(use-package company
  :ensure t
  :init (global-company-mode)
  :config
  (setq company-dabbrev-downcase nil))

(use-package multiple-cursors
  :ensure t
  :bind (("M-." . mc/mark-next-like-this-symbol)
         ("M-," . mc/mark-previous-like-this-symbol))
  :hook (prog-mode . multiple-cursors-mode))

(use-package web-mode
  :ensure t
  :hook ((web-mode . (lambda () (setq web-mode-markup-indent-offset 2))))
  :config
  (setq web-mode-extra-auto-pairs '(("erb" . (("beg" "end")))))
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-code-indent-offset 2)
  :mode (("\\.html?\\'" . web-mode)
         ("\\.erb\\'" . web-mode)))

(windmove-default-keybindings)

;; aliases
(defalias 'list-buffers 'ibuffer)
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'eb 'eval-buffer)

;; disable auto-save and auto-backup
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)

(setq ruby-insert-encoding-magic-comment nil)

(delete-selection-mode 1)

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

(global-set-key (kbd "M-j") 'clipboard-yank) ;; paste from clipboard
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

(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

;; highlight redundant whitespace
(add-hook 'prog-mode-hook (lambda ()
                            (interactive)
                            (setq show-trailing-whitespace 1)))

;; Reset default values
(add-hook 'emacs-startup-hook #'mu-set-gc-threshold)
