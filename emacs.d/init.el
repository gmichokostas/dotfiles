;; -*- lexical-binding: t; -*-

(defvar file-name-handler-alist-old file-name-handler-alist)

(setq package-enable-at-startup nil
      file-name-handler-alist nil
      gc-cons-threshold 402653184
      gc-cons-percentage 0.8
      auto-window-vscroll nil)

(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)

(setq load-prefer-newer t)

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

;; MacOS related
(when (memq window-system '(mac ns darwin))
  ;; titlebar for MacOS
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (require 'ls-lisp)
  (setq mac-option-key-is-meta nil ;; rebind Meta key to cmd
        frame-title-format nil
        ns-use-proxy-icon nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier 'none
        dired-use-ls-dired nil
        ls-lisp-use-insert-directory-program nil))

;; general editor settings

(setq org-hide-leading-stars t
      org-ellipsis "⤵")
(setq apropos-sort-by-scores t)
(global-prettify-symbols-mode 1)
(global-auto-revert-mode 1) ;; reload file when it has changed in the disk
(windmove-default-keybindings)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; aliases

(defalias 'list-buffers 'ibuffer)
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'eb 'eval-buffer)

;; Better defaults

(setq
 column-number-mode t
 display-line-numbers-grow-only t
 initial-scratch-message ""
 load-prefer-newer t
 ring-bell-function 'ignore
 select-enable-clipboard t
 inhibit-startup-screen t)

(setq-default truncate-lines t
              indent-tabs-mode nil
              require-final-newline t)

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(delete-selection-mode 1)

;; highlight redundant whitespace
(add-hook 'prog-mode-hook (lambda ()
                            (interactive)
                            (setq show-trailing-whitespace 1)))

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(global-unset-key "\C-z")

;; disable auto-save and auto-backup

(setq auto-save-default nil
      make-backup-files nil
      create-lockfiles nil)

(setq ruby-insert-encoding-magic-comment nil)

;; UI

(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(set-frame-font "Meslo LG M DZ for Powerline 15")
(show-paren-mode)
(global-hl-line-mode t)

;; packages

(use-package htmlize :ensure t)
(use-package all-the-icons :ensure t)
(use-package undo-tree :ensure t)

(use-package minions
  :ensure t
  :config
  (setq minions-mode-line-lighter "[+]")
  (minions-mode))

(use-package color-identifiers-mode
  :ensure t
  :hook (after-init . global-color-identifiers-mode))

(use-package doom-modeline
  :ensure t
  :defer t
  :config
  (setq doom-modeline-height 20
        doom-modeline-bar-width 3
        doom-modeline-buffer-file-name-style 'truncate-upto-root
        doom-modeline-version nil
        doom-modeline-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-minor-modes t
        doom-modeline-github nil)
  :hook (after-init . doom-modeline-init))

(use-package hungry-delete
  :ensure t
  :bind (("C-<backspace>" . hungry-delete-backward))
  :config
  (global-hungry-delete-mode))

(use-package rust-mode
  :ensure t
  :hook ((rust-mode . company-mode))
  :config
  (setq rust-format-on-save t
        rust-rustfmt-bin "rustfmt"))

(use-package racer
  :ensure t
  :requires rust-mode
  :hook ((racer-mode . eldoc-mode)
         (racer-mode . company-mode)
         (rust-mode . racer-mode))
  :config
  (require 'f)
  (setq racer-cmd (f-expand "~/.cargo/bin/racer")
        racer-rust-src-path
        (f-expand "~/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src")))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package dockerfile-mode
  :ensure t
  :mode (("Dockerfile\\'" . dockerfile-mode)))

(use-package lua-mode
  :ensure t
  :mode (("\\.lua\\'" . lua-mode))
  :hook ((lua-mode . company-mode))
  :config
  (setq lua-indent-level 2))

(use-package company-lua
  :ensure t
  :after company)

(use-package eyebrowse
  :ensure t
  :config
  (setq eyebrowse-mode-line-separator " "
        eyebrowse-new-workspace t)
  (eyebrowse-setup-opinionated-keys)
  (eyebrowse-mode t))

(use-package magit
  :ensure t
  :defer t
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package git-gutter
  :ensure t
  :hook ((prog-mode . git-gutter-mode)
         (org-mode . git-gutter-mode)
         (yaml-mode . git-gutter-mode)))

(use-package org-bullets
  :ensure t
  :defer t
  :hook ((org-mode . (lambda () (org-bullets-mode 1)))))

(use-package dumb-jump
  :ensure t
  :hook (after-init . dumb-jump-mode)
  :bind (("C-M-o" . dumb-jump-go-other-window))
  :config
  (setq dumb-jump-selector 'ivy
        dumb-jump-prefer-searcher 'rg))

(use-package ibuffer-vc
  :ensure t
  :hook ((ibuffer-mode . (lambda ()
                           (ibuffer-vc-set-filter-groups-by-vc-root)
                           (unless (eq ibuffer-sorting-mode 'alphabetic)
                             (ibuffer-do-sort-by-alphabetic))))))

(use-package coffee-mode
  :ensure t
  :mode "\\.coffee\\'"
  :config
  (setq coffee-tab-width 2)
  (setq whitespace-action '(auto-cleanup))
  (setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab)))

(use-package vue-mode
  :ensure t
  :mode "\\.vue\\'"
  :config
  (setq mmm-submode-decoration-level 0))

(use-package slime
  :ensure t
  :defer t
  :config
  (setq inferior-lisp-program "/usr/local/bin/sbcl"
        slime-contribs '(slime-fancy)))

(use-package geiser
  :ensure t
  :defer t
  :config
  (setq geiser-active-implementations '(chicken)))

(use-package neotree
  :ensure t
  :defer t
  :bind (([f8] . neotree-toggle))
  :config
  (setq projectile-switch-project-action 'neotree-projectile-action
        neo-smart-open t))

(use-package yaml-mode
  :ensure t
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)))

(use-package doom-themes
  :ensure t
  :config
  (doom-themes-neotree-config)
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-one-brighter-modeline t
        doom-one-brighter-comments t
        doom-neotree-enable-folder-icons t
        doom-neotree-enable-file-icons t
        doom-neotree-enable-chevron-icons t
        doom-neotree-project-size 1
        doom-neotree-folder-size 1
        doom-neotree-chevron-size 0.6)
  (load-theme 'doom-one t))

(use-package cider
  :ensure t
  :defer t
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
  :mode ("\\.rb\\'" . ruby-mode)
  :hook ((ruby-mode . ruby-electric-mode)
         (ruby-mode . hs-minor-mode)))

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
  :init (setq projectile-completion-system 'ivy)
  :bind (("M-p" . projectile-find-file)
         ("M-t" . projectile-toggle-between-implementation-and-test))
  :config
  (projectile-mode +1))

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :config
  (setq company-dabbrev-downcase nil
        company-require-match nil))

(use-package multiple-cursors
  :ensure t
  :bind (("M-." . mc/mark-next-like-this-word)
         ("M-," . mc/mark-previous-like-this-word)
         ("C-M-<mouse-1>" . mc/add-cursor-on-click))
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

(when (executable-find "aspell")
  (setq-default ispell-program-name "aspell")
  (add-hook 'org-mode-hook (lambda ()
                             (flyspell-mode 1))))

(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))

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
If there's no region, the current line will be duplicated.  However, if
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

(eval-after-load "hideshow"
  '(add-to-list 'hs-special-modes-alist
              `(ruby-mode
                ,(rx (or "def" "class" "module" "do" "{" "[" "if" "else" "unless")) ; Block start
                ,(rx (or "}" "]" "end"))                       ; Block end
                ,(rx (or "#" "=begin"))                        ; Comment start
                ruby-forward-sexp nil)))

(global-set-key (kbd "C-c h") 'hs-hide-block)
(global-set-key (kbd "C-c s") 'hs-show-block)
