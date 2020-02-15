;; -*- lexical-binding: t; -*-

(defvar file-name-handler-alist-old file-name-handler-alist)

(setq package-enable-at-startup nil
      file-name-handler-alist nil
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      auto-window-vscroll nil)

(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold 16777216
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)

(defun ym/defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun ym/restore-garbage-collection-h ()
  "Defer it so that commands launched immediately after will enjoy the benefits."
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold 16777216))))

(add-hook 'minibuffer-setup-hook #'ym/defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'ym/restore-garbage-collection-h)

(setq load-prefer-newer t)

;; Package configs
(eval-when-compile
  (require 'package)
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package))

(setq package-enable-at-startup nil)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; MacOS related
(when (memq window-system '(mac ns darwin))
  ;; titlebar for MacOS
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (require 'ls-lisp)
  (setq mac-option-key-is-meta nil ;; rebind Meta key to cmd
        mac-option-modifier 'super
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier 'none
        frame-title-format nil
        ns-use-proxy-icon nil
        ns-use-thin-smoothing t
        dired-use-ls-dired nil
        ls-lisp-use-insert-directory-program nil))

;; general editor settings
(setq org-hide-leading-stars t
      org-ellipsis "⤵")
(setq apropos-sort-by-scores t
      require-final-newline t)

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

(setq column-number-mode t
      display-line-numbers-grow-only t
      initial-scratch-message ""
      load-prefer-newer t
      frame-inhibit-implied-resize t
      ring-bell-function 'ignore
      select-enable-clipboard t
      mouse-select-region-move-to-beginning t
      mouse-drag-and-drop-region t
      list-matching-lines-jump-to-current-line t
      whitespace-line-column 80
      inhibit-startup-screen t)

(recentf-mode nil)

(setq-default cursor-type 'bar
              truncate-lines t
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

(global-set-key (kbd "C-<backspace>") 'just-one-space)

;; disable auto-save and auto-backup

(setq auto-save-default nil
      make-backup-files nil
      create-lockfiles nil)

(setq ruby-insert-encoding-magic-comment nil)

;; UI
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(show-paren-mode)
(global-hl-line-mode t)

(add-to-list 'default-frame-alist '(font . "Menlo-15"))

;; modeline config
(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification " "
                mode-line-position
                minions-mode-line-modes
                mode-line-misc-info " "
                flycheck-mode-line
                vc-mode
                mode-line-end-spaces))

(add-hook
 'after-init-hook
 (lambda nil
   (progn
     (set-face-attribute 'font-lock-variable-name-face
                         nil
                         :weight 'semi-bold'
                         :slant 'italic)

     (set-face-attribute 'font-lock-comment-face
                         nil
                         :slant 'italic)

     (set-face-attribute 'font-lock-function-name-face
                         nil
                         :slant 'italic)

     (set-face-attribute 'font-lock-string-face
                         nil
                         :slant 'italic))))

;; packages
;;
(use-package ace-window
  :ensure t
  :defer 1
  :bind (("M-p" . 'ace-window)))

(use-package git-link
  :ensure t
  :defer 1)

(use-package restclient
  :ensure t
  :mode (("\\.http\\'" . restclient-mode)))

(use-package company-restclient
  :ensure t
  :defer t
  :config
  (add-to-list 'company-backends 'company-restclient))

(use-package htmlize
  :ensure t
  :defer 2)

(use-package minions
  :ensure t
  :config
  (setq minions-mode-line-lighter "[+]")
  (minions-mode))

(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :hook (before-save . gofmt-before-save)
  :config
  (setq gofmt-command "goimports"))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package flycheck-clj-kondo
  :ensure t
  :defer t)

(use-package flycheck
  :ensure t
  :defer 2
  :config
  (global-flycheck-mode)
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(ruby-rubocop javascript-jshint)))
  (flycheck-add-mode 'javascript-eslint 'web-mode))

(use-package dockerfile-mode
  :ensure t
  :mode (("Dockerfile\\'" . dockerfile-mode)))

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
  :defer 1
  :hook ((prog-mode . git-gutter-mode)
         (org-mode . git-gutter-mode)
         (yaml-mode . git-gutter-mode)))

(use-package org
  :ensure org-plus-contrib
  :mode (("\\.org\\'" . org-mode)))

(use-package org-bullets
  :ensure t
  :defer t
  :hook ((org-mode . (lambda () (org-bullets-mode 1)))))

(use-package org-journal
  :ensure t
  :config
  (setq org-journal-file-type 'yearly
        org-journal-dir "~/notes/"
        org-journal-time-format ""
        org-journal-file-format "%Y.org"
        org-journal-date-format "%A, %d %B %Y"))

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
        neo-window-fixed-size nil
        neo-smart-open t))

(use-package yaml-mode
  :ensure t
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)))

(use-package js2-mode
  :ensure t
  :hook ((js2-mode . js2-imenu-extras-mode))
  :mode (("\\.js\\'" . js2-mode))
  :config
  (setq js2-basic-offset 2
        js2-include-jslint-globals nil
        js2-include-jslint-declaration-externs nil))

(use-package json-mode
  :ensure t
  :hook ((json-mode . (lambda () (make-local-variable 'js-indent-level)
                        (setq js-indent-level 2))))
  :mode (("\\.json\\'" . json-mode)))

(use-package base16-theme
  :ensure t
  :config
  (setq base16-highlight-mode-line 'contrast)
  (load-theme 'base16-tomorrow-night t))

(use-package cider
  :ensure t
  :defer t
  :config (setq cider-repl-use-pretty-printing t))

(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode))
  :config
  (require 'flycheck-clj-kondo))

(use-package clojure-mode-extra-font-locking
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljs\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode)))

(use-package lispy
  :ensure t
  :hook ((clojure-mode . lispy-mode)
         (emacs-lisp-mode . lispy-mode)))

(use-package ruby-electric
  :ensure t
  :mode ("\\.rb\\'" . ruby-mode)
  :hook ((ruby-mode . ruby-electric-mode)
         (ruby-mode . hs-minor-mode)))

(use-package rspec-mode
  :ensure t
  :init
  (add-hook 'after-init-hook 'inf-ruby-switch-setup))

(use-package inf-ruby
  :ensure t
  :hook ((ruby-mode . inf-ruby-minor-mode)
         (compilation-filter-hook inf-ruby-auto-enter)))

(use-package projectile-rails
  :ensure t
  :config
  (projectile-rails-global-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode . rainbow-delimiters-mode)
         (cider-repl-mode . rainbow-delimiters-mode)))

(use-package smartparens
  :ensure t
  :hook ((prog-mode . smartparens-mode)
         (cider-repl-mode . smartparens-mode)))

(use-package ivy
  :ensure t
  :bind (("C-c C-r" . ivy-resume))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-format-function 'ivy-format-function-arrow
        enable-recursive-minibuffers t))

(use-package counsel
  :ensure t
  :bind(("M-x"     . counsel-M-x)
        ("C-c l"   . counsel-semantic-or-imenu)
        ("C-x C-f" . counsel-find-file)
        ("<f1> f"  . counsel-describe-function)
        ("<f1> v"  . counsel-describe-variable)
        ("<f1> l"  . counsel-find-library)
        ("<f2> i"  . counsel-info-lookup-symbol)
        ("<f2> u"  . counsel-unicode-char)
        ("C-c g"   . counsel-git)
        ("C-c j"   . counsel-git-grep)
        ("C-c a"   . counsel-ag)
        ("C-x l"   . counsel-locate)
        ("C-c k"   . counsel-rg)
        ("C-c n"   . counsel-fzf))
  :config
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package avy
  :ensure t
  :bind (("C-;" . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package projectile
  :ensure t
  :config
  (setq projectile-completion-system 'ivy
        projectile-create-missing-test-files t
        projectile-project-search-path '("~/Code/"))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :config
  (setq company-dabbrev-downcase nil
        company-require-match nil))

(use-package multiple-cursors
  :ensure t
  :bind (("C-." . mc/mark-next-like-this-word)
         ("C-," . mc/mark-previous-like-this-word)
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
         ("\\.erb\\'" . web-mode)
         ("\\.vue\\'" . web-mode)))

(use-package elfeed
  :ensure t
  :defer t
  :hook ((elfeed-new-entry . (lambda () (elfeed-make-tagger :before "3 weeks ago"
				                       :remove 'eunread))))
  :config
  (setq elfeed-feeds
        '("https://www.youtube.com/feeds/videos.xml?channel_id=UCMGXFEew8I6gzjg3tWen4Gw"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCKTehwyGCKF-b2wo0RKwrcg"
          "https://nullprogram.com/feed/"
          "https://www.masteringemacs.org/feed/"
          "http://planet.emacsen.org/atom.xml"
          "https://www.reddit.com/r/emacs/.rss"
          "https://irreal.org/blog/?feed=rss2"
          "https://emacsnotes.wordpress.com/feed/")))

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

(global-set-key (kbd "M-S-<up>") 'move-region-up)
(global-set-key (kbd "M-S-<down>") 'move-region-down)

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

(global-set-key (kbd "<S-return>") 'smart-open-line-below)
(global-set-key (kbd "C-o") 'smart-open-line-above)

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
