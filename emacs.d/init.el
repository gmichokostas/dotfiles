;;; -*- lexical-binding: t; -*-

(defvar file-name-handler-alist-old file-name-handler-alist)

(setq package-enable-at-startup nil
      file-name-handler-alist   nil
      gc-cons-threshold         most-positive-fixnum
      gc-cons-percentage        0.8
      auto-window-vscroll       nil)

(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold       16777216
                   gc-cons-percentage      0.1)
             (garbage-collect)) t)

(defun ym/defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun ym/restore-garbage-collection-h ()
  "Defer it so that commands launched immediately after will enjoy the benefits."
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold 16777216))))

(add-hook 'minibuffer-setup-hook #'ym/defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook  #'ym/restore-garbage-collection-h)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; setup use-package
(eval-and-compile
  (require 'package)
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
			   ("gnu" . "https://elpa.gnu.org/packages/")
                           ("org" . "https://orgmode.org/elpa/")))
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package)
  (setf use-package-always-ensure t))

;; macOS config
(when (and (eq system-type 'darwin)
	   (display-graphic-p))
  ;; titlebar for macOS
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (require 'ls-lisp)
  ;; swap meta and hyper positions for macOS
  (setq mac-option-modifier   'super
        mac-command-modifier  'meta
	ns-function-modifier  'hyper
	ns-use-thin-smoothing t
	dired-use-ls-dired    nil))

;; set default font
(add-to-list 'default-frame-alist '(font . "Iosevka-16"))

;; use italics in comments
(custom-set-faces
 '(font-lock-comment-face ((t (:slant italic))))
 '(font-lock-type-face    ((t (:slant italic)))))

;; disable auto-save and auto-backup
(setq auto-save-default nil
      make-backup-files nil
      create-lockfiles  nil)

;; aliases
(defalias 'list-buffers 'ibuffer)
(defalias 'yes-or-no-p  'y-or-n-p)
(defalias 'eb           'eval-buffer)

;; trim trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; enable electric-pair-mode in programming mode to
;; get matching delimiters
(add-hook 'prog-mode-hook 'electric-pair-mode)

;; line numbers config
;;; show line numbers in prog mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(setq column-number-mode             t     ; show lin-number/column combo
      display-line-numbers-grow-only t     ; dont resize line number column when we go to smaller line number
      display-line-numbers-type 'relative) ; line number type is relative to mark

;; always add a new line at the EOF
(setq require-final-newline t)

;; show funcy symbols like λ for lambads
(global-prettify-symbols-mode 1)

;; reload file when it has changed in the disk
(global-auto-revert-mode 1)

;; disable scratch message
(setq initial-scratch-message "")

;; please don't ring
(setq ring-bell-function 'ignore)

;; don't show startup scene
(setq inhibit-startup-screen t)

;; hide the toolbar
(tool-bar-mode -1)

;; hide the scroll bar
(scroll-bar-mode -1)

;; show matching parens
(show-paren-mode)

;; highlight the current line
(global-hl-line-mode t)

;; replace selected text on type
(delete-selection-mode +1)

;; show trailing whitespaces
(setq show-trailing-whitespace t)

;; move point to begging when double clicking
(setq mouse-select-region-move-to-beginning t)

;; drag and drop selected regiong
(setq  mouse-drag-and-drop-region t)

;; never use tabs
(setq indent-tabs-mode nil)

;; disable C-z which minimizes Emacs
(global-unset-key "\C-z")

(defun ym/open-line-below ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(global-set-key (kbd "<S-return>") 'ym/open-line-below)

(defun ym/duplicate-current-line ()
  "Duplicate the current line below."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (forward-line 1)
  (yank))

(global-set-key (kbd "C-c d") 'ym/duplicate-current-line)

;;; set the colorscheme
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold       t
        doom-themes-enable-italic     t
        nlinum-highlight-current-line t)
  (load-theme 'doom-tomorrow-night t))

;;; Clojure support
(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'"  . clojure-mode)
         ("\\.edn\\'"  . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode)))

(use-package cider
  :ensure t
  :defer t
  :config (setq cider-repl-use-pretty-printing t))

;;; Git on steroids
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package exec-path-from-shell
  :ensure t
  :defer 3
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;;; window management
(use-package eyebrowse
  :ensure t
  :config
  (setq eyebrowse-mode-line-separator " "
        eyebrowse-new-workspace       t)
  (eyebrowse-setup-opinionated-keys)
  (eyebrowse-mode t))

;;; handy tool for http requests
(use-package restclient
  :ensure t
  :mode (("\\.http\\'" . restclient-mode)))

;;; ORG mode config
(use-package org
  :ensure org-plus-contrib
  :mode (("\\.org\\'" . org-mode))
  :config
  (setq org-hide-leading-stars t
	org-ellipsis "⤵"))

(use-package org-bullets
  :ensure t
  :hook ((org-mode . (lambda () (org-bullets-mode 1)))))

(use-package org-journal
  :ensure t
  :custom
  (org-journal-dir         "~/notes/")
  (org-journal-file-format "%Y")
  :config
  (setq org-journal-file-type 'yearly
        org-journal-time-format ""
        org-journal-date-format "%A, %d %B %Y"))

;;; show funcy symbols when editing Git tracked files
(use-package git-gutter
  :ensure t
  :hook ((prog-mode . git-gutter-mode)
         (org-mode  . git-gutter-mode)
         (yaml-mode . git-gutter-mode)))

;;; Scheme support tool
(use-package geiser
  :ensure t
  :defer t
  :config
  (setq geiser-active-implementations '(guile)))

;;; Utility when editing html files
(use-package web-mode
  :ensure t
  :hook ((web-mode . (lambda () (setq web-mode-markup-indent-offset 2))))
  :config
  (setq web-mode-extra-auto-pairs '(("erb" . (("beg" "end"))))
	web-mode-enable-css-colorization          t
	web-mode-enable-current-column-highlight  t
	web-mode-enable-current-element-highlight t
	web-mode-code-indent-offset 2)
  :mode (("\\.html?\\'" . web-mode)
         ("\\.erb\\'"   . web-mode)
         ("\\.vue\\'"   . web-mode)))

;;; Markdown support
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;; YAML support
(use-package yaml-mode
  :ensure t
  :mode (("\\.yml\\'"  . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)))

(use-package ivy
  :ensure t
  :defer 1
  :bind (("C-c C-r" . ivy-resume))
  :config
  (setq ivy-use-virtual-buffers t
        ivy-format-function 'ivy-format-function-arrow
        enable-recursive-minibuffers t)
  (ivy-mode 1))

(use-package counsel
  :ensure t
  :defer 1
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
  :bind (("C-;"   . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)))

(use-package swiper
  :ensure t
  :defer 1
  :bind (("C-s" . swiper)))

;;; project utility
(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-create-missing-test-files t
	projectile-project-search-path '("~/Code")
	projectile-completion-system 'ivy)
  (projectile-mode +1))
