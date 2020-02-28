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

;; setup use-package
(eval-and-compile
  (require 'package)
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")))
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
	dired-use-ls-dired    nil))

;; set default font
(add-to-list 'default-frame-alist '(font . "Menlo-15"))

;; use italics in comments
(custom-set-faces
  '(font-lock-comment-face ((t (:slant italic)))))

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

;; line numbers config
;;; show line numbers in prog mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(setq column-number-mode             t     ;; show lin-number/column combo
      display-line-numbers-grow-only t     ;; dont resize line number column when we go to smaller line number
      display-line-numbers-type 'relative) ;; line number type is relative to mark

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

;; move point to begging when double clicking
(setq mouse-select-region-move-to-beginning t)

;; drag and drop selected regiong
(setq  mouse-drag-and-drop-region t)

;; never use tabs
(setq indent-tabs-mode nil)

;; disable C-z which minimizes Emacs
(global-unset-key "\C-z")

;;; set the colorscheme
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        nlinum-highlight-current-line t)
  (load-theme 'doom-tomorrow-night t))
