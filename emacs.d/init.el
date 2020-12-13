;;; -*- lexical-binding: t; -*-

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; setup use-package
(eval-and-compile
  (require 'package)
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("gnu"   . "https://elpa.gnu.org/packages/")
                           ("org"   . "https://orgmode.org/elpa/")))
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(eval-when-compile (require 'use-package))

;; set default font
(push '(font . "Inconsolata 16") default-frame-alist)

;; macOS config
(when (eq system-type 'darwin)
  ;; swap meta and hyper positions for macOS
  (setq mac-option-modifier   'super
        mac-command-modifier  'meta
	ns-function-modifier  'hyper
	ns-use-thin-smoothing t
	dired-use-ls-dired    nil))

;; use italics in comments
;; fix comments background issue in Org files
(custom-set-faces
 '(font-lock-comment-face   ((t (:slant italic))))
 '(font-lock-reference-face ((t (:slant italic))))
 '(hl-line                  ((t (:inherit nil :background "gray6"))))
 '(org-block                ((t (:background nil))))
 '(org-block-begin-line     ((t (:background nil))))
 '(org-block-end-line       ((t (:background nil)))))

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

;; always add a new line at the EOF
(setq require-final-newline t)

;; show funcy symbols like λ for lambads
(global-prettify-symbols-mode 1)

;; reload file when it has changed in the disk
(global-auto-revert-mode 1)

;; Start find-file etc in home directory
(setq default-directory "~/")

;; Show file's full path on frame
(setq-default frame-title-format "%b (%f)")

;; disable scratch message
(setq initial-scratch-message "")

;; please don't ring
(setq ring-bell-function 'ignore)

;; enable kill whole line with C-a C-k
(setq kill-whole-line t)

;; don't show startup scene
(setq inhibit-startup-screen t)

;; show matching parens
(setq show-paren-delay 0)
(show-paren-mode)

;; highlight the current line
(add-hook 'prog-mode-hook #'hl-line-mode)

;; replace selected text on type
(delete-selection-mode 1)

;; show trailing whitespaces
(setq-default show-trailing-whitespace t)

;; move point to begging when double clicking
(setq mouse-select-region-move-to-beginning t)

;; drag and drop selected regiong
(setq mouse-drag-and-drop-region t)

;; never use tabs
(setq-default indent-tabs-mode nil)

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

(defun ym/upcase-word (n)
  "Upcase nth words before the point."
  (interactive "p")
  (setq n (- n))
  (upcase-word n))

(global-set-key (kbd "C-c u") 'ym/upcase-word)

;;;; colorize output in compile buffer
;; don't show ANSI escape sequences in compile buffer
(require 'ansi-color)

(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point)))

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  (setq exec-path-from-shell-variables '("PATH"))
  (exec-path-from-shell-initialize))

;;; s-expression util
(use-package lispy
  :ensure t
  :hook ((clojure-mode       . lispy-mode)
         (clojurescript-mode . lipsy-mode)
         (scheme-mode        . lipsy-mode)
         (emacs-lisp-mode    . lispy-mode)))

;;; Clojure support
(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'"  . clojure-mode)
         ("\\.edn\\'"  . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode)))

(use-package elixir-mode :ensure t)

(use-package flymake
  :ensure nil
  :bind (([f8] . flymake-goto-next-error)
         ([f7] . flymake-goto-prev-error))
  :hook ((ruby-mode    . (lambda () (flymake-mode t)))
         (clojure-mode . (lambda () (flymake-mode t))))
  :config (remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake))

(use-package flymake-kondor
  :ensure t
  :hook (clojure-mode . flymake-kondor-setup))

(use-package cider
  :ensure t
  :defer t
  :config (setq cider-repl-use-pretty-printing t))

(use-package rainbow-delimiters
  :ensure t
  :hook ((clojure-mode    . rainbow-delimiters-mode)
         (cider-repl-mode . rainbow-delimiters-mode)
         (geiser-mode     . rainbow-delimiters-mode)
         (emacs-lisp-mode . rainbow-delimiters-mode)))

;;; Git on steroids
(use-package magit
  :ensure t
  :defer 1
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

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
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((scheme . t)))
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
  (setq geiser-active-implementations '(guile mit)))

;;; Utility when editing html files
(use-package web-mode
  :ensure t
  :hook ((web-mode . (lambda () (setq web-mode-markup-indent-offset 2))))
  :config
  (setq web-mode-extra-auto-pairs '(("erb" . (("beg" "end"))))
        web-mode-enable-css-colorization          t
        web-mode-enable-current-column-highlight  t
        web-mode-enable-current-element-highlight t
        web-mode-enable-auto-closing              t
        web-mode-auto-close-style                 2
        web-mode-code-indent-offset               2)
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

(use-package browse-at-remote
  :ensure t
  :defer 1)

(use-package icomplete-vertical
  :ensure t
  :demand t
  :custom
  (completion-styles '(partial-completion substring flex initials))
  (completion-category-overrides '((file (styles basic substring))))
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  :config
  (icomplete-mode)
  (icomplete-vertical-mode)
  (defun minibuffer-try-complete-and-exit ()
    (interactive)
    (minibuffer-force-complete)
    (setq-local deactivate-mark nil)
    (throw 'exit nil))
  :bind (:map icomplete-minibuffer-map
              ([return] . minibuffer-try-complete-and-exit)
              ("<down>" . icomplete-forward-completions)
              ("C-n"    . icomplete-forward-completions)
              ("<up>"   . icomplete-backward-completions)
              ("C-p"    . icomplete-backward-completions)
              ("C-v"    . icomplete-vertical-toggle)))

(use-package expand-region
  :ensure t
  :bind ("C-=" . 'er/expand-region))

(use-package vterm
  :ensure t
  :config
  (setq vterm-kill-buffer-on-exit t))

(use-package ruby-test-mode
  :ensure t
  :bind (("C-c f" . 'ruby-test-run)
         ("C-c n" . 'ruby-test-run-at-point)
         ("C-c a" . 'ruby-test-toggle-implementation-and-specification))
  :mode (("\\.rb\\'" . ruby-mode)))

(use-package sql-indent
  :ensure t
  :hook ((sql-mode . sqlind-minor-mode)))

(use-package project :ensure t)

(use-package emacs
  :ensure nil
  :config
  (setq diff-font-lock-prettify t)
  (load-theme 'wombat))
