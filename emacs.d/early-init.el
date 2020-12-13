;;; early-init.el -*- lexical-binding: t; -*-
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

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(defun ym/defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun ym/restore-garbage-collection-h ()
  "Defer it so that commands launched immediately after will enjoy the benefits."
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold 16777216))))

(add-hook 'minibuffer-setup-hook #'ym/defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook  #'ym/restore-garbage-collection-h)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; macOS config
(when (eq system-type 'darwin)
  ;; titlebar for macOS
  (push '(ns-appearance . dark)           default-frame-alist)
  (push '(ns-transparent-titlebar . dark) default-frame-alist))

;; hide the toolbar
(tool-bar-mode -1)

;; hide the scroll bar
(scroll-bar-mode -1)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Initialise installed packages
(setq package-enable-at-startup t)

;; Allow loading from the package cache.
(setq package-quickstart t)

;; maximize window on startup
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
