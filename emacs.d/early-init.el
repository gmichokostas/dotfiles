;;; early-init.el -*- lexical-binding: t; -*-
(defvar file-name-handler-alist-old file-name-handler-alist)

(setq package-enable-at-startup nil
      file-name-handler-alist   nil
      gc-cons-threshold         most-positive-fixnum
      gc-cons-percentage        0.8
      auto-window-vscroll       nil)

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

(setq load-prefer-newer t)

;; maximize window on startup
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
