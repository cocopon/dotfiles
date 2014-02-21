(require 'cl)

; Startup
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")

; Language
(set-language-environment "Japanese")
(set-locale-environment nil)

; Encoding
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

; Key
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-m") 'newline-and-indent)
(global-set-key (kbd "<C-tab>") 'other-window)
(global-set-key (kbd "<C-S-tab>") (lambda () (interactive) (other-window -1)))

; Backup
(setq delete-auto-save-files t)
(setq make-backup-files nil)

; Parenthesis
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)

; cua-mode
(cua-mode t)
(setq cua-enable-cua-keys nil)

; iswitchb-mode
(iswitchb-mode t)

; Misc
(fset 'yes-or-no-p 'y-or-n-p)
(setq scroll-margin 0
      scroll-step 1)
(setq kill-whole-line t)
(setq vc-follow-symlinks t)
(setq-default show-trailing-whitespace t)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

; package
(when (require 'package nil 'noerror)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (package-initialize)
  (add-to-list 'load-path "~/.emacs.d/site-lisp"))

(defvar my-packages
  '(
    auto-complete
    csv-mode
    markdown-mode
    popwin
    undo-tree
    zlc
    ))

(defun my-install-packages ()
  (interactive)
  (let ((packages (loop for package in my-packages
			     when (not (package-installed-p package))
			     collect package)))
    (when packages
      (package-refresh-contents)
      (dolist (package packages)
	(package-install package)))))

; auto-complete
(when (require 'auto-complete nil 'noerror)
  (global-auto-complete-mode t)
  (require 'auto-complete-config)
  (ac-config-default)
  (setq ac-use-menu-map t)
  (define-key ac-menu-map (kbd "C-n") 'ac-next)
  (define-key ac-menu-map (kbd "C-p") 'ac-previous))

; linum
(when (require 'linum nil 'noerror)
  (global-linum-mode t)
  (setq linum-format "%4d"))

; popwin
(when (require 'popwin nil 'noerror)
  (setq display-buffer-function 'popwin:display-buffer))

; undo-tree
(when (require 'undo-tree nil 'noerror)
  (global-undo-tree-mode t)
  (global-set-key (kbd "M-/") 'undo-tree-redo)
  (global-set-key (kbd "M-_") 'undo-tree-redo))

; zlc
(when (require 'zlc nil 'noerror)
  (zlc-mode t)
  (define-key minibuffer-local-map (kbd "C-n") 'zlc-select-next)
  (define-key minibuffer-local-map (kbd "C-p") 'zlc-select-previous))

; Gui
(when (display-graphic-p)
  ; Bars
  (menu-bar-mode -1)
  (tool-bar-mode -1)

  ; Theme
  (load-theme 'misterioso t)

  ; Font
  (set-face-attribute 'default nil
		      :family "Ricty"
		      :height 135)
  (set-fontset-font nil 'japanese-jisx0208
		    (font-spec :family "Ricty"))

  ; Misc
  (set-face-attribute 'mode-line nil :box nil))

; Local Settings
(if (file-exists-p "~/.emacs.d/.emacs_local")
    (load-file "~/.emacs.d/.emacs_local"))
