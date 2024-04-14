(require 'package)
(package-initialize)

(setq column-number-mode t
      compilation-scroll-output t
      find-file-visit-truename t
      inhibit-startup-message t
      custom-file "~/.emacs.d/my-custom.el"
      vc-follow-symlinks t
      visible-bell 1
      truncate-lines t
      shell-file-name "bash"
      uniquify-buffer-name-style 'reverse
      ad-redefinition-action 'accept
      calendar-week-start-day 1
      select-enable-primary t
      select-enable-clipboard nil
      gc-cons-threshold 100000000
      dired-dwim-target t
      user-full-name "Maxime Rey"
      enable-remote-dir-locals t)

(setq-default indent-tabs-mode nil
              fill-column 78)

(set-face-attribute 'default nil :height 160)
(defalias 'yes-or-no-p 'y-or-n-p)
;; Any add to list for package-archives (to add marmalade or melpa) goes here
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(add-to-list 'package-archives 
    '("MELPA" .
      "http://melpa.org/packages/"))

(global-auto-revert-mode)
(global-font-lock-mode t)
(show-paren-mode)
;; (bink-cursor-mode)
(straight-use-package 'evil)
(evil-mode)
(straight-use-package 'ivy)
(ivy-mode)
(straight-use-package 'counsel)
(counsel-mode)
(savehist-mode)
(straight-use-package 'rg)
(straight-use-package 'magit)

(load-theme 'tango-dark t)
(with-eval-after-load 'ivy
  (setq ivy-use-virtual-buffers t
        ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                (t . ivy--regex-fuzzy))
        ivy-virtual-abbreviate 'full
        counsel-find-file-ignore-regexp "\\.go\\'"
        enable-recursive-minibuffers t
        recentf-max-saved-items nil))
(straight-use-package 'ivy-prescient)
(ivy-prescient-mode 1)
(defvar bootstrap-version)


(setq package-selected-packages '(lsp-mode yasnippet lsp-treemacs helm-lsp
    projectile hydra flycheck company avy which-key helm-xref dap-mode))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

;; sample `helm' configuration use https://github.com/emacs-helm/helm/ for details
(helm-mode)
(require 'helm-xref)
(define-key global-map [remap find-file] #'helm-find-files)
(define-key global-map [remap execute-extended-command] #'helm-M-x)
(define-key global-map [remap switch-to-buffer] #'helm-mini)

(which-key-mode)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(visual-line-mode)
(setq org-latex-packages-alist '(("margin=2cm" "geometry" nil)))
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)  ;; clangd is fast

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)
  (yas-global-mode))

(defun wsl-copy-region-to-clipboard (start end)
  "Copy region to Windows clipboard."
  (interactive "r")
  (call-process-region start end "clip.exe" nil 0))

(defun wsl-cut-region-to-clipboard (start end)
    (interactive "r")
      (call-process-region start end "clip.exe" nil 0)
        (kill-region start end))

(defun wsl-clipboard-to-string ()
    "Return Windows clipboard as string."
      (let ((coding-system-for-read 'dos))
	(substring; remove added trailing \n
		     (shell-command-to-string
		         "powershell.exe -Command Get-Clipboard") 0 -1)))

(defun wsl-paste-from-clipboard (arg)
    "Insert Windows clipboard at point. With prefix ARG, also add to kill-ring"
      (interactive "P")
        (let ((clip (wsl-clipboard-to-string)))
	  (insert clip)
	  (if arg (kill-new clip))))

(define-key global-map (kbd "C-x C-y") 'wsl-paste-from-clipboard)
(define-key global-map (kbd "C-x M-w") 'wsl-copy-region-to-clipboard)
(define-key global-map (kbd "C-x C-w") 'wsl-cut-region-to-clipboard)

(use-package lsp-mode
  :hook ((prog-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :config
  (progn
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-tramp-connection "clangd")
                      :major-modes '(c-mode c++-mode)
                      :remote? t
                      :server-id 'clangd-remote))))

(menu-bar-mode -1)
(tool-bar-mode -1) 
