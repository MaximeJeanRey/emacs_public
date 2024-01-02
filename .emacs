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

(defalias 'yes-or-no-p 'y-or-n-p)

;; Any add to list for package-archives (to add marmalade or melpa) goes here
(add-to-list 'package-archives 
    '("MELPA" .
      "http://melpa.org/packages/"))

(global-auto-revert-mode)
(global-font-lock-mode t)
(show-paren-mode)
;; (bink-cursor-mode)
(evil-mode)
(ivy-mode)
(counsel-mode)
(savehist-mode)

(with-eval-after-load 'ivy
  (setq ivy-use-virtual-buffers t
        ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                (t . ivy--regex-fuzzy))
        ivy-virtual-abbreviate 'full
        counsel-find-file-ignore-regexp "\\.go\\'"
        enable-recursive-minibuffers t
        recentf-max-saved-items nil))


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









