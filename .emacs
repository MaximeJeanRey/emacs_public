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
      enable-remote-dir-locals t
      default-frame-alist '((undecorated . t)))

(setq-default indent-tabs-mode nil
              fill-column 78)
(setq default-frame-alist '((undecorated . t)))
(set-face-attribute 'default nil :height 160)
(defalias 'yes-or-no-p 'y-or-n-p)
;; Any add to list for package-archives (to add marmalade or melpa) goes here

(add-to-list 'package-archives 
    '("MELPA" .
      "http://melpa.org/packages/"))

(global-auto-revert-mode)
(global-font-lock-mode 1)
(add-hook 'c-mode-hook 'font-lock-mode)
(show-paren-mode)
;; (bink-cursor-mode)
;;(straight-use-package 'evil)
(evil-mode)
;;(straight-use-package 'ivy)
(ivy-mode)
;;(straight-use-package 'counsel)
(counsel-mode)
(savehist-mode)
;;(straight-use-package 'rg)
;;(straight-use-package 'magit)

(package-install 'flycheck)

(global-flycheck-mode)


(load-theme 'tango-dark t)
(with-eval-after-load 'ivy
  (setq ivy-use-virtual-buffers t
        ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                (t . ivy--regex-fuzzy))
        ivy-virtual-abbreviate 'full
        counsel-find-file-ignore-regexp "\\.go\\'"
        enable-recursive-minibuffers t
        recentf-max-saved-items nil))
;;(straight-use-package 'ivy-prescient)
(ivy-prescient-mode 1)
(defvar bootstrap-version)



(setq package-selected-packages '(lsp-mode yasnippet lsp-treemacs helm-lsp
    projectile hydra flycheck company avy which-key helm-xref dap-mode))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

;; sample `helm' configuration use https://github.com/emacs-helm/helm/ for details
;;(helm-mode)
;;(require 'helm-xref)
;;(define-key global-map [remap find-file] #'helm-find-files)
;;(define-key global-map [remap execute-extended-command] #'helm-M-x)
;;(define-key global-map [remap switch-to-buffer] #'helm-mini)

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
(global-set-key (kbd "C-c w") 'clipboard-yank)
(global-set-key (kbd "C-SPC") 'my-rg)
(global-set-key (kbd "C-s")  'switch-to-buffer)
(global-set-key (kbd "ù")  'other-window)
(global-set-key (kbd "C-ù")  'evil-window-exchange)

(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(add-to-list 'default-frame-alist '(drag-internal-border . 1))
(add-to-list 'default-frame-alist '(internal-border-width . 5))

;;Exit insert mode by pressing j and then j quickly
(setq key-chord-two-keys-delay 0.3)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-mode 1)
(electric-pair-mode t)
(add-hook 'c-mode-common-hook
          (lambda () (modify-syntax-entry ?_ "w")))
(setq-default show-trailing-whitespace t)

;;Bind key to add semicolon to the end of current line
(global-set-key (kbd "C-;")
  (lambda ()
    (interactive)
    ;; Keep cursor motion within this block (don't move the users cursor).
    (save-excursion
      ;; Typically mapped to the "End" key.
      (call-interactively 'move-end-of-line)
      (insert ";"))))
(setq backup-directory-alist            '((".*" . "~/.Trash")))


(with-eval-after-load 'rg
  (setq rg-command-line-flags '("--hidden" "-L" "-g !*.git"))
  (rg-define-search my-rg :files "everything"))

(idle-highlight-mode 1)

(provide '.emacs)
;; .emacs ends here

