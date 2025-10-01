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
      gc-cons-threshold 100000000
      dired-dwim-target t
      user-full-name "Maxime Rey"
      enable-remote-dir-locals t
      default-frame-alist '((undecorated . t)))

(add-to-list 'load-path "~/.emacs.d/elisp/")

(setq default-frame-alist '((undecorated . t)))
(set-face-attribute 'default nil :height 130)
(defalias 'yes-or-no-p 'y-or-n-p)
;; Any add to list for package-archives (to add marmalade or melpa) goes here

(add-to-list 'package-archives 
    '("MELPA" .
      "http://melpa.org/packages/"))

(global-auto-revert-mode)
(add-hook 'c-mode-hook 'font-lock-mode)
(show-paren-mode)
(evil-mode)
(ivy-mode)
(counsel-mode)


(global-flycheck-mode)

(load-theme 'tango t)
(with-eval-after-load 'ivy
  (setq ivy-use-virtual-buffers t
        ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                (t . ivy--regex-fuzzy))
        ivy-virtual-abbreviate 'full
        counsel-find-file-ignore-regexp "\\.go\\'"
        enable-recursive-minibuffers t
        recentf-max-saved-items nil))
(require 'ivy-prescient)
(ivy-prescient-mode 1)

;; Emacs 24.5 config
(add-to-list 'load-path "~/.emacs.d/elisp/")

;; OpenBSD KNF for C/C++
;(require 'openbsd-knf-style)
;(c-add-style "OpenBSD" openbsd-knf-style)

(setq package-selected-packages '(lsp-mode yasnippet lsp-treemacs helm-lsp
    projectile hydra flycheck company avy which-key helm-xref dap-mode))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))
(which-key-mode)
(add-hook 'c-mode-hook (lambda ()
			 (whitespace-mode nil)
			 (c-set-style "gnu")))
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

(with-eval-after-load 'lsp

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (yas-global-mode))
  (require 'lsp-ui-flycheck)
  (setq lsp-prefer-flymake nil
        lsp-ui-sideline-enable nil)
  (add-hook 'lsp-after-open-hook
            (lambda ()
              (lsp-ui-flycheck-enable 1)))
  (flycheck-add-next-checker 'lsp-ui 'c/c++-googlelint)
  (lsp-register-client
   (make-lsp-client :new-connection
                    (lsp-tramp-connection "pyls")
                    :major-modes '(python-mode)
                    :remote? t
                    :server-id 'pyls-remote)))

(global-set-key (kbd "C-c w") 'clipboard-yank)
(global-set-key (kbd "C-*") 'switch-to-buffer)
(global-set-key (kbd "C-s")  'swiper)
(global-set-key (kbd "ù")  'other-window)
(global-set-key (kbd "C-ù")  'evil-window-exchange)
(global-set-key (kbd "C-x j")  'previous-buffer)
(global-set-key (kbd "C-c C-g") 'same-window-prefix)
(global-set-key (kbd "C-x C-j") 'previous-buffer)
(global-set-key (kbd "²") 'dabbrev-expand)
(global-set-key (kbd "M-p") 'counsel-yank-pop)
(global-set-key (kbd "C-c u") 'browse-url)

(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(add-to-list 'default-frame-alist '(drag-internal-border . 1))
(add-to-list 'default-frame-alist '(internal-border-width . 5))

(global-unset-key (kbd "C-\\"))

;;Exit insert mode by pressing j and then k quickly
(setq key-chord-two-keys-delay 0.3)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-mode 1)

;(electric-pair-mode t)

;; (setq-default show-trailing-whitespace t)

(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

(setq-default ispell-program-name "aspell")

(require 'rg)
(with-eval-after-load 'rg
  (setq rg-command-line-flags '("--hidden" "-L" "-g !*.git"))
  (rg-define-search my-rg :files "everything"))


(global-set-key (kbd "C-c s") 'rg)

(idle-highlight-mode t)
(setq make-backup-files nil)

(add-to-list 'load-path "~/.emacs.d/bb-mode")
(load "bb-mode.el")

; Kill
(setq kill-do-not-save-duplicates t)

; Org Mode
(add-hook 'org-mode-hook (lambda nil
          (auto-fill-mode 1)
          (set-fill-column 78)))


(add-hook 'c-mode-common-hook
          (lambda () (modify-syntax-entry ?_ "w")))

; Evil Mode
(require 'evil)
(with-eval-after-load 'evil
  (evil-ex-define-cmd "x" 'evil-write)  ;; Redéfinit :x pour fonctionner comme :w
  (evil-set-initial-state 'compilation-mode 'emacs)
  (evil-set-initial-state 'rg-mode 'normal)
  (setq evil-want-C-i-jump nil ;; retire le C-i pour tabulation
        evil-symbol-word-search t
        evil-insert-state-modes nil
        evil-motion-state-modes nil
        evil-move-cursor-back t
        evil-kill-on-visual-paste nil))

(fset 'evil-visual-update-x-selection #'ignore)


(with-eval-after-load 'sgml-mode
  (define-key sgml-mode-map (kbd "ù") nil));; Redéfinit :x pour fonctionner comme :w

; Add custom templates
(define-skeleton insert-org-image
  "A meeting skeleton" nil
  "#+ATTR_LATEX: :width 15cm
#+CAPTION: ")

(with-eval-after-load 'whitespace
  (setq whitespace-line-column nil
        whitespace-style '(face trailing lines-tail
                                space-before-tab newline
                                indentation empty space-after-tab)))

(put 'magit-clean 'disabled nil)

;;gnus
;; (with-eval-after-load 'smtpmail
  ;; (setq smtpmail-stream-type 'starttls
        ;; smtpmail-smtp-service 587))
  ;; (setq gnus-select-method
      ;; '(nnimap "imap.gmail.com")
      ;; gnus-parameters
      ;; '((".*"
         ;; (posting-style
          ;; (address "maximejeanrey@gmail.com")
          ;; ("X-Message-SMTP-Method" "smtp smtp.gmail.com 587")
          ;; (gcc nil)))))
;; (setq sendmail-program "/usr/bin/msmtp")

(with-eval-after-load 'sendmail
  (setq user-mail-address "maximejeanrey@gmail.com")
  (setq user-full-name "Maxime Rey"))

;; (with-eval-after-load 'simple
  ;; (setq mail-user-agent 'gnus-user-agent))

;; (setq smtpmail-default-smtp-server "smtp.gmail.com")
;; (setq smtpmail-smtp-server "smtp.gmail.com")
;; (setq smtpmail-smtp-service 587)
;; (setq smtpmail-debug-info t)
;; (setq message-send-mail-function 'smtpmail-send-it )

;; Configuration de la méthode de sélection pour utiliser Gmail via IMAP
(setq gnus-select-method
      '(nnimap "gmail"
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port 993)
               (nnimap-stream ssl)))

;; Assurez-vous que Gnus est chargé
(require 'gnus)
;(require 'org-download)


;; Configurer msmtp comme programme d'envoi
(setq send-mail-function 'sendmail-send-it)

;; Spécifiez le chemin vers le binaire msmtp
(setq sendmail-program "/usr/bin/msmtp")

;; Définir le fichier de configuration msmtp
(setq message-send-mail-function 'sendmail-send-it)
(setq sendmail-coding-system 'utf-8)
(setq mail-specify-envelope-from t)
(setq mail-envelope-from 'header)

;; Optionnel : si vous voulez que Gnus utilise votre fichier msmtprc
(setq msmtp-program "msmtp")
(setq msmtp-queue-incoming-mail nil) ;; Ne pas utiliser la file d'attente pour les mails sortants
(setq gnus-permanently-visible-groups ".*")
(setq gnus-fetch-old-headers 'always)
(setq gnus-keep-backlog 'all)
(setq gnus-read-active-file 'some)


(yas-global-mode 1)
(global-whitespace-mode -1)

(setq display-line-numbers-type 'relative)
