;; -*- coding: utf-8 -*-


(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

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

(add-to-list 'default-frame-alist '(font . "Consolas-11"))
(set-face-attribute 'default nil :font "Consolas" :height 110)

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

(load-theme 'tango-dark t)
(with-eval-after-load 'ivy
  (setq ivy-use-virtual-buffers t
        ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                (t . ivy--regex-fuzzy))
        ivy-virtual-abbreviate 'full
        counsel-find-file-ignore-regexp "\\.go\\'"
        enable-recursive-minibuffers t
        recentf-max-saved-items nil))
;; (require 'ivy-prescient)

(ivy-prescient-mode 1)

;; Emacs 24.5 config
(add-to-list 'load-path "~/.emacs.d/elisp/")

(require 'company)
;; Activer Company-mode globalement (partout, tout le temps)
(add-hook 'after-init-hook 'global-company-mode)


(with-eval-after-load 'company
  (setq company-idle-delay 0.1)         ;; Attend 0.1 seconde avant d'afficher le menu
  (setq company-minimum-prefix-length 2) ;; Alerte dès qu'on tape 2 caractères

  ;; Sources de complétion : mots du fichier + mots du projet + syntaxe C++
  (setq company-backends '((company-dabbrev-code company-keywords company-capf)
                           company-dabbrev)))



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
(global-set-key (kbd "C-u") 'vundo)
(global-set-key (kbd "C-\\") 'switch-to-buffer)
(global-set-key (kbd "C-s")  'swiper)
(global-set-key (kbd "ù")  'other-window)
(global-set-key (kbd "C-ù")  'evil-window-exchange)
(global-set-key (kbd "C-x j")  'previous-buffer)
(global-set-key (kbd "C-c C-g") 'same-window-prefix)
(global-set-key (kbd "C-x C-j") 'previous-buffer)
(global-set-key (kbd "\u00b2") 'dabbrev-expand)
(global-set-key (kbd "M-p") 'counsel-yank-pop)
(global-set-key (kbd "C-c u") 'browse-url)
(global-set-key (kbd "M-o") 'ff-find-other-file)

(menu-bar-mode -1)
(tool-bar-mode -1)
(add-to-list 'default-frame-alist '(drag-internal-border . 1))
(add-to-list 'default-frame-alist '(internal-border-width . 5))

(setq key-chord-two-keys-delay 0.3)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-mode 1)

(setq-default ispell-program-name "aspell")
 (require 'rg)
 (with-eval-after-load 'rg
   (setq rg-command-line-flags '("--hidden" "-L" "-g !*.git"))
   (setq rg-command-line-flags '("--hidden" "-L" "-g !*.svn"))
   (rg-define-search my-rg :files "everything"))

(global-set-key (kbd "C-c s") 'my-rg)

(idle-highlight-mode t)
(setq make-backup-files nil)

(setq kill-do-not-save-duplicates t)

(add-hook 'org-mode-hook (lambda nil
          (auto-fill-mode 1)
          (set-fill-column 78)))

(add-hook 'c-mode-common-hook
          (lambda () (modify-syntax-entry ?_ "w")))

; Evil Mode

(require 'evil)
(with-eval-after-load 'evil
  (evil-ex-define-cmd "x" 'evil-write)
  (evil-set-initial-state 'compilation-mode 'emacs)
  (evil-set-initial-state 'rg-mode 'normal)
  (setq evil-want-C-i-jump nil ;; retire le C-i pour tabulation
        evil-symbol-word-search t
        evil-insert-state-modes nil
        evil-motion-state-modes nil
        evil-move-cursor-back t
        evil-kill-on-visual-paste nil))

(fset 'evil-visual-update-x-selection #'ignore)

; Add custom templates
(define-skeleton insert-org-image "A meeting skeleton" nil "#+ATTR_LATEX: :width 15cm #+CAPTION: ")

(with-eval-after-load 'whitespace
  (setq whitespace-line-column nil
        whitespace-style '(face trailing lines-tail
                                space-before-tab newline
                                indentation empty space-after-tab)))

(put 'magit-clean 'disabled nil)
(yas-global-mode 1)
(global-whitespace-mode -1)

(setq display-line-numbers-type 'relative)

;;(setq tags-table-list '("/home/reym/workspace_cap4000/dev_cyber-ldap_requirements/TAGS"))
;;(setq tags-table-list '("/home/reym/workspace_cap4000/trunk/TAGS"))
(setq tags-table-list '("/home/reym/TAGS"))

;;(setq tags-file-name "/home/reym/workspace_cap4000/dev_cyber-ldap_requirements/TAGS")
;;(setq tags-file-name '("/home/reym/workspace_cap4000/trunk/TAGS"))
(setq tags-file-name '("/home/reym/TAGS"))

;; 2. Configuration globale de l'indentation (Tabulations de 4 espaces)

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil) ; nil signifie "utiliser des espaces", pas des tabulations

(defun mon-style-c-allman ()
  "Configure l'indentation pour mettre les accolades sur une nouvelle ligne."
  (c-set-style "bsd")
  (setq c-basic-offset 4)
  (setq-local tab-width 4)
  (setq-local indent-tabs-mode nil)) ;; Utilise des espaces

;; Appliquer ce style automatiquement aux fichiers C, C++ et Java
(add-hook 'c-mode-common-hook 'mon-style-c-allman)

;; 1. Ajouter le dossier de vos scripts au chemin de recherche d'Emacs
(add-to-list 'load-path "~/.emacs.d/elpa/bb-mode/")

(require 'bb-mode)
(setq auto-mode-alist (cons '("\\.bb$" . bb-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.inc$" . bb-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.bbappend$" . bb-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.bbclass$" . bb-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.conf$" . bb-mode) auto-mode-alist))

(global-display-line-numbers-mode)
 
(dolist (mode '(term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; Ouvrir les fichiers C/C++ en ISO-8859-1

(add-to-list 'auto-coding-alist '("\\.h\\'" . iso-latin-1))
(add-to-list 'auto-coding-alist '("\\.hpp\\'" . iso-latin-1))
(add-to-list 'auto-coding-alist '("\\.c\\'" . iso-latin-1))
(add-to-list 'auto-coding-alist '("\\.cc\\'" . iso-latin-1))
(add-to-list 'auto-coding-alist '("\\.cpp\\'" . iso-latin-1))

(add-hook 'c-mode-common-hook
          (lambda ()
                        (setq buffer-file-coding-system 'iso-latin-1)))


(defun my-osc52-copy (text)
  (let ((coding-system-for-write 'utf-8))
    (send-string-to-terminal
     (concat "\033]52;c;"
             (base64-encode-string
              (string-as-unibyte
               (encode-coding-string text 'utf-8))
              t)
             "\a"))))

(advice-add
 'kill-new
 :after
 (lambda (text &rest _)
   (my-osc52-copy text)))

(evil-set-initial-state 'eshell-mode 'emacs)

(add-hook 'eshell-mode-hook (lambda () (company-mode -1)))
(add-hook 'shell-mode-hook  (lambda () (company-mode -1)))
(add-hook 'comint-mode-hook (lambda () (company-mode -1)))
