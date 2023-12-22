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
(blink-cursor-mode -1)
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
