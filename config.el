(setq user-full-name "Simon Weis"
      user-mail-address "simonattilaweis@tutanota.com")
(setq calendar-latitude 47.73
      calendar-longitude 12.88)

(setq doom-font (font-spec :family "JetBrainsMono" :size 20))

(setq doom-theme 'modus-vivendi)
(doom-themes-visual-bell-config)
(defvar siatwe/frame-transparency '(100 . 100))
(set-frame-parameter (selected-frame) 'alpha siatwe/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,siatwe/frame-transparency))

(setq display-line-numbers-type t)

;; Disable line numbers for specific modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(global-hl-line-mode +1)

(setq-default fill-column 80)

(setq org-directory "~/.org")
(setq org-ellipsis " ▾")
(require 'org-crypt)
(after! org
  (add-to-list 'org-modules 'org-habit))
(org-crypt-use-before-save-magic)
(setq calendar-week-start-day 1)
(setq org-tags-exclude-from-inheritance '("crypt"))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(setq org-agenda-prefix-format '((agenda . "%i %-12:c%?-12t%b% s")
                                 (todo . " %i %-12:c")
                                 (tags . " %i %-12:c")
                                 (search . " %i %-12:c")))


(setq org-agenda-format-date (lambda (date) (concat "\n"
                                                    (make-string (window-width) 9472)
                                                    "\n"
                                                    (org-agenda-format-date-aligned date))))

(setq org-agenda-span 100
      org-agenda-start-on-weekday nil
      org-agenda-start-day "-31d")
(setq org-log-done 'time)

(use-package! evil
  :config
  (setq-default evil-kill-on-visual-paste nil))

(use-package! evil-matchit
  :config
  (global-evil-matchit-mode 1))

(use-package! git-auto-commit-mode
  :config
  (setq-default gac-automatically-push-p t)
  (setq-default gac-automatically-add-new-files-p t))
