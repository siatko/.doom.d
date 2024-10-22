(setq user-full-name "Simon Kosina"
      user-mail-address "siatko@proton.me")
(setq calendar-latitude 47.73
      calendar-longitude 12.88)

(setq doom-font (font-spec :family "JetBrainsMono" :size 20))

(setq doom-theme 'modus-vivendi)
(doom-themes-visual-bell-config)
(defvar siatwe/frame-transparency '(100 . 100))
(set-frame-parameter (selected-frame) 'alpha siatwe/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,siatwe/frame-transparency))
(toggle-frame-maximized)

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

(use-package! evil
  :config
  (setq-default evil-kill-on-visual-paste nil))

(use-package! evil-matchit
  :config
  (global-evil-matchit-mode 1))

(use-package! git-auto-commit-mode
  :config
  (setq-default gac-automatically-push-p t)
  (setq-default gac-automatically-add-new-files-p t)

  ;; Advise the `gac-push' function to pull before pushing and refresh the buffer
  (defun gac-pull-before-push (&rest _args)
    "Pull from the current repo before pushing and refresh the buffer."
    (let ((current-file (buffer-file-name)))
      (shell-command "git pull")
      (when current-file
        (with-current-buffer (find-buffer-visiting current-file)
          (revert-buffer t t t)))))
  (advice-add 'gac-push :before #'gac-pull-before-push))


(setq org-log-done 'time)
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-start-day "-31d")
(setq org-agenda-span 100)

(custom-set-variables
 '(org-agenda-start-day "-31d")
 '(org-agenda-span 100))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq org-agenda-start-day "-31d")
            (setq org-agenda-span 100)))

(setq org-agenda-custom-commands
      '(("w" "Agenda for the last 100 days"
         agenda ""
         ((org-agenda-start-day "-31d")
          (org-agenda-span 100)))
        ("f" "Future dates with entries"
         agenda ""
         ((org-agenda-time-grid nil)
          (org-agenda-entry-types '(:timestamp :sexp))
          (org-agenda-span 'year)
          (org-agenda-start-day "-31d")
          (org-agenda-span 100)
          (org-agenda-show-all-dates nil)
          (org-agenda-overriding-header "Future dates with entries")))))

(setq org-directory "~/.org")
(require 'org-crypt)

(org-crypt-use-before-save-magic)
(setq calendar-week-start-day 1)
(setq org-tags-exclude-from-inheritance '("crypt"))

(setq org-agenda-prefix-format '((agenda . "%i %-12:c%?-12t%b% s")
                                 (todo . " %i %-12:c")
                                 (tags . " %i %-12:c")
                                 (search . " %i %-12:c")))


(setq org-agenda-format-date (lambda (date) (concat "\n"
                                                    (make-string (window-width) 9472)
                                                    "\n"
                                                    (org-agenda-format-date-aligned date))))
(setq system-time-locale "C")
(setq projectile-enable-caching nil)
(setq browse-url-browser-function 'browse-url-xdg-open)
