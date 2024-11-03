(setq user-full-name "Simon Kosina"
      user-mail-address "siatko@proton.me")

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

(use-package! denote
  :config
  (setq denote-directory (expand-file-name "~/Documents/Org")))



(setq org-agenda-start-on-weekday nil)
(setq org-agenda-start-day "-31d")
(setq org-agenda-span 100)
(setq calendar-week-start-day 1)
(setq system-time-locale "C")
(setq projectile-enable-caching nil)
(setq browse-url-browser-function 'browse-url-xdg-open)
(setq org-directory "~/Documents/Org")

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

(setq org-agenda-prefix-format '((agenda . "%i %?-12t%b% s")
                                 (todo . " %i")
                                 (tags . " %i")
                                 (search . " %i")))

(setq org-agenda-format-date (lambda (date) (concat "\n"
                                                    (make-string (window-width) 9472)
                                                    "\n"
                                                    (org-agenda-format-date-aligned date))))

(defun dired-open-file-externally ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (message "Opening %s..." file)
    (call-process "termux-open" nil 0 nil file)
    (message "Opening %s done" file)))

(global-set-key (kbd "C-c o") 'dired-open-file)
