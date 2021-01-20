(setq user-full-name "Simon Attila Weis"
      user-mail-address "simonattilaweis@tutanota.com")
(setq calendar-latitude 47.73
      calendar-longitude 12.88)

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (setq doom-theme 'doom-dracula)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(defvar siatwe/frame-transparency '(95 . 95))
(set-frame-parameter (selected-frame) 'alpha siatwe/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,siatwe/frame-transparency))

(setq doom-font (font-spec :family "Hack" :size 18))

(setq display-line-numbers-type t)

;; Disable line numbers for specific modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq read-process-output-max (* 3072 3072))
(add-hook 'focus-out-hook #'garbage-collect)

(toggle-frame-maximized)

(global-hl-line-mode +1)

(mouse-avoidance-mode)

(setq-default fill-column 80)

(setq org-directory "~/org/")
(setq org-ellipsis " ▾")
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq calendar-week-start-day 1)
(setq org-tags-exclude-from-inheritance '("crypt"))

(defun siatwe/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . siatwe/org-mode-visual-fill))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package! evil
  :config
  (setq-default evil-kill-on-visual-paste nil)
  (setq evil-escape-key-sequence "jj"))

(use-package! evil-matchit
  :config
  (global-evil-matchit-mode 1))

(use-package! key-chord
  :config
  (setq key-chord-two-keys-delay 0.5)
  (key-chord-define evil-insert-state-map "kk" 'yas-expand)
  (key-chord-define evil-normal-state-map "ge" 'next-error)
  (key-chord-define evil-normal-state-map "gE" 'previous-error)
  (key-chord-mode 1))

(use-package! lsp-mode
  :config
  (setq lsp-idle-delay 0.500)
  (setq lsp-enable-file-watchers 1)
  (setq lsp-file-watch-threshold '99999)
  (setq lsp-ui-doc-enable nil))

(after! elfeed
  (setq elfeed-search-filter "@2-weeks-ago"))
(add-hook! 'elfeed-search-mode-hook 'elfeed-update)

(use-package! lsp-treemacs
  :config
  (lsp-treemacs-sync-mode 1))

(use-package projectile
  :config
  (setq projectile-project-search-path '("/data/55/" "/data/53/" "/data/Projects/")))

(defun minify-js-or-less ()
  (interactive)
  (save-window-excursion
    ;; LESS
    (when (string= (file-name-extension buffer-file-name) "less")
      (async-shell-command
       (concat "lessc --no-color --clean-css " (projectile-project-root) "webroot/less/main.less "  (projectile-project-root) "webroot/less/main.css")))
    ;; JS
    (when (string= (file-name-extension buffer-file-name) "js")
      (async-shell-command
       (concat "yui-compressor " (projectile-project-root) "webroot/js/main.js -o "  (projectile-project-root) "webroot/js/main.min.js")))))

(add-hook 'after-save-hook 'minify-js-or-less)

(defun eshell-add-aliases ()
  "Eshell aliases"
  (dolist (var '(("ff" "find-file $1")
                 ("55" "cd /data/55/ $*")
                 ("53" "cd /data/53/ $*")
                 ("dotfiles" "/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME $*")))
    (add-to-list 'eshell-command-aliases-list var)))

(add-hook 'eshell-post-command-hook 'eshell-add-aliases)

(map!

 :leader
 :desc "Elfeed"
 "ef" #'elfeed

 :leader
 :desc "Outline/Symbols"
 "es" #'lsp-treemacs-symbols

 :leader
 :desc "Decrypt entry"
 "ex" #'org-decrypt-entry

 :leader
 :desc "Adjust font size"
 "ea" #'+hydra/text-zoom/body

 :leader
 :desc "Maximize window"
 "em" #'maximize-window)

(load! "~/.doom.d/lisp/dndv5.el")
