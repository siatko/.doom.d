(setq user-full-name "Simon Attila Weis"
      user-mail-address "me@siatwe.dev")
(setq calendar-latitude 47.73
      calendar-longitude 12.88)

(setq doom-font (font-spec :family "JetBrainsMono" :size 20))
(setq doom-variable-pitch-font (font-spec :family "Ubuntu" :size 20))
(use-package! mixed-pitch
  :hook
  (text-mode . mixed-pitch-mode))

(setq doom-theme 'doom-dracula)
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

(setq read-process-output-max (* 3072 3072))
(add-hook 'focus-out-hook #'garbage-collect)

(toggle-frame-maximized)

(global-hl-line-mode +1)

;;(mouse-avoidance-mode)

(setq-default fill-column 80)

(setq org-directory "~/.org")
(setq org-ellipsis " ▾")
(require 'org-web-tools)
(require 'org-crypt)
(after! org
  (add-to-list 'org-modules 'org-habit))
(org-crypt-use-before-save-magic)
(setq calendar-week-start-day 1)
(setq org-tags-exclude-from-inheritance '("crypt"))

(defun siatwe/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  ;;:hook (org-mode . siatwe/org-mode-visual-fill)
  )

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

(evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history       )
;;(eshell-git-prompt-use-theme 'powerline)

(use-package! evil
  :config
  (setq-default evil-kill-on-visual-paste nil)
  (setq evil-escape-key-sequence "jj")
  (setq evil-escape-delay 0.6))

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

;;(after! elfeed
;;  (setq elfeed-search-filter "@2-weeks-ago"))
;;(add-hook! 'elfeed-search-mode-hook 'elfeed-update)
;;(require 'elfeed)
;;(defun elfeed-v-mpv (url)
;;  "Watch a video from URL in MPV"
;;  (async-shell-command (format "mpv %s" (string-replace "=" "\\=" (string-replace "?" "\\?" url)))))
;;
;;(defun elfeed-view-mpv (&optional use-generic-p)
;;  "Youtube-feed link"
;;  (interactive "P")
;;  (let ((entries (elfeed-search-selected)))
;;    (cl-loop for entry in entries
;;             do (elfeed-untag entry 'unread)
;;             when (elfeed-entry-link entry)
;;             do (elfeed-v-mpv it))
;;    (mapc #'elfeed-search-update-entry entries)
;;    (unless (use-region-p) (forward-line))))
;;(define-key elfeed-search-mode-map (kbd "v") 'elfeed-view-mpv)

(use-package! lsp-treemacs
  :config
  (lsp-treemacs-sync-mode 1))

(use-package! projectile
  :config
  (setq projectile-project-search-path '("/data/55/" "/data/53/" "/data/Projects/")))

(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package! git-auto-commit-mode
  :config
  (setq-default gac-automatically-push-p t)
  (setq-default gac-automatically-add-new-files-p t))

(use-package! org-mind-map
  :init
  (require 'ox-org)
  :ensure t
  ;; Uncomment the below if 'ensure-system-packages` is installed
  ;;:ensure-system-package (gvgen . graphviz)
  :config
  (setq org-mind-map-engine "dot")       ; Default. Directed Graph
  ;; (setq org-mind-map-engine "neato")  ; Undirected Spring Graph
  ;; (setq org-mind-map-engine "twopi")  ; Radial Layout
  ;; (setq org-mind-map-engine "fdp")    ; Undirected Spring Force-Directed
  ;; (setq org-mind-map-engine "sfdp")   ; Multiscale version of fdp for the layout of large graphs
  ;; (setq org-mind-map-engine "twopi")  ; Radial layouts
  ;; (setq org-mind-map-engine "circo")  ; Circular Layout
  )

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
                 ("ytd" "youtube-dl --extract-audio --audio-format mp3 -i -o '%(title)s.%(ext)s' $1")
                 ("dotfiles" "/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME $*")))
    (add-to-list 'eshell-command-aliases-list var)))

(add-hook 'eshell-post-command-hook 'eshell-add-aliases)

(defun youtube-dl-via-id-to-mp3 (url)
  "Turn a youtube URL into a mp3 file. Works also for playlists."
  (interactive "sURL: ")
  (save-match-data
    (if (string-match
         "\\(?:\\.be/\\|v=\\|v%3D\\|^\\)\\([-_a-zA-Z0-9]\\{11\\}\\)" url)
        (async-shell-command
         (concat "youtube-dl --extract-audio --audio-format mp3 -i -o '%(title)s.%(ext)s' "
                 (match-string 1 url)))
      (message "This does not seem to be a valid Youtube URL."))))

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
 :desc "Launch app-launcher"
 "d" #'app-launcher-run-app

 :leader
 :desc "Maximize window"
 "em" #'maximize-window)

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(setq +ivy-buffer-preview t)

(defun siatwe/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name doom-private-dir))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

;;(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'siatwe/org-babel-tangle-config)))
