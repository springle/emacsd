;;;;;;;;;;;;;;;;;;;;;
;; GLOBAL SETTINGS ;;
;;;;;;;;;;;;;;;;;;;;;

;; SET VARIABLES
(setq
    create-lockfiles nil
    confirm-kill-emacs 'y-or-n-p
    make-backup-files nil
    dired-use-ls-dired nil
    inhibit-startup-screen t)

;; SET MODES
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(line-number-mode)
(column-number-mode)

;; SET HOOKS
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; CONFIGURE PACKAGE REPOSITORIES
(require 'package)
(add-to-list 'package-archives (cons "melpa" "http://melpa.org/packages/"))
(add-to-list 'package-archives (cons "elpa" "http://elpa.gnu.org/packages/"))
(package-initialize)

;; INSTALL PACKAGES
(mapc
 (lambda (package)
   (unless (package-installed-p package)
     (progn (message "installing %s" package)
            (package-refresh-contents)
            (package-install package))))
 '(
   ag
   auto-complete
   dockerfile-mode
   evil
   evil-org
   helm
   helm-ag
   key-chord
   magit
   markdown-mode
   projectile
   undo-tree
   which-key
   yaml-mode
  ))

;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE SETTINGS ;;
;;;;;;;;;;;;;;;;;;;;;;

;; AUTO-COMPLETE
(ac-config-default)

;; EVIL
(setq evil-want-C-i-jump nil)
(evil-mode 1)
(evil-set-initial-state 'calendar-mode 'emacs)

;; KEY-CHORD
(key-chord-mode t)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

;; ORG-MODE
(setq org-agenda-files (list "~/org/"))
(setq org-log-done 'time)
(setq org-todo-keywords '((sequence "TODO(t)" "CURR(c)" "WAIT(w)" "|" "DONE(d)")))
(setq org-todo-keyword-faces
      '(("TODO" . "green")
	("CURR" . "orange")
	("WAIT" . "red")
	("DONE" . "white")))
(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-agenda-include-diary t)
(setq org-deadline-warning-days 0)
(setq org-capture-templates
 '(("t" "Todo" entry (file+headline "~/org/main.org" "Inbox")
        "* TODO %?\n %a")))

;; PROJECTILE
(require 'projectile)
(setq projectile-enable-caching t)
(projectile-global-mode)

;; UNDO-TREE
(global-undo-tree-mode 1)

;; WHICH-KEY
(which-key-mode)

;;;;;;;;;;;;
;; KEYMAP ;;
;;;;;;;;;;;;

(define-key evil-normal-state-map " " nil)

;; GENERIC
(define-key evil-normal-state-map (kbd "q") 'kill-this-buffer)
(define-key evil-normal-state-map (kbd "SPC !") 'shell-command)
(define-key evil-visual-state-map (kbd "SPC |") 'shell-command-on-region)

;; COMMON (,)
(define-key evil-normal-state-map (kbd "SPC , f") 'find-file)
(define-key evil-normal-state-map (kbd "SPC , u") 'undo-tree-visualize)
(define-key evil-normal-state-map (kbd "SPC , b") 'evil-switch-to-windows-last-buffer)
(define-key evil-normal-state-map (kbd "SPC , w") 'helm-mini)
(define-key evil-normal-state-map (kbd "SPC , s") 'helm-ag-project-root)
(define-key evil-normal-state-map (kbd "SPC , x") 'kill-this-buffer)

;; DIARY (d)
(global-set-key (kbd "C-c d")
		(lambda ()
		  (interactive)
		  (eval-buffer
		   (find-file "~/.emacs.d/diary"))))

;; FRAMES (f)
(define-key evil-normal-state-map (kbd "SPC f d") 'delete-frame)
(define-key evil-normal-state-map (kbd "SPC f n") 'make-frame-command)
(define-key evil-normal-state-map (kbd "SPC f o") 'other-frame)

;; GIT (g)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;; HELM (h)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)


;; LEDGER
(define-key evil-normal-state-map (kbd "SPC l c") 'ledger-mode-clean-buffer)

;; META (m)
(global-set-key (kbd "C-c m")
		(lambda ()
		  (interactive)
		  (eval-buffer
		   (find-file "~/.emacs.d/init.el"))))

;; ORG (o)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-switchb)
(global-set-key (kbd "C-c o o")
		(lambda ()
		  (interactive)
		  (find-file "~/org/main.org")))
(global-set-key (kbd "C-c o n")
		(lambda ()
		  (interactive)
		  (find-file "~/org/notes.org")))

;; ORG-EVIL
(add-hook 'org-mode-hook 'evil-org-mode)

;;;;;;;;;;;;
;; CUSTOM ;;
;;;;;;;;;;;;
