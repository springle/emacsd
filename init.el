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
   auto-complete
   evil
   helm
   helm-ag
   key-chord
   ledger-mode
   magit
   undo-tree
   which-key
  ))

;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE SETTINGS ;;
;;;;;;;;;;;;;;;;;;;;;;

;; AUTO-COMPLETE
(ac-config-default)

;; EVIL
(evil-mode 1)

;; HELM
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-d") 'helm-browse-project)

;; HELM-AG
(global-set-key (kbd "C-x C-a g") 'helm-ag)

;; KEY-CHORD
(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

;; LEDGER
(autoload 'ledger-mode "ledger-mode" "A major mode for Ledger" t)
(add-to-list 'load-path
             (expand-file-name "/path/to/ledger/source/lisp/"))
(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))

;; MAGIT
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;; ORG-MODE
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)
;; (setq org-agenda-files (list "~/Dropbox/org/"))

;; UNDO-TREE
(global-undo-tree-mode 1)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'undo-tree-redo)

;;;;;;;;;;;;;;;
;; FUNCTIONS ;;
;;;;;;;;;;;;;;;

(defun springle-meta-edit ()
  (interactive)
  (switch-to-buffer (find-file "~/.emacs.d/init.el")))

(defun springle-meta-reload ()
  (interactive)
  (eval-buffer (find-file "~/.emacs.d/init.el")))

;;;;;;;;;;;;
;; KEYMAP ;;
;;;;;;;;;;;;

;; GENERAL
(define-key evil-normal-state-map " " nil)

;; GIT
(define-key evil-normal-state-map (kbd "SPC g") 'magit-dispatch-popup)

;; FRAMES
(define-key evil-normal-state-map (kbd "SPC f o") 'other-frame)

;; HELM
(define-key evil-normal-state-map (kbd "SPC b") 'helm-mini)
(define-key evil-normal-state-map (kbd "SPC f f") 'helm-find-files)
(define-key evil-normal-state-map (kbd "SPC a g") 'helm-ag)

;; META
(define-key evil-normal-state-map (kbd "SPC m e") 'springle-meta-edit)
(define-key evil-normal-state-map (kbd "SPC m r") 'springle-meta-reload)

;; UNDO
(define-key evil-normal-state-map (kbd "SPC u") 'undo)
(define-key evil-normal-state-map (kbd "SPC r") 'undo-tree-redo)

;; WINDOWS
(define-key evil-normal-state-map (kbd "SPC h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "SPC j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "SPC k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "SPC l") 'evil-window-right)

;; WHICH-KEY
(which-key-mode)

;;;;;;;;;;;;
;; CUSTOM ;;
;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (leuven)))
 '(package-selected-packages (quote (ledger-mode key-chord magit helm-ag evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
