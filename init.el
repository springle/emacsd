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
   evil
   helm
   helm-ag
   key-chord
   ledger-mode
   magit
   projectile
   spacemacs-theme
   undo-tree
   which-key
   yaml-mode
  ))

;;;;;;;;;;;;
;; PYTHON ;;
;;;;;;;;;;;;

;; Configure flymake for Python
(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "epylint" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))

;; Set as a minor mode for Python
(add-hook 'python-mode-hook '(lambda () (flymake-mode)))

;; Configure to wait a bit longer after edits before starting
(setq-default flymake-no-changes-timeout '1)

;; Keymaps to navigate to the errors
(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-cn" 'flymake-goto-next-error)))
(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-cp" 'flymake-goto-prev-error)))

;; To avoid having to mouse hover for the error message, these functions make flymake error messages
;; appear in the minibuffer
(defun show-fly-err-at-point ()
  "If the cursor is sitting on a flymake error, display the message in the minibuffer"
  (require 'cl)
  (interactive)
  (let ((line-no (line-number-at-pos)))
    (dolist (elem flymake-err-info)
      (if (eq (car elem) line-no)
      (let ((err (car (second elem))))
        (message "%s" (flymake-ler-text err)))))))

(add-hook 'post-command-hook 'show-fly-err-at-point)

;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE SETTINGS ;;
;;;;;;;;;;;;;;;;;;;;;;

;; AUTO-COMPLETE
(ac-config-default)

;; EVIL
(setq evil-want-C-i-jump nil)
(evil-mode 1)

;; HELM
(global-set-key (kbd "M-x") 'helm-M-x)

;; KEY-CHORD
(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

;; LEDGER
(autoload 'ledger-mode "ledger-mode" "A major mode for Ledger" t)
(add-to-list 'load-path (expand-file-name "/path/to/ledger/source/lisp/"))
(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))

;; ORG-MODE
(setq org-agenda-files (list "~/org/"))
(setq org-log-done 'time)
(setq org-log-done 'note)
(setq org-todo-keywords '((sequence "TODO" "WAITING" "|" "DONE")))

;; UNDO-TREE
(global-undo-tree-mode 1)

;; WHICH-KEY
(which-key-mode)

;;;;;;;;;;;;;;;
;; FUNCTIONS ;;
;;;;;;;;;;;;;;;

(defun springle-split (f1 f2)
  (find-file f1)
  (split-window-horizontally)
  (find-file-other-window f2))

(defun springle-meta-edit ()
  (interactive)
  (switch-to-buffer (find-file "~/.emacs.d/init.el")))

(defun springle-meta-reload ()
  (interactive)
  (eval-buffer (find-file "~/.emacs.d/init.el")))

(defun springle-org-main ()
  (interactive)
  (eval-buffer (find-file "~/org/main.org")))

;;;;;;;;;;;;;;;;;;
;; KEYMAP (SPC) ;;
;;;;;;;;;;;;;;;;;;

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

;; BUFFERS (b)
(define-key evil-normal-state-map (kbd "SPC b b") 'helm-mini)
(define-key evil-normal-state-map (kbd "SPC b d") 'kill-buffer)

;; FRAMES (f)
(define-key evil-normal-state-map (kbd "SPC f d") 'delete-frame)
(define-key evil-normal-state-map (kbd "SPC f n") 'make-frame-command)
(define-key evil-normal-state-map (kbd "SPC f o") 'other-frame)

;; GIT (g)
(define-key evil-normal-state-map (kbd "SPC g s") 'magit-status)
(define-key evil-normal-state-map (kbd "SPC g d") 'magit-dispatch-popup)

;; HELP (h)
(define-key evil-normal-state-map (kbd "SPC h k") 'describe-key)
(define-key evil-normal-state-map (kbd "SPC h i") 'info)

;; LEDGER
(define-key evil-normal-state-map (kbd "SPC l c") 'ledger-mode-clean-buffer)

;; META (m)
(define-key evil-normal-state-map (kbd "SPC m e") 'springle-meta-edit)
(define-key evil-normal-state-map (kbd "SPC m r") 'springle-meta-reload)

;; ORG (o)
(define-key evil-normal-state-map (kbd "SPC o l") 'org-store-link)
(define-key evil-normal-state-map (kbd "SPC o a") 'org-agenda)
(define-key evil-normal-state-map (kbd "SPC o c") 'org-capture)
(define-key evil-normal-state-map (kbd "SPC o b") 'org-switchb)
(define-key evil-normal-state-map (kbd "SPC o o") 'springle-org-main)

;; PROJECTS (p)
(define-key evil-normal-state-map (kbd "SPC p f") 'projectile-find-file)
(define-key evil-normal-state-map (kbd "SPC p d") 'projectile-discover-projects-in-directory)
(define-key evil-normal-state-map (kbd "SPC p p") 'projectile-switch-project)
(define-key evil-normal-state-map (kbd "SPC p b d") 'bookmark-delete)
(define-key evil-normal-state-map (kbd "SPC p b l") 'list-bookmarks)
(define-key evil-normal-state-map (kbd "SPC p b n") 'bookmark-set)
(define-key evil-normal-state-map (kbd "SPC p b o") 'bookmark-jump)

;; EXECUTE (x)
(define-key evil-normal-state-map (kbd "SPC x") 'helm-M-x)
(define-key evil-visual-state-map (kbd "SPC x") 'helm-M-x)

;; WINDOWS (w)
(define-key evil-normal-state-map (kbd "SPC w o") 'delete-other-windows)
(define-key evil-normal-state-map (kbd "SPC w d") 'delete-window)
(define-key evil-normal-state-map (kbd "SPC w h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "SPC w j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "SPC w k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "SPC w l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "SPC w s") 'evil-window-split)
(define-key evil-normal-state-map (kbd "SPC w v") 'evil-window-vsplit)
(define-key evil-normal-state-map (kbd "SPC w e") 'evil-window-rotate-downwards)
(define-key evil-normal-state-map (kbd "SPC w r") 'evil-window-rotate-upwards)

;;;;;;;;;;;;
;; CUSTOM ;;
;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(custom-enabled-themes (quote (spacemacs-dark)))
 '(custom-safe-themes
   (quote
    ("7527f3308a83721f9b6d50a36698baaedc79ded9f6d5bd4e9a28a22ab13b3cb1" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(line-number-mode nil)
 '(package-selected-packages
   (quote
    (nord-theme spacemacs-theme projectile ag yaml-mode org-download ledger-mode key-chord magit helm-ag evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
