;; ===========================
;; Emacs Python IDE config (Ruff + Treemacs + Magit)
;; ===========================
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; ---------------------------
;; Basic UI tweaks
;; ---------------------------
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(global-display-line-numbers-mode t)
(show-paren-mode 1)
(setq inhibit-startup-message t)
(set-face-attribute 'default nil :height 160) ;; 16pt (160)

;; ---------------------------
;; Temporary Files & Backups (The Fix)
;; ---------------------------
(defvar my-backup-dir (concat user-emacs-directory "backups/"))
(defvar my-auto-save-dir (concat user-emacs-directory "auto-save/"))

(dolist (dir (list my-backup-dir my-auto-save-dir))
  (unless (file-exists-p dir)
    (make-directory dir t)))

(setq backup-directory-alist `(("." . ,my-backup-dir))
      make-backup-files t
      backup-by-copying t
      version-control t
      kept-new-versions 5
      delete-old-versions t)

(setq auto-save-file-name-transforms `((".*" ,my-auto-save-dir t))
      auto-save-list-file-prefix (concat my-auto-save-dir ".saves-")
      auto-save-default t)

(if (boundp 'lock-file-name-transforms)
    (setq lock-file-name-transforms `((".*" ,my-auto-save-dir t)))
  (setq create-lockfiles nil))

(defun my-clean-old-temporary-files ()
  "Удаляет временные файлы старше 24 часов."
  (let ((day-in-seconds (* 24 60 60))
        (current-time (float-time (current-time))))
    (dolist (dir (list my-backup-dir my-auto-save-dir))
      (when (file-exists-p dir)
        (dolist (file (directory-files dir t))
          (unless (file-directory-p file)
            (let ((file-mtime (float-time (nth 5 (file-attributes file)))))
              (when (> (- current-time file-mtime) day-in-seconds)
                (delete-file file)))))))))
(add-hook 'emacs-startup-hook #'my-clean-old-temporary-files)

;; ---------------------------
;; Packages
;; ---------------------------
(use-package ivy :config (ivy-mode 1))
(use-package counsel)
(use-package swiper)
(use-package projectile :config (projectile-mode +1))
(use-package which-key :config (which-key-mode))
(use-package magit :bind ("C-x g" . magit-status))
(use-package exec-path-from-shell 
  :if (memq window-system '(mac ns x))
  :config (exec-path-from-shell-initialize))

;; ---------------------------
;; Python & LSP
;; ---------------------------
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :config (setq python-indent-offset 4))

(use-package lsp-mode
  :hook (python-mode . lsp-deferred)
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-prefer-flymake nil
        lsp-enable-snippet t))

(use-package lsp-ui :commands lsp-ui-mode)

(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0))

(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (flycheck-define-checker python-ruff
    "A Python syntax and style checker using Ruff."
    :command ("ruff" "check" "--stdin-filename" source-inplace "-")
    :error-patterns
    ((error line-start (file-name) ":" line ":" column ": " (message) line-end))
    :modes python-mode)
  (add-to-list 'flycheck-checkers 'python-ruff))

(use-package blacken
  :hook (python-mode . blacken-mode))

(use-package pyvenv
  :config
  (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
  (pyvenv-mode 1)
  (add-hook 'python-mode-hook
            (lambda ()
              (let ((venv-path (locate-dominating-file default-directory ".venv")))
                (when venv-path
                  (pyvenv-activate (expand-file-name ".venv" venv-path)))))))

;; ---------------------------
;; Debugger (DAP)
;; ---------------------------
(use-package dap-mode
  :after lsp-mode
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-auto-configure-mode)
  (require 'dap-python)
  (setq dap-python-executable "python3"))

;; ---------------------------
;; UI: Treemacs & Theme
;; ---------------------------
(use-package treemacs
  :after projectile
  :bind ("<f8>" . treemacs)
  :config
  (setq treemacs-width 35
        treemacs-is-never-other-window t)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t))

(use-package doom-themes
  :config (load-theme 'doom-one t))

;; ---------------------------
;; Global Key Bindings
;; ---------------------------
(global-set-key (kbd "M-.") 'lsp-find-definition)
(global-set-key (kbd "M-,") 'lsp-find-references)
(global-set-key (kbd "C-c C-r") 'python-shell-send-region)
(global-set-key (kbd "C-c C-c") 'python-shell-send-buffer)

(custom-set-variables
 '(package-selected-packages nil))
