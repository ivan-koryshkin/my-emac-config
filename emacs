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

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

(global-display-line-numbers-mode t)
(show-paren-mode 1)
(setq inhibit-startup-message t)
;; ---------------------------
;; Font size
;; ---------------------------
(set-face-attribute 'default nil :height 160) ;; 18pt


;; ---------------------------
;; Ivy + Projectile
;; ---------------------------
(use-package ivy :config (ivy-mode 1))
(use-package counsel)
(use-package swiper)
(use-package projectile :config (projectile-mode +1))

;; ---------------------------
;; Which key
;; ---------------------------
(use-package which-key :config (which-key-mode))

;; ---------------------------
;; Python mode + LSP
;; ---------------------------
(use-package python
  :ensure t
  :hook (python-mode . lsp-deferred)
  :config (setq python-indent-offset 4))

(use-package lsp-mode
  :commands lsp lsp-deferred
  :hook (python-mode . lsp-deferred)
  :config (setq lsp-prefer-flymake nil))

(use-package lsp-ui :commands lsp-ui-mode)

(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0))

(global-set-key (kbd "M-.") 'lsp-find-definition)
(global-set-key (kbd "M-,") 'lsp-find-references)

;; ---------------------------
;; Flycheck + Black + Ruff
;; ---------------------------
(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (flycheck-define-checker python-ruff
    "A Python syntax and style checker using Ruff."
    :command ("ruff" source-inplace)
    :error-patterns
    ((error line-start (file-name) ":" line ":" column ": error: " (message) line-end)
     (warning line-start (file-name) ":" line ":" column ": warning: " (message) line-end))
    :modes python-mode)
  (add-to-list 'flycheck-checkers 'python-ruff))

(use-package blacken
  :hook (python-mode . blacken-mode))

;; ---------------------------
;; DAP mode (debugger)
;; ---------------------------
(use-package dap-mode
  :after lsp-mode
  :config
  (dap-auto-configure-mode)
  (require 'dap-python)
  (setq dap-python-executable (executable-find "python")))
;; ===========================
;; Python + venv support
;; ===========================
(use-package pyvenv
  :config
  (add-hook 'python-mode-hook
            (lambda ()
              (let ((venv-path (locate-dominating-file default-directory ".venv")))
                (when venv-path
                  (pyvenv-activate (expand-file-name ".venv" venv-path))))))
  (setq pyvenv-mode-line-indicator
        '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
  (pyvenv-mode 1))

(use-package lsp-mode
  :hook (python-mode . lsp)
  :commands lsp)

(use-package dap-mode
  :after lsp-mode
  :config
  (require 'dap-python)
  (setq dap-python-executable (executable-find "python"))
  (dap-mode 1)
  (dap-ui-mode 1))

;; ---------------------------
;; File explorer: Treemacs
;; ---------------------------
(use-package treemacs
  :after projectile
  :config
  (setq treemacs-width 35
        treemacs-is-never-other-window t)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'deferred))


(with-eval-after-load 'treemacs
  (global-set-key (kbd "<f8>") 'treemacs))


;; ---------------------------
;; Git integration: Magit
;; ---------------------------
(use-package magit
  :bind ("C-x g" . magit-status))

;; ---------------------------
;; Optional: Better syntax highlighting / shell path
;; ---------------------------
(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

;; ---------------------------
;; Dark theme
;; ---------------------------
(use-package doom-themes
  :config
  (load-theme 'doom-one t))

;; ---------------------------
;; Key bindings (Python)
;; ---------------------------
(global-set-key (kbd "C-c C-r") 'python-shell-send-region)
(global-set-key (kbd "C-c C-c") 'python-shell-send-buffer)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; Tmp files folder
(defvar my-auto-save-dir "~/.emacs-auto-saves/"

  (unless (file-exists-p my-auto-save-dir)
  (make-directory my-auto-save-dir t)))

(setq auto-save-file-name-transforms
      `((".*" ,my-auto-save-dir t)))

;; Recovery files folder
(setq backup-directory-alist `(("." . "~/.emacs-backups")))
(setq make-backup-files t)


(defun my-clean-temp-files ()
  (let ((dirs (list my-auto-save-dir "~/.emacs-backups")))
    (dolist (dir dirs)
      (when (file-exists-p dir)
        (dolist (file (directory-files dir t))
          (when (and (string-match-p "\\(#.*#$\\|~$\\|\\.#[^/]+\\)" file)
                     (> (- (float-time (current-time))
                           (float-time (nth 5 (file-attributes file))))
                        (* 24 60 60)))
            (delete-file file)))))))

(add-hook 'emacs-startup-hook 'my-clean-temp-files)

