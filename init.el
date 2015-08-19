(defvar package-archives)
(defvar package-archive-contents)

(setq package-archives '(("sunrise" . "http://joseito.republika.pl/sunrise-commander/")
                         ("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; activate all the packages (in particular autoloads)
(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; this installs use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)


(use-package ag
  :if (not noninteractive)
  :ensure ag)

(use-package auto-complete
  :if (not noninteractive)
  :ensure auto-complete
  :diminish auto-complete-mode
  :config (progn
            (require 'auto-complete-config)
            (ac-config-default)
            (setq-default ac-sources '(ac-source-yasnippet
                                       ac-source-filename
                                       ac-source-abbrev
                                       ac-source-dictionary
                                       ac-source-words-in-same-mode-buffers))
            (global-auto-complete-mode 1)))

(use-package cyberpunk-theme
  :ensure cyberpunk-theme)

(use-package flycheck
  :ensure flycheck
  :config
  (progn
    ;; Add virtualenv support for checkers
    (defadvice flycheck-checker-executable
      (around python-flycheck-check-executable (checker)
              activate compile)
      "`flycheck-checker-executable' with virtualenv support."
      (if (eq major-mode 'python-mode)
          (let* ((process-environment (python-shell-calculate-process-environment))
                 (exec-path (python-shell-calculate-exec-path)))
            ad-do-it)
        ad-do-it))

    (defadvice flycheck-start-checker
      (around python-flycheck-start-checker (checker callback)
              activate compile)
      "`flycheck-start-checker' with virtualenv support."
      (if (eq major-mode 'python-mode)
          (let* ((process-environment (python-shell-calculate-process-environment))
                 (exec-path (python-shell-calculate-exec-path)))
            ad-do-it)
        ad-do-it))

    (setq flycheck-mode-line-lighter " ")

    (global-flycheck-mode 1)))

(use-package go-mode
  :ensure go-mode)

;; this is for the autocomplete for commands
(use-package ido
  :if (not noninteractive)
  :config
  (progn
    (use-package ido-vertical-mode
      :ensure ido-vertical-mode)
    (use-package flx
      :ensure flx)
    (use-package flx-ido
      :ensure flx-ido)
    (setq ido-enable-flex-matching t
          ido-use-faces nil
          flx-ido-use-faces t)
    (ido-mode 1)
    (ido-everywhere 1)
    (ido-vertical-mode 1)
    (flx-ido-mode 1)))


(use-package powerline
  :if (not noninteractive)
  :ensure powerline
  :config (powerline-default-theme))

(use-package projectile
  :if (not noninteractive)
  :diminish projectile-mode
  :ensure projectile
  :config (projectile-global-mode 1))

(use-package python
  :config
  (progn
    (use-package jedi
      :ensure jedi)
    (setq jedi:complete-on-dot t)
    (remove-hook 'python-mode-hook 'wisent-python-default-setup)
    (add-hook 'python-mode-hook 'jedi:setup)))

(use-package python-django
  :if (not noninteractive)
  :bind ("C-x j" . python-django-open-project)
  :ensure python-django)

(use-package magit
  :if (not noninteractive)
  :bind ("C-x g" . magit-status)
  :ensure magit
  :config
  (progn
    (defun magit-diff-toggle-whitespace ()
      (interactive)
      (if (member "-w" magit-diff-options)
          (magit-diff-dont-ignore-whitespace)
        (magit-diff-ignore-whitespace)))
    (defun magit-diff-ignore-whitespace ()
      (interactive)
      (add-to-list 'magit-diff-options "-w")
      (magit-refresh))
    (defun magit-diff-dont-ignore-whitespace ()
      (interactive)
      (setq magit-diff-options (remove "-w" magit-diff-options))
      (magit-refresh))
    (bind-key "W" 'magit-diff-toggle-whitespace magit-status-mode-map)))

;; colour for your parens...
(use-package rainbow-mode
  :if (not noninteractive)
  :ensure rainbow-mode
  :config (progn
            (mapc (lambda (mode)
                    (add-to-list 'rainbow-r-colors-major-mode-list mode))
                  '(css-mode emacs-lisp-mode lisp-interaction-mode))
            (add-hook 'prog-mode-hook #'rainbow-turn-on)))

(use-package rainbow-delimiters
  :if (not noninteractive)
  :ensure rainbow-delimiters
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package scroll-bar
  :config (scroll-bar-mode -1))

(use-package smartparens
  :if (not noninteractive)
  :ensure smartparens
  :diminish (smartparens-mode . " π"))

(use-package smex
  :if (not noninteractive)
  :ensure smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c M-x" . execute-extended-command))
  :config (smex-initialize))

(use-package tool-bar
  :config (tool-bar-mode -1))

(use-package uniquify
  :if (not noninteractive)
  :config (setq uniquify-buffer-name-style 'forward))

(use-package whitespace
  :if (not noninteractive)
  :diminish (global-whitespace-mode . " ω")
  :config (progn
            (setq whitespace-style '(trailing tabs indentation::space face))
            (setq whitespace-global-modes
                  '(c-mode c++-mode clojure-mode emacs-lisp-mode js-mode php-mode
                           python-mode lisp-mode))
            (global-whitespace-mode 1)))

(setq inhibit-startup-screen t
      initial-scratch-message ""
      use-dialog-box nil
      x-select-enable-clipboard t
      x-select-enable-primary t)