(defvar package-archives)
(defvar package-archive-contents)

(defvar my:backup-directory
  (expand-file-name (concat user-emacs-directory "backups/"))
  "Directory storing all backups and auto-save files.
Must end with a trailing slash.")

(setq exec-path (append exec-path '("/Users/aperalta/Library/Python/3.7/bin")))
(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)

(setq inhibit-startup-message t)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

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
  :config (progn
	    (setq ag-executable "/usr/local/bin/ag"))
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
	    (setq ac-modes (delq 'python-mode ac-modes))
            (global-auto-complete-mode 1)))

(use-package material-theme
  :ensure material-theme)

(use-package files
  :config
  (progn
    (setq auto-save-file-name-transforms `((".*" ,my:backup-directory t))
          auto-save-list-file-prefix my:backup-directory
          backup-directory-alist `(("." . ,my:backup-directory))
          auto-save-default t
          auto-save-interval 200
          auto-save-timeout 20
          backup-by-copying t
          delete-by-moving-to-trash t
          delete-old-versions t
          kept-new-versions 20
          kept-old-versions 0
          make-backup-files t
          version-control t)
    (defun force-backup-of-buffer ()
      "Reset `buffer-backed-up' to nil."
      (setq buffer-backed-up nil))
    ;; Always create backups on save:
    (add-hook 'before-save-hook #'delete-trailing-whitespace)
    (add-hook 'before-save-hook #'force-backup-of-buffer)))


(use-package elpy)
(elpy-enable)

(use-package pyenv-mode
  :init
  (add-to-list 'exec-path "~/.pyenv/shims")
  (setenv "WORKON_HOME" "~/.pyenv/versions/")
  :config
  (pyenv-mode)
  :bind
  ("C-x p e" . pyenv-activate-current-project))

(defun pyenv-activate-current-project ()
  "Automatically activates pyenv version if .python-version file exists."
  (interactive)
  (let ((python-version-directory (locate-dominating-file (buffer-file-name) ".python-version")))
    (if python-version-directory
        (let* ((pyenv-version-path (f-expand ".python-version" python-version-directory))
               (pyenv-current-version (s-trim (f-read-text pyenv-version-path 'utf-8))))
          (pyenv-mode-set pyenv-current-version)
          (message (concat "Setting virtualenv to " pyenv-current-version))))))

(defvar pyenv-current-version nil nil)

(defun pyenv-init()
  "Initialize pyenv's current version to the global one."
  (let ((global-pyenv (replace-regexp-in-string "\n" "" (shell-command-to-string "pyenv global"))))
    (message (concat "Setting pyenv version to " global-pyenv))
    (pyenv-mode-set global-pyenv)
    (setq pyenv-current-version global-pyenv)))

(add-hook 'after-init-hook 'pyenv-init)

;; (use-package flycheck
;;   :ensure flycheck
;;   :config
;;   (progn
;;     ;; Add virtualenv support for checkers
;;     (defadvice flycheck-checker-executable
;;       (around python-flycheck-check-executable (checker)
;;               activate compile)
;;       "`flycheck-checker-executable' with virtualenv support."
;;       (if (eq major-mode 'python-mode)
;;           (let* ((process-environment (python-shell-calculate-process-environment))
;;                  (exec-path (python-shell-calculate-exec-path)))
;;             ad-do-it)
;;         ad-do-it))

;;     (defadvice flycheck-start-checker
;;       (around python-flycheck-start-checker (checker callback)
;;               activate compile)
;;       "`flycheck-start-checker' with virtualenv support."
;;       (if (eq major-mode 'python-mode)
;;           (let* ((process-environment (python-shell-calculate-process-environment))
;;                  (exec-path (python-shell-calculate-exec-path)))
;;             ad-do-it)
;;         ad-do-it))

;;     (setq flycheck-mode-line-lighter " ")))


;; go get -u golang.org/x/tools/cmd/...
;; go get -u github.com/rogpeppe/godef/...
;; go get -u github.com/nsf/gocode
;; go get -u golang.org/x/tools/cmd/goimports
;; go get -u golang.org/x/tools/cmd/guru
;; go get -u github.com/dougm/goflymake

(defun kt/go-mode-hook ()
	(setq tab-width 2)
	(linum-mode 1)
	(flycheck-mode)
	(go-eldoc-setup)
	(rainbow-delimiters-mode)
	(idle-highlight-mode))

(use-package go-mode
	:ensure go-mode
	:mode "\\.go\\'"
	:commands (gofmt-before-save)
	:init (progn
		(setenv "GOPATH" (expand-file-name "~/go"))
		(setenv "PATH" (concat  (getenv "GOPATH") ":" (getenv "PATH")))
		(setq exec-path (append exec-path (list (format "%s/bin/" (getenv "GOPATH")))))
		(setq exec-path (append exec-path '("/usr/local/go/bin/")))
		(add-to-list 'load-path (format "%s/src/github.com/dougm/goflymake" (getenv "GOPATH"))))
	:bind (("C-c C-r" . go-remove-unused-imports)
	       ("C-." . godef-jump)
	       ("M-a" . beginning-of-defun)
	       ("M-e" . end-of-defun))
	:config (progn
						(use-package go-eldoc
							:ensure go-eldoc)

						;; go-autocomplete requires this thing and for some reason it
						;; won't ask for it
						(use-package auto-complete
							:ensure auto-complete)
						(require 'auto-complete-config)
						(ac-config-default)
						(use-package go-autocomplete
							:ensure go-autocomplete)

						(use-package idle-highlight-mode
							:ensure idle-highlight-mode)

						;; Use goimports instead of gofmt
						;; you have to install it first: see here go get golang.org/x/tools/cmd/goimports
						(setq gofmt-command "goimports")
						(add-hook 'before-save-hook 'gofmt-before-save)
 						(add-hook 'go-mode-hook #'kt/go-mode-hook)))


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

(use-package multi-web-mode
	:if (not noninteractive)
	:ensure multi-web-mode
	:config (progn
		  (setq mweb-default-major-mode 'html-mode)
		  (setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
				    (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
				    (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
		  (setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
		  (multi-web-global-mode 1)))

(use-package powerline
  :if (not noninteractive)
  :ensure powerline
  :config (powerline-default-theme))

(use-package projectile
  :if (not noninteractive)
  :diminish projectile-mode
  :ensure projectile
  :config
  (progn
    (projectile-global-mode 1)
    (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))
  )

;; (use-package python
;;   :config
;;   (progn
;;     (use-package jedi
;;       :ensure jedi)
;;     (setq jedi:complete-on-dot t)
;;     (remove-hook 'python-mode-hook 'wisent-python-default-setup)
;;     (add-hook 'python-mode-hook 'jedi:setup)
;;     (add-hook 'python-mode-hook 'flycheck-mode)))

;; (use-package python-django
;;   :if (not noninteractive)
;;   :bind ("C-x j" . python-django-open-project)
;;   :ensure python-django)

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
		  '(c-mode c++-mode emacs-lisp-mode python-mode lisp-mode go-mode))
	    (global-whitespace-mode 1)))


(use-package org
  :config (progn
	    (org-agenda-files (list "~/org/shipping.org"
				    "~/org/personal.org"))))



(setq custom-file "~/.emacs.d/custokm.el")
(load custom-file 'noerror)
