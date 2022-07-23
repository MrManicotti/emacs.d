;; straight bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Configuration
(straight-use-package 'use-package)

(use-package projectile
  :straight t
  :init
  (setq projectile-project-search-path '("~/Projects/"))
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package plantuml-mode
  :straight t
  :custom
  (plantuml-jar-path "~/languages_servers/plantuml-1.2022.6.jar")
  (plantuml-default-exec-mode 'jar))

(use-package terraform-mode
  :straight t)

(use-package elixir-mode
  :straight t)

(use-package python
  :hook
  (python-mode . (lambda ()
                   (setq python-indent-def-block-scale 1))))

(use-package lsp-mode
  :commands lsp
  :straight t
  :diminish lsp-mode
  :hook
  (elixir-mode . lsp)
  (terraform-mode . lsp)
  (javascript-mode . lsp)
  :init
  (add-to-list 'exec-path "~/languages_servers/elixir-ls-1.13")
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\build\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\deps\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\terraform\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\venv\\'")
  )

(use-package avy
  :straight t
  :bind
  (("C-:" . avy-goto-char)
   ("C-'" . avy-goto-char-2)
   ("M-g f" . avy-goto-line)
   ("M-g w" . avy-goto-word-1)
   ("M-g e" . avy-goto-word-0)))

(use-package company
  :straight t
  :config
  (global-company-mode))

(use-package doom-modeline
  :straight t
  :config
  (doom-modeline-mode 1))

(use-package all-the-icons
  :straight t
  :if (display-graphic-p))

(use-package magit
  :straight t)

(use-package helm
  :straight t
  :config
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (helm-mode 1))

(use-package helm-projectile
  :straight t
  :config
  (helm-projectile-on))

(use-package ag
  :straight t)

(use-package helm-ag
  :straight t)

(use-package paredit
  :straight t)

(use-package sly
  :straight t
  :init
  (setq inferior-lisp-program "/usr/local/bin/sbcl"))

(use-package which-key
  :straight t)

(use-package tree-sitter-langs
  :straight t)

(use-package tree-sitter
  :straight t
  :init
  (require 'tree-sitter)
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package elpy
  :straight t
  :config
  (elpy-enable))

(use-package lsp-pyright
  :straight t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

(use-package org
  :straight t)

(use-package org-roam
  :straight t
  :custom
  (org-roam-directory (file-truename "~/roam"))
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n c" . org-roam-capture)
   ("C-c n r" . org-roam-node-random)		    
   (:map org-mode-map
         (("C-c n i" . org-roam-node-insert)
          ("C-c n o" . org-id-get-create)
          ("C-c n t" . org-roam-tag-add)
          ("C-c n a" . org-roam-alias-add)
          ("C-c n l" . org-roam-buffer-toggle))))
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode))

;; Misc
(setq js-indent-level 2)
(setq-default indent-tabs-mode nil)
(tool-bar-mode -1)

(load-theme 'modus-operandi)

;; LSP Mode Config
(add-to-list 'load-path
              "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

;; Org Agenda Configuration
(setq org-agenda-files
      '("~/Projects/atlantis-minutes"
	"~/Projects/atlantis"))
