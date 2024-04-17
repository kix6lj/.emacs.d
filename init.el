;; -*- mode: emacs-lisp; lexical-binding: t -*-

(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

(global-hl-line-mode 1)
(set-face-font 'default "Consolas 12")
;; (set-fontset-font "fontset-default" )
(menu-bar-mode -1)

(setq visible-bell t)

;;; initialize package sources
(require 'package)

(setq package-archives '(("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu//")
			 ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
			 ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))


(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialized use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(column-number-mode)
(global-display-line-numbers-mode t)

(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		treemacs-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;; set emacs backup file directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))
(setq backup-by-copying t)

(org-babel-load-file (concat user-emacs-directory "config.org"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(embark-consult embark dashboard emacs-dashboard corfu-popupinfo corfu-info vertico-directory marginalia corfu consult orderless vertico eyebrowse neotree treesit-auto sort-tab ledger-mode ob-ledger utop flycheck-ocaml merlin-eldoc merlin-mode ocp-indent json-mode flycheck undo-tree git-timemachine merlin tuareg clang-format eshell-git-prompt ansi-term-mode ansi-term eterm-256color which-key use-package rainbow-delimiters magit lsp-ui lsp-metals helpful doom-themes doom-modeline))
 '(safe-local-variable-values
   '((eval add-to-list 'eglot-server-programs
	   '(python-ts-mode "/home/kix6/.miniconda3/envs/SMAC/bin/pyright-langserver" "--stdio"))
     (eval
      (add-to-list 'eglot-server-programs
		   '(python-ts-mode "/home/kix6/.miniconda3/envs/SMAC/bin/pyright-langserver" "--stdio")))
     (demo . "this is a demo"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
