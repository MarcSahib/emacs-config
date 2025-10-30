;; -*- lexical-binding: t; -*-
(setq select-active-regions nil)     ; Make copy paste stable for windows 
(setq inhibit-startup-message t)     ; No splash screen
(setq initial-scratch-message nil)   ; No default message in scratch buffer
(setq confirm-kill-emacs 'y-or-n-p)  ; Warn instead of closing Emacs
(require 'package)

;; Sources: GNU + MELPA
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; If packet list is still empty, update
(unless package-archive-contents
  (package-refresh-contents))

;; Install vterm if its not installed already
(unless (package-installed-p 'vterm)
  (package-install 'vterm))

;; Load only if installed vterm
(when (package-installed-p 'vterm)
  (require 'vterm))

(defun my-flash-mode-line ()
  "Flash the mode line instead of ringing the bell."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))
(setq ring-bell-function #'my-flash-mode-line)

;; Dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  ;; Header text
  (setq dashboard-startup-banner 'official) ;; official, 0 = no banner
  (setq dashboard-utems '((recents .5)      ;; last 5 opened files
			  (projects .5)))   ;; Projects
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
)

;; === Terminal-only Treemacs für Rebecca-Theme ===
(use-package treemacs
  :ensure t)

(use-package treemacs-nerd-icons
  :after treemacs
  :ensure t
  :config
  (treemacs-load-theme "nerd-icons"))

;(treemacs-load-theme "all-the-icons")

(setq modus-themes-common-palette-overrides
 ` ((accent-1 "#79a8ff")
   (bg-active bg-main)
   (bg-completion "#2f447f")
   (bg-hover-secondary "#676E95")
   (bg-line-number-active unspecified)
   (bg-line-number-inactive "#292D3E")
   (bg-main "#292D3E")
   (bg-mode-line-active "#232635")
   (bg-mode-line-inactive "#282c3d")
   (bg-prompt unspecified)
   (bg-prose-block-contents "#232635")
   (bg-prose-block-delimiter bg-prose-block-contents)
   (bg-region "#3C435E")
   (bg-tab-bar      "#292D3E")
   (bg-tab-current  bg-main)
   (bg-tab-other    "#292D3E")
   (border-mode-line-active nil)
   (border-mode-line-inactive nil)
   (builtin "#82aaff")
   (comment "#676E95")
   (constant "#f78c6c")
 (docstring "#8d92af")
 (fg-active fg-main)
 (fg-completion white)
 (fg-heading-0 "#82aaff")
 (fg-heading-1 "#82aaff")
 (fg-heading-2 "#c792ea")
 (fg-heading-3 "#bb80b3")
 (fg-heading-4 "#a1bfff")
 (fg-line-number-active fg-main)
 (fg-line-number-inactive "gray50")
 (fg-main "#EEFFFF")
 (fg-mode-line-active "#A6Accd")
 (fg-mode-line-inactive "#676E95")
 (fg-prompt "#c792ea")
 (fg-prose-block-delimiter "#676E95")
 (fg-prose-verbatim "#c3e88d")
 (fg-region white)
 (fnname "#82aaff")
 (fringe "#292D3E")
 (keyword "#89DDFF")
 (string "#c3e88d")
 (type "#c792ea")
 (variable "#c792ea"))
 ;; (border-mode-line-active "#676E95")
 ;; (border-mode-line-inactive bg-dim)
 )

(load-theme 'rebecca t)
;(load-theme 'modus-vivendi-tinted)

;; Markdown
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" "\\.markdown\\'")
  :init
  (setq markdown-command "multimarkdown"))

(add-hook 'markdown-mode-hook 'orgtbl-mode)

;; Scratch Message
(setq initial-scratch-message ";;From *scratch*")
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; YAML
(use-package yaml-mode
  :ensure t
  :mode ("\\.ya?ml\\'" . yaml-mode))

;; Ansible
(use-package ansible
  :ensure t
  :hook (yaml-mode . ansible))

; Ansible Docs
(use-package ansible-doc
  :ensure t
  :hook (yaml-mode . ansible-doc-mode))

;; Start Emacs in full screen 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(all-the-icons ansible ansible-doc company-ansible dashboard flycheck
		   jinja2-mode magit markdown-mode nov rebecca-theme
		   treemacs-nerd-icons vterm yaml-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Jinja2
(use-package jinja2-mode
  :ensure t
  :mode ("\\.j2\\'" . jinja2-mode))

;; Linter
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; Ansible-Lint
(flycheck-define-checker ansible-lint
  "A checker using ansible-lint."
  :command ("ansible-lint" source-inplace)
  :error-patterns
  ((error line-start (file-name) ":" line ": " (message) line-end))
  :modes (yaml-mode))

(add-to-list 'flycheck-checkers 'ansible-lint)

;; Autocompletion
(use-package company-ansible
  :ensure t
  :after company
  :hook (yaml-mode . company-ansible-lint))

;; Snippets
;; (use-package ansible-snippets
;;   :ensure t
;;  :after yasnippet)
  
;; Magit
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

;; Epub's
(use-package nov
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;; Fast swtichting between windows and frames
(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window))

;; Delete with Backspace and DEL
(delete-selection-mode 1)

;; avy GoToChar
(use-package avy
  :ensure t
  :bind
  (("C-:" . avy-goto-char)
   ("C-'" . avy-goto-char-2)
   ("M-g g" . avy-goto-line)))
