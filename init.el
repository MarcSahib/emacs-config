;; -*- lexical-binding: t; -*-

;; ==============================================================
;; Basics
;; ==============================================================
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq confirm-kill-emacs 'y-or-n-p)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; ==============================================================
;; Package Management
;; ==============================================================
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; use-package installieren falls fehlt
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; ==============================================================
;; No Menu- And Scroll-Bars
;; ==============================================================
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)


 

;; ==============================================================
;; Theme
;; ==============================================================
(use-package rebecca-theme
  :config
  (load-theme 'deeper-blue t)
  (custom-set-faces
   '(menu ((t (:background "#301a4b" :foreground "white"))))))




;; ==============================================================
;; Autocompletion
;; ==============================================================
(use-package corfu
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-preselect-first t)
  (corfu-cycle t))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; ==============================================================
;; LSP
;; ==============================================================
(use-package lsp-mode 
  :commands lsp
  :hook ((yaml-mode . lsp)
         (json-mode . lsp)
         (python-mode . lsp))  
  :config
  ;; Ansible LSP
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("ansible-language-server" "--stdio"))
    :major-modes '(yaml-mode)
    :server-id 'ansible-ls))
  (setq lsp-log-io nil))



;; ==============================================================
;; YAML / Ansible
;; ==============================================================
(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package ansible-doc
  :hook (yaml-mode . ansible-doc-mode))

;; ==============================================================
;; Markdown
;; ==============================================================
(use-package markdown-mode
  :mode ("\\.md\\'" "\\.markdown\\'")
  :init
  (setq markdown-command "pandoc -f markdown -t html"))

;; ==============================================================
;; Jinja2
;; ==============================================================
(use-package jinja2-mode
  :mode ("\\.j2\\'" . jinja2-mode))

;; ==============================================================
;; Linting
;; ==============================================================
(use-package flycheck
  :init (global-flycheck-mode))

(flycheck-define-checker ansible-lint
  "A checker using ansible-lint."
  :command ("ansible-lint" source-inplace)
  :error-patterns ((error line-start (file-name) ":" line ": " (message) line-end))
  :modes (yaml-mode))
(add-to-list 'flycheck-checkers 'ansible-lint)

;; ==============================================================
;; Git
;; ==============================================================
(use-package magit
  :bind (("C-x g" . magit-status)))

;; ==============================================================
;; Terminal
;; ==============================================================
(use-package vterm)

;; ==============================================================
;; Window Management
;; ==============================================================
(use-package ace-window
  :bind (("M-o" . ace-window)))

;; ==============================================================
;; Kill Ring
;; ==============================================================
(use-package browse-kill-ring
  :bind ("C-c k" . browse-kill-ring))

;; ==============================================================
;; Line numbers toggle
;; ==============================================================
(global-set-key (kbd "C-x C-l") #'display-line-numbers-mode)

;;===============================================================
;; Clipboard
;;===============================================================
(setq select-enable-clipboard t)
;(setq select-enable-primary t)
;(setq save-interprogram-paste-before-kill t)

;; ==============================================================
;; Dashboard
;; ==============================================================
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  ;; Header text
  (setq dashboard-startup-banner 'official) ;; official, 0 = no banner
  (setq dashboard-items '((recents . 5)      ;; last 5 opened files
			  (projects . 5)))   ;; Projects
  (setq dashboard-set-heading-icons nil)
  (setq dashboard-set-file-icons nil)
)

;; ==============================================================
;; Kill-Ring -> Windows Clipboard
;; ==============================================================
(defun copy-to-windows-clipboard (text)
  "Copy TEXT to Windows clipboard via clip.exe."
  (let ((process-connection-type nil))
    (let ((proc (start-process "clip" nil "clip.exe")))
      (process-send-string proc text)
      (process-send-eof proc))))

(defun my/copy-region-to-windows-clipboard-advice (&rest args)
  "After kill-ring-save, copy region to Windows clipboard."
  (let ((beg (nth 0 args))
        (end (nth 1 args)))
    (when (and beg end (use-region-p))
      (copy-to-windows-clipboard (buffer-substring beg end)))))

(advice-add 'kill-ring-save :after #'my/copy-region-to-windows-clipboard-advice)


;; Mit Shift + TAB nach links
(defun my-markdown-shifttab-indent-left ()
  "Shift region or current line to the left in markdown-mode."
  (interactive)
  (if (use-region-p)
      (indent-rigidly (region-beginning) (region-end) (- tab-width))
    (indent-rigidly (line-beginning-position) (line-end-position) (- tab-width))))

(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "<backtab>") #'my-markdown-shifttab-indent-left))


;; Mit Shift + Tab nach links wenn in Markdown Table
(defun my-markdown-shifttab-smart ()
  "In markdown-mode: If inside a table, move backward; otherwise indent left."
  (interactive)
  (if (and (fboundp 'markdown-table-align)
           (markdown-table-at-point-p))
      ;; In einer Markdown-Tabelle -> R++ckw+ñrts bewegen
      (markdown-table-backward-cell)
    ;; Sonst: Text nach links einr++cken
    (if (use-region-p)
        (indent-rigidly (region-beginning) (region-end) (- tab-width))
      (indent-rigidly (line-beginning-position) (line-end-position) (- tab-width)))))

(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "<backtab>") #'my-markdown-shifttab-smart))


;; Treemacs
(use-package treemacs
  :ensure t)

(use-package treemacs-nerd-icons
  :after treemacs
  :ensure t
  :config
  (treemacs-load-theme "nerd-icons"))





(provide 'init)
;;; init.el ends here
