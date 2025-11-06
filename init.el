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
(menu-bar-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq default-frame-alist '((undecorated . t)))
 

;; ==============================================================
;; Theme
;; ==============================================================
(mapc #'disable-theme custom-enabled-themes)
(load-theme 'deeper-blue t)

(set-face-attribute 'mode-line nil
                    :foreground "gray60"
                    :background "gray20"
                    :box nil)
(set-face-attribute 'mode-line-inactive nil
                    :foreground "gray60"
                    :background "gray10"
                    :box nil)
(set-face-attribute 'mode-line-buffer-id nil
		    :foreground "gray80")

(add-to-list 'default-frame-alist '(undecorated . t))


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
; Icons in corfu autocompletion
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
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp))
         ;; if you want which-key integration
         ;(lsp-mode . lsp-enable-which-key-integration)
  :commands lsp)

;; Custom LSP Command
; register server
(require 'lsp-mode)
;; Sag lsp-mode, dass pylsp in Docker läuft:
(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   (lambda ()
                     ;; Liste von Strings, kein einziger String!
                     (list "podman" "run" "--rm" "-i"
                           "-v" (concat (expand-file-name default-directory) ":/workspace")
                           "-w" "/workspace"
                           "docker.io/lspcontainers/python-lsp-server:1.12.0"
                           "pylsp")))
  :major-modes '(python-mode)
  :priority -1
  :server-id 'pylsp-docker))

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

;; -----------------------------------------
;; Windows Clipboard Integration via wl-copy
;; -----------------------------------------
;(when (executable-find "wl-copy")
;  (setq interprogram-cut-function
;        (lambda (text &optional push)
;          (let ((process-connection-type nil))
;            (let ((proc (start-process "wl-copy" "*Messages*" "wl-copy" "-n")))
;              (process-send-string proc text)
;              (process-send-eof proc)))))
;
;  (setq interprogram-paste-function
;        (lambda ()
;          (with-temp-buffer
;            (call-process "wl-paste" nil t)
;            (buffer-string)))))

;; ==============================================================
;; Allgemeine UTF-8 Einstellungen
;; ==============================================================

;; UTF-8 bevorzugen, Emacs-intern und für Clipboard
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-selection-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(setq select-enable-clipboard t)
(setq select-enable-primary t)

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
      ;; In einer Markdown-Tabelle -> Rückwärts bewegen
      (markdown-table-backward-cell)
    ;; Sonst: Text nach links einrücken
    (if (use-region-p)
        (indent-rigidly (region-beginning) (region-end) (- tab-width))
      (indent-rigidly (line-beginning-position) (line-end-position) (- tab-width)))))

(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "<backtab>") #'my-markdown-shifttab-smart))

;; ==============================================================
;; avy GoToChar
;; ==============================================================
(use-package avy
  :ensure t
  :bind
  (("C-:" . avy-goto-char)
   ("C-'" . avy-goto-char-2)
   ("M-g g" . avy-goto-line)))



(provide 'init)
;;; init.el ends here
