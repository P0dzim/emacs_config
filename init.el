;; 1. CONFIGURAÇÕES BÁSICAS DE INTERFACE
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 1) ;; Ativa números de linha globalmente

;; 2. GERENCIADOR DE PACOTES
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Instala pacotes essenciais automaticamente
(setq my-packages '(neotree company doom-themes all-the-icons nerd-icons))
(unless package-archive-contents (package-refresh-contents))
(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

;; 3. CONFIGURAÇÃO DO NEOTREE
(require 'nerd-icons)
(require 'all-the-icons)
(require 'neotree)

;; Muda o newtree para abrir com C-\
(global-set-key (kbd "C-\\") 'neotree-toggle)

;; 4. AUTOCOMPLETE (COMPANY)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-minimum-prefix-length 2)
(setq company-idle-delay 0.2)
(setq company-tooltip-limit 3)      ;; Define o limite de opções
(setq company-tooltip-align-annotations t) ;; Mantém o visual alinhado

;; 5. TEMA VISUAL
(load-theme 'doom-acario-dark t)
(doom-themes-neotree-config)

;; 6. ADICIONA ATALHO PARA CABEÇALHO PADRÃO
(if (file-exists-p "~/.emacs.d/stdheader.el") (load-file "~/.emacs.d/stdheader.el"))
