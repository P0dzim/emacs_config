;; 1. CONFIGURAÇÕES BÁSICAS DE INTERFACE E COMPORTAMENTO
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 1) ;; Ativa números de linha globalmente
(defalias 'yes-or-no-p 'y-or-n-p) ;; Responder perguntas com apenas y ou n
(delete-selection-mode 1) ;; Não coloca o texto removido na área de texto copiado


;; Cria hook de comportamento para a norminette
(defun c-hook-fun()
  (setq-local c-basic-offset 4)
  (setq-local tab-width 4)
  (setq-local indent-tabs-mode t)
  (setq-local c-backspace-function 'backward-delete-char)
  (c-set-offset 'substatement-open 0)
  (setq-local tab-stop-list
                '(4 8 12 16 20 24 28 32
                    36 40 44 48 52 56 60
                      64 68 72 76 80 84 88 92 96
                    100 104 108 112 116 120))
  (local-set-key (kbd "TAB") #'self-insert-command)
  (local-set-key (kbd "C-c e o") #'ff-get-other-file) ;; Abre o outro arquivo, se .c abre o .o e vice-versa
  (electric-indent-mode nil))

;; Seta o hook para C e C++
(add-hook 'c-mode-hook #'c-hook-fun)
(add-hook 'c++-mode-hook #'c-hook-fun)

;; 2. GERENCIADOR DE PACOTES
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Instala pacotes essenciais automaticamente
(setq my-packages '(neotree company doom-themes all-the-icons nerd-icons vterm xclip))
(unless package-archive-contents (package-refresh-contents))
(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

;; 3. CONFIGURAÇÃO DO NEOTREE
(require 'nerd-icons)
(require 'all-the-icons)
(require 'neotree)
(global-set-key (kbd "C-\\") 'neotree-toggle) ;; Muda o neotree para abrir com C-\

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

;; 7. CONFIGURA VTERM
(defvar set-term "/bin/bash") ;; Define o bash como padrão
(defun open-terminal ()
  (interactive)
  (split-window-sensibly)   ;; Divide a tela
  (other-window 1)       ;; Move o cursor para a nova janela
  (shrink-window 10)     ;; Ajusta o tamanho (opcional)
  (ansi-term set-term)) ;; Abre o terminal real
(global-set-key (kbd "C-e") 'open-terminal)

;; 8. CONFIGURA O WINDMOVE
(windmove-default-keybindings 'meta)

;; 9. SALVA OS ARQUIVOS TEMPORÁRIOS E DE BACKUP EM PASTAS ESPECÍFICA
(defvar backup-dir "~/.emacs.d/backups")
(defvar temp-dir "~/.emacs.d/temp_files")

(unless (file-exists-p backup-dir)
  (make-directory backup-dir t))
(unless (file-exists-p temp-dir)
  (make-directory temp-dir t))
;; Configura arquivos de backup (~) para irem para a pasta central
(setq backup-directory-alist `(("." . ,backup-dir)))
;; Configura arquivos auto-save (#) para irem para a pasta central
;; O argumento `t` no final garante que o caminho seja único para evitar conflitos
(setq auto-save-file-name-transforms `((".*" ,temp-dir t)))

;; 10. COMPARTILHA A SESSÃO DE COPIAR E COLAR DO SISTEMA COM O EMACS (PARA A VERSÃO TERMINAL)
(require 'xclip)
(xclip-mode 1)
