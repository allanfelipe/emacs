;; Meu arquivo de configuração .emacs (emacs 24.4.1)
;; Allan Felipe
;; 15-Dez-2016

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Configurações gerais ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Não exibe a tela inicial
(setq inhibit-startup-screen t)

; Tira a barra de ferramentas
(tool-bar-mode 0)

; Cursor é barra e não box
(setq-default cursor-type '(bar . 2))

; Cursor pisca indefinidamente e não apenas 10 vezes
(setq blink-cursor-blinks 0)

; Wrap words (kill line apaga só a linha visual. Se quiser apagar toda a linha, descomenta a linha seguinte)
;(setq-default word-wrap t)
(global-visual-line-mode t)

; Habilita C-c, C-v, C-x, C-z
(cua-mode 1)

; Não toca o beep do pc 
(setq ring-bell-function 'ignore)

; Highlight de parênteses e colchetes quando o cursor está do lado externo
(show-paren-mode 1)

; Também mostra numeração da coluna, além da linha, na barra de status
(column-number-mode 1)

; Muda yes-or-no para y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

; Major mode para buffers novos ou sem modo especificado
(setq-default major-mode 'text-mode)

; Major mode do buffer inicial *scratch*
(setq initial-major-mode 'text-mode)

; Inicia scratch vazio
(setq initial-scratch-message "")

; Desabilita indentação automática
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

; Valor de apenas 1 linha pra mouse wheel
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))

; Não acelera quando usa mouse wheel
(setq mouse-wheel-progressive-speed nil)

; Número de linhas de scrolling
(setq scroll-step 1)

; Fica mais organizado no minibuffer ao abrir buffers e arquivos
(ido-mode 1)

; Exibe numeração das linhas, numa coluna à esquerda
(global-linum-mode t)

; Carrega um tema
(load-theme 'misterioso) 
(set-cursor-color "white")

; Posição e tamanho inicial do frame (Em 1280x1024)
(setq initial-frame-alist '((top . 104) (left . 295) (width . 80) (height . 40)))


; Backup files estão na forma !diretório!file~ (com version-control fica file.~1~, file.~2~,..., onde números mais altos são os mais novos). Sempre que se salva um buffer depois de abrir o arquivo, ele faz um backup da versão antiga que foi aberta. (Parece que só root pode abrir os backups)

(setq backup-directory-alist '(("" . "~/.emacs.d/backups/"))
   version-control t           ; Arquivos de backup são numerados
   vc-make-backup-files t      ; Faz backup de versioned files
   backup-by-copying t         ; Backup por cópia
   delete-old-versions t       ; Deleta backups antigos sem perguntar
   kept-old-versions 0         ; Não mantém nenhuma versão mais antiga
   kept-new-versions 6         ; Mantém 6 arquivos mais recentes
)

; Auto-save files estão na forma #!diretório!file#. São a última versão da edição do arquivo e só um arquivo fica salvo.

(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/auto-saves/" t))
      auto-save-interval 200    ; Número de keystrokes pra salvar)
)

; Key bindings para temas (dá download em um tema "nome-theme.el" e joga na pasta ~/.emacs.d/themes/) [o "t" dá bypass nas perguntas sobre a segurança do arquivo]
; M-x customize-themes: Lista os temas disponíveis

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (load-theme 'misterioso) (set-cursor-color "white"))) ; Um tema padrão
(global-set-key (kbd "C-<f2>") (lambda () (interactive) (load-theme 'tangotango t))) ; fundo cinza escuro (letra lilás, azul)
(global-set-key (kbd "C-<f3>") (lambda () (interactive) (load-theme 'hamburg t) (set-cursor-color "grey"))) ; fundo quase preto (letra lilás, azul)
(global-set-key (kbd "C-<f4>") (lambda () (interactive) (load-theme 'gotham t))) ; fundo quase preto (letras em tons de azul/verde)
(global-set-key (kbd "C-<f5>") (lambda () (interactive) (load-theme 'subatomic t))) ; fundo roxo claro (letra amarelo, verde)
(global-set-key (kbd "C-<f6>") (lambda () (interactive) (load-theme 'warm-night t) (set-cursor-color "grey"))) ; fundo marrom
(global-set-key (kbd "C-<f7>") (lambda () (interactive) (load-theme 'zenburn t)))   ; fundo bege (letra amarelo, laranja) 
(global-set-key (kbd "C-<f8>") (lambda () (interactive) (load-theme 'monokai t))) ; fundo meio marrom escuro (letra amarelo, vermelha)
(global-set-key (kbd "C-<f9>") (lambda () (interactive) (load-theme 'gruber-darker t))) ; fundo preto (letra amarelo, verde, laranja)
(global-set-key (kbd "C-<f10>") (lambda () (interactive) (load-theme 'leuven t) (set-cursor-color "black")))   ; claro (fundo branco)
(global-set-key (kbd "C-<f11>") (lambda () (interactive) (load-theme 'greymatters t) (set-cursor-color "black"))) ; claro (fundo branco)
(global-set-key (kbd "C-<f12>") (lambda () (interactive) (load-theme 'white-sand t) (set-cursor-color "black"))) ; claro (fundo creme)


; Faz o load-theme desabilitar temas ativos antes de dar loading em um tema novo. (senão pode dar pau em algumas cores)

(defun disable-all-themes ()
  "disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))
 
(defadvice load-theme (before disable-themes-first activate)
  (disable-all-themes))



;;; Atalhos:
; Navega por buffers = Start+Alt + .//  ou Start+Alt + arrows
; Navega por janelas = Alt + .// ou Alt + arrows
; Tamanho do frame (Vertical, horizontal) = Ctrl+ +/-, Alt+ +/-
; Tamanho de janelas = Alt + Arrows do keypad (Num Lock ativado)
; Splita frame (À direita/embaixo) = Start+ right/down
; Fecha janela = Start+left ou F12
; Fecha todas as outras janelas = Start+up
; Kill line inteira e não só do ponto pra frente = Ctrl+Start + k


; Key bindings de movimentação pelas janelas
(global-set-key (kbd "M-<up>") 'windmove-up)
(global-set-key (kbd "M-<down>") 'windmove-down)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<left>") 'windmove-left)

(defun prev-window ()
  (interactive)
  (other-window -1))

(global-set-key (kbd "M-.") 'prev-window)
(global-set-key (kbd "M-/") 'other-window)

; Key bindings para o tamanho de janelas (com o frame dividido) 
(global-set-key (kbd "M-<kp-6>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<kp-4>") 'shrink-window-horizontally)
(global-set-key (kbd "M-<kp-8>") 'enlarge-window)
(global-set-key (kbd "M-<kp-2>") 'shrink-window)


; Key bindings para o tamanho de frames
(defun enlarge-frame ()
  (interactive)
  (set-frame-height (selected-frame) (+ (frame-height (selected-frame)) 2))
)

(defun shrink-frame ()
  (interactive)
  (set-frame-height (selected-frame) (- (frame-height (selected-frame)) 2))
)

(defun enlarge-frame-horizontally ()
  (interactive)
  (set-frame-width (selected-frame) (+ (frame-width (selected-frame)) 2))
)

(defun shrink-frame-horizontally ()
  (interactive)
  (set-frame-width (selected-frame) (- (frame-width (selected-frame)) 2))
)

(global-set-key (kbd "M-<kp-add>") 'enlarge-frame-horizontally)
(global-set-key (kbd "M-<kp-subtract>") 'shrink-frame-horizontally)
(global-set-key (kbd "C-<kp-add>") 'enlarge-frame)
(global-set-key (kbd "C-<kp-subtract>") 'shrink-frame)

; Key Bindings diversos ("s" = Windows/Start/Super key)
(global-set-key (kbd "s-M-<left>") 'previous-buffer)
(global-set-key (kbd "s-M-.") 'previous-buffer)
(global-set-key (kbd "s-M-<right>") 'next-buffer)
(global-set-key (kbd "s-M-/") 'next-buffer)
(global-set-key (kbd "s-<right>") 'split-window-right)
(global-set-key (kbd "s-<down>") 'split-window-below)
(global-set-key (kbd "s-<left>") 'delete-window)
(global-set-key (kbd "<f12>") 'delete-window)
(global-set-key (kbd "s-<up>") 'delete-other-windows)

(global-set-key (kbd "C-s-k") 'kill-whole-line)



; Usar MELPA para dar download em pacotes (M-x list-packages, package-install)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)


; Instalar "auto-complete" (M-x package-install [RET] auto-complete [RET])
; Pegar os nomes corretos dos diretórios se for instalar de novo
(add-to-list 'load-path "~/.emacs.d/auto-complete-20161029.643")
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete-20161029.643/dict")
(require 'auto-complete-config)
(ac-config-default)
(setq ac-auto-show-menu 0.1) ; Diminue o delay ao mostrar o menu de sugestões de auto-complete
(global-auto-complete-mode t)



; Character-based terminal settings (tty). A maioria das configurações vem do terminal. Problemas: cores, key bindings, velocidade do cursor.

(if (not (display-graphic-p))
  (progn
  (load-theme 'tty-dark t)
  (global-linum-mode 1)
  (setq linum-format "%d ")  ; adiciona um espaço entre o número e o início da linha
  (setq visible-cursor nil)  ; cursor sublinhado e não block
  )
)



; Pasta com meus arquivos de configuração relacionados a outros major modes.
(add-to-list 'load-path "~/.emacs.d/settings")

(require 'python-settings)
(require 'latex-settings)
