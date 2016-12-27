;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Configurações do modo python ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Pacotes a instalar (apt):
; python-pip, python-pandas, python-wxtools (para rodar o backend wx)
; Estou usando python 2.7 com ipython 4 (instalado na mão com o .tar, porque no ipython 5 a shell dá pau dentro do emacs)

; Tem 2 modos python: python-mode.el (com comandos que começam com py-) e python.el (com comandos começam com python-). Estou usando o python.el, que já vem com o emacs.


(setq
  python-shell-interpreter "ipython"
  python-shell-interpreter-args "--gui=wx --matplotlib=wx --colors=Linux"
  python-shell-prompt-regexp "In \\[[0-9]+\\]: "
  python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
  python-shell-completion-setup-code
    "from IPython.core.completerlib import module_completion"
  python-shell-completion-module-string-code
    "';'.join(module_completion('''%s'''))\n"
  python-shell-completion-string-code
  "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"
)


; Character-based terminal settings (tty). No terminal não tem o wx.
(if (not (display-graphic-p))
  (setq python-shell-interpreter-args "--colors=Linux")
)


; Divide a tela na vertical, abre o arquivo em um buffer na janela da direita (com foco) e a shell do ipython na esquerda.
(defun my-eval-after-load-python (buffer alist direction &optional size pixelwise)

  (setq initial-frame-alist '((top . 44) (left . 18) (width . 135) (height . 49)))  ; ; Posição e tamanho inicial do frame (Na resolução de 1280x1024)

  (let ((window (split-window (selected-window) size direction pixelwise)))
    (window--display-buffer buffer window 'window alist display-buffer-mark-dedicated)
    (run-python (python-shell-parse-command))
    (other-window 1)
    (python-shell-switch-to-shell)
    (select-window window)
  )
)

(eval-after-load "python" '(my-eval-after-load-python (current-buffer) nil 'right (floor (* 0.49 (window-width)))))



;;; Atalhos:
; F5 : Executa todo o buffer
; C-c C-s : Executa só região selecionada

(add-hook 'python-mode-hook
  (lambda ()
    (define-key python-mode-map (kbd "<f5>") 'python-shell-send-buffer)
    (define-key python-mode-map (kbd "C-c C-s") 'python-shell-send-region)
    (define-key inferior-python-mode-map (kbd "C-<up>") 'previous-line)
    (define-key inferior-python-mode-map (kbd "C-<down>") 'next-line)
    (define-key inferior-python-mode-map (kbd "<up>") 'comint-previous-input) 
    (define-key inferior-python-mode-map (kbd "<down>") 'comint-next-input)
  )
)


; Não pergunta: “Active processes exist; kill them and exit anyway (y or n)” quando fecha o emacs (O processo Python do buffer *Python* rodando no /dev/pts/N referente a /usr/local/bin/ipython --gui=wx --matplotlib=wx --colors=linux)
(add-hook 'comint-exec-hook
  (lambda () 
    (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil))
)


(provide 'python-settings)
