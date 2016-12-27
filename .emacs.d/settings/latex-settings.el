;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Configurações do modo latex ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Pacotes a instalar (apt):
; auctex, texlive-base (já tem), texlive-lang-portuguese (para poder usar [brazil]{babel}), texlive-fonts-recommended (resolveu outro pau de fontes que estava dando)


(setq TeX-auto-save t)         ; Enable parse on save
(setq TeX-parse-self t)        ; Enable parse on load
(setq-default TeX-master nil)  ; Query for master file

; Caching the preamble will significantly speed up regeneration of previews
(setq preview-auto-cache-preamble nil)

; Don't query the user before saving each file with TeX-save-document
(setq TeX-save-query nil)

(setq TeX-PDF-mode t)                    ; Usa pdflatex
(setq-default TeX-command-Show "LaTeX")  ; default do C-c C-c é latex e não view



(defun my-eval-after-load-latex()

; Posição e tamanho inicial do frame (Na resolução de 1280x1024)
  (setq initial-frame-alist '((top . 44) (left . 20) (width . 135) (height . 49)))

)

(eval-after-load "tex" '(my-eval-after-load-latex))



;;; Atalhos:
; C-c C-s : Preview da seção
; C-c C-c : Salva e compila
; C-c C-v : Tex-view (Abre o evince com o pdf compilado)
; C-c C-d / C-c C-z : Vê erros de compilação (next/previous) [O previous tem bug até certa versão do auctex (2016)]


; Salva (master e todos do input) e roda o TeX-master-file sem perguntar qual comando usar (sempre usa o "LaTeX")
(defun rl-save-and-LaTeX ()
  (interactive)
  (let (TeX-save-query)
    (TeX-save-document (TeX-master-file))
  )
  (TeX-command "LaTeX" 'TeX-master-file)
)

(add-hook 'TeX-mode-hook
  (lambda ()
    (define-key TeX-mode-map (kbd "C-c C-c") 'rl-save-and-LaTeX)
    (define-key LaTeX-mode-map (kbd "C-c C-s") nil)  ; tira o LaTeX-section
    (define-key LaTeX-mode-map (kbd "C-c C-s") 'preview-section)
    (define-key TeX-mode-map (kbd "C-c C-d") 'TeX-next-error)
    (define-key TeX-mode-map (kbd "C-c C-z") 'TeX-previous-error)
  )
)


(provide 'latex-settings)
