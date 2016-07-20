;; paladim.el -- Emacs major mode for the Paladim language.
;;
;; INSTALLATION
;;
;; In order to make the command `paladim-mode' work and auto-enable for Paladim
;; files, you need to put this file in your load path and put this in you .emacs
;; file:
;;
;;   (require 'paladim)
;;   (add-to-list 'auto-mode-alist '("\\.pal$" . paladim-mode))
;;
;;
;; COMMANDS
;;
;; C-c C-e:  Compile the program in the current buffer.  Prints errors if they
;;           occur.
;; C-c C-i:  Interpret the current buffer (note: does not work if it asks for
;;           input)


(define-derived-mode paladim-mode pascal-mode "PALADIM"
  "PALADIM is the Pascal variant used in the Compilers 2013 course."
  (setq-local comment-start "//")
  (setq-local comment-end "")
  (modify-syntax-entry ?/ ". 12b" paladim-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" paladim-mode-syntax-table)
  (modify-syntax-entry ?{ "(" paladim-mode-syntax-table)
  (modify-syntax-entry ?} ")" paladim-mode-syntax-table)
  )

(defun paladim-run-current-buffer (flag)
  (interactive)
  (setq buf-name (buffer-file-name))
  (setq root-dir (concat (file-name-directory buf-name) "/.."))
  (shell-command (concat root-dir "/BIN/Paladim " flag " "
                         (shell-quote-argument buf-name))))

(defun paladim-compile-current-buffer ()
  (interactive)
  (paladim-run-current-buffer "-c"))

(defun paladim-interpret-current-buffer ()
  (interactive)
  (paladim-run-current-buffer "-ti"))

(define-key paladim-mode-map
  (kbd "C-c C-e") 'paladim-compile-current-buffer)
(define-key paladim-mode-map
  (kbd "C-c C-i") 'paladim-interpret-current-buffer)

(provide 'paladim)
