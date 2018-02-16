;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Personal Emacs  configuration to be used in conjunction with prelude   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

;; Add line numbers
(global-linum-mode t)

;; Disable whitespace-mode
(setq prelude-whitespace nil)

;; (setq whitespace-line-column 120) ;; limit line length

;; Attempt to reload the buffers when they are edited outside emacs
(global-auto-revert-mode t)

(global-set-key [f8] 'neotree-toggle)

;; (go-guru-hl-identifier-mode)
(add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)

;;install neotree
;;install ag.el

 (provide 'personal)
;;; personal.el ends here
