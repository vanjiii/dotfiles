;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Personal Emacs  configuration to be used in conjunction with prelude   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:
;;; In Scratch M-x eval-buffer - executes the lisp in the current buffer.

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

;; Set global env for emacs
(add-to-list ' exec-path "/home/vanjiii/go/bin")

;; emacs-go-tag configuration
;; snakecase: BaseDomain -> base_domain
;; camelcase: BaseDomain -> baseDomain
;; lispcase: BaseDomain -> base-domain
(setq go-tag-args (list "-transform" "snakecase"))
;; check if exists go-tag and install it if necessary
(unless (package-installed-p 'go-tag)
  (package-refresh-contents)
  (package-install 'go-tag))

;; install neotree
(unless (package-installed-p 'neotree)
  (package-refresh-contents)
  (package-install 'neotree))
;; install ag.el
;; install ag or ack or the_silver_searcher package in OS
;; install graphql-mode


;; bind the sugegstion window to keybind and stops autosuggestion
(global-set-key (kbd "C-s-SPC") 'company-complete)
(setq company-idle-delay nil)

;; Add scopes for go guru examples
;; After adding GOPATH to emacs add the package
;; The first is the project package, the second is the packages to ignore
;; git.yatrusanalytics.com/yatrus/sunshine/...,-git.yatrusanalytics.com/yatrus/sunshine/vendor/...

(global-set-key (kbd "C-c C-c y") 'debug-print)
(global-set-key (kbd "C-c C-c e") 'check-error)

;; add condition if statement is not nil

(defun check-error()
  "Insert err check under marked word."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent)
  (insert "if err != nil {")
  (newline-and-indent)
  (insert "return err ")
  (newline-and-indent)
  (insert "}")
  (indent-for-tab-command))


(defun debug-print ()
  "Insert Printing snippet."
  (interactive)
  (insert-print)
  (insert-arg-print))

(defun insert-print ()
    "Insert marking print."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent)
  (insert "fmt.Print(\"=========================\")"))

;; (thing-at-point 'word 'no-properties)

(defun insert-arg-print ()
  "Insert Print with debugging parameter."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent)
  (insert "fmt.Println()"))

(provide 'personal)
;;; personal.el ends here
