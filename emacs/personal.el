;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Personal Emacs  configuration to be used in conjunction with prelude   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:
;;; In Scratch M-x eval-buffer - executes the lisp in the current buffer.

;; Add line numbers
(global-linum-mode t)

;; Disable whitespace-mode
(setq prelude-whitespace nil)

;; Attempt to reload the buffers when they are edited outside emacs
(global-auto-revert-mode t)

;; bind 'f8' to neo tree toggle
(global-set-key [f8] 'neotree-toggle)

;; (go-guru-hl-identifier-mode) or highlight the occurrence when cursor is on.
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

;; s package for test manipulation
;; https://github.com/magnars/s.el
(unless (package-installed-p 's)
  (package-refresh-contents)
  (package-install 's))

;; install ag or ack or the_silver_searcher package in OS
(unless (package-installed-p 'ag)
  (package-refresh-contents)
  (package-install 'ag))
;; install graphql-mode


;; bind the sugegstion window to keybind and stops autosuggestion
(global-set-key (kbd "C-x C-o") 'company-complete)
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
  (insert-print))

(defun insert-print ()
    "Insert marking print."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent)
  (insert "fmt.Printf(\"\\n debugging: %#v \\n\", )"))

;; ignore directories in the grep search
(eval-after-load "grep"
  '(progn
     (add-to-list 'grep-find-ignored-directories "vendor")))


(defun toggle-window-split ()
  "Vertical split shows more of each line, horizontal split shows more lines.
This code toggles between them.
It only works for frames with exactly two windows.
The top window goes to the left or vice-versa."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x |") 'toggle-window-split)

(setq-default fill-column 79)

;; (load-theme 'nord t1)

(global-set-key (kbd "C-x m") 'ansi-term)

(setq max-specpdl-size 650)
(setq max-lisp-eval-depth 400)

;; (setenv "GO111MODULE" "on")
(setenv "SUNSHINE_ENV" "dev")

;; wrap lines in the read-only buffers like Go-fmt etc.
(global-visual-line-mode t)

;; example of a function that just insert a tab char
(defun insert-tab-char ()
  "Insert a tab char. (ASCII 9, \t)."
  (interactive)
  (insert "\t"))

(global-set-key (kbd "C-i") 'insert-tab-char) ; same as Ctrl+i

(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-char-2)
(global-set-key (kbd "M-g f") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)

;; Passing command line argument to `go test' using `--' -> `-- -c ./config/dev.toml'.
;; dlv test stageai.tech/zzzax/pay/calendar/ -- -c ./config/dev.toml -test.run TestListEvents

;; (add-to-list 'default-frame-alist
;;              '(font . "Monoid-10"))

(add-to-list 'default-frame-alist
             '(font . "Fira Code Retina-11"))

(doom-modeline-mode 1)
(doom-themes-neotree-config)

(global-set-key (kbd "C-<f8>") 'helm-semantic-or-imenu)
(global-set-key (kbd "M-<f8>") 'minimap-mode)

;; (add-hook 'after-save-hook #'projectile-regenerate-tags)
;;(package-install 'ctags-update)
;; (ctags-global-auto-update-mode)
;; (setq ctags-update-prompt-create-tags nil);you need manually create TAGS in your project

(provide 'personal)
;;; personal.el ends here
