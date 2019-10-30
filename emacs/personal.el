;;; package --- Summary
;;; Commentary:
;;; Personal Emacs  configuration to be used in conjunction with prelude   ;;

;;; Code:
;;; In Scratch M-x eval-buffer - executes the lisp in the current buffer.

(projectile-register-project-type 'go '("go.mod")
                                  :compile "make clean build"
                                  :test "make clean build test"
                                  :test-suffix "_test")

(disable-theme 'zenburn)
(load-theme 'eink t)

(custom-theme-set-faces
 `eink
 `(go-guru-hl-identifier-face ((t (:weight bold)))))

;; Add line numbers
(global-linum-mode t)

;; Disable whitespace-mode
(setq prelude-whitespace nil)
(setq whitespace-line-column 250)

;; Attempt to reload the buffers when they are edited outside emacs
(global-auto-revert-mode t)

;; bind 'f8' to neo tree toggle
(global-set-key [f8] 'neotree-toggle)

;; (go-guru-hl-identifier-mode) or highlight the occurrence when cursor is on.
(add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)

;; emacs-go-tag configuration
;; snakecase: BaseDomain -> base_domain
;; camelcase: BaseDomain -> baseDomain
;; lispcase: BaseDomain -> base-domain
(setq go-tag-args (list "-transform" "snakecase"))

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

;; bind the sugegstion window to keybind and stops autosuggestion
(global-set-key (kbd "C-x C-o") 'company-complete)
(setq company-idle-delay nil)

;; Add scopes for go guru examples
;; After adding GOPATH to emacs add the package
;; The first is the project package, the second is the packages to ignore
;; git.yatrusanalytics.com/yatrus/sunshine/...,-git.yatrusanalytics.com/yatrus/sunshine/vendor/...

(global-set-key (kbd "C-c C-c y") 'debug-print)

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
  "Vertical split show more of each line, horizontal split show more lines.
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

;; (setq-default fill-column 79)

(global-set-key (kbd "C-x m") 'ansi-term)

(setq max-specpdl-size 650)
(setq max-lisp-eval-depth 400)

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

(add-to-list 'default-frame-alist
             '(font . "Fira Code-11"))

(global-set-key (kbd "C-<f8>") 'helm-semantic-or-imenu)
(global-set-key (kbd "M-<f8>") 'minimap-mode)

(defun sudo-save ()
  "Execute sudo save for files that user do not own."
  (interactive)
  (if (not buffer-file-name)
      (write-file (concat "/sudo:root@localhost:" (ido-read-file-name "File:")))
    (write-file (concat "/sudo:root@localhost:" buffer-file-name))))

(require 'diminish)
(diminish 'ivy-mode)
(diminish 'company-mode)
(diminish 'projectile-mode)
(diminish 'super-save-mode)
(diminish 'editorconfig-mode)
(diminish 'flycheck-mode)
(diminish 'flyspell-mode)
(diminish 'whitespace-mode)
(diminish 'which-key-mode)
(diminish 'guru-mode)
(diminish 'guru-global-mode)
(diminish 'guru-mode)

(provide 'personal)
;;; personal.el ends here
