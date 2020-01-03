;;; package --- Summary
;;; Commentary:
;;; Personal Emacs  configuration to be used in conjunction with prelude   ;;

;;; Code:

(projectile-register-project-type 'go '("go.mod")
                                  :compile "make clean build"
                                  :test "make clean build test"
                                  :test-suffix "_test")

;; Add line numbers
(global-linum-mode t)
(setq linum-format "%4d ")

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

(defun mode-line-fill (face reserve)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (when
      (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
              'display
              `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))
              'face face
              )
  )

;; mode line
(setq-default mode-line-format
              (list

               ;; line and column
               " " ;; '%02' to set to 2 chars at least; prevents flickering
               (propertize "Row:%02l " 'face 'font-lock-type-face)
               (propertize "Col:%02c" 'face 'font-lock-type-face)
               "  "

               ;; the buffer name; the file name as a tool tip
               '(:eval (propertize " %b " 'face 'font-lock-keyword-face
                                   'help-echo (buffer-file-name)))

               ;; relative position, size of file
               "["
               (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
               "/"
               (propertize "%I" 'face 'font-lock-constant-face) ;; size
               "] "

               ;; the current major mode for the buffer.
               "["

               '(:eval (propertize "%m" 'face 'font-lock-string-face
                                   'help-echo "The current major mode for the buffer"))
               "] "


               "[" ;; insert vs overwrite mode, input-method in a tooltip
               '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
                                   'face 'font-lock-preprocessor-face
                                   'help-echo (concat "Buffer is in "
                                                      (if overwrite-mode "overwrite" "insert") " mode")))

               ;; was this buffer modified since the last save?
               '(:eval (when (buffer-modified-p)
                         (concat ","  (propertize "Mod"
                                                  'face 'font-lock-warning-face
                                                  'help-echo "Buffer has been modified"))))

               ;; is this buffer read-only?
               '(:eval (when buffer-read-only
                         (concat ","  (propertize "RO"
                                                  'face 'font-lock-type-face
                                                  'help-echo "Buffer is read-only"))))
               "] "

               '(vc-mode vc-mode)

               ;; right align
               (mode-line-fill 'mode-line 20)

               '(:eval (propertize (emacs-uptime "Uptime: %hh ")))

               ;; add the time, with the date and the emacs uptime in the tooltip
               '(:eval (propertize (format-time-string "%H:%M")
                                   'help-echo
                                   (concat (format-time-string "%c; ")
                                           (emacs-uptime "Uptime:%hh"))))
               ))

(global-set-key (kbd "C-c p s h") 'helm-ag)

(provide 'personal)
;;; personal.el ends here
