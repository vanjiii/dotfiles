;;; package --- Summary
;;; Commentary:
;;; Personal Emacs  configuration to be used in conjunction with prelude   ;;

;;; Code:

(setq user-full-name "Ivan V. Dimitrov"
      user-mail-address "ivan.v.dimitrov@pm.me")

(load-theme 'nordless t)

(projectile-register-project-type 'go '("go.mod")
                                  :compile "make clean build"
                                  :test "make clean build test"
                                  :test-suffix "_test")

;; Remove the useless menu bar by default.
(menu-bar-mode -1)

;; Disable whitespace-mode
(setq prelude-whitespace nil)
(setq whitespace-line-column 250)

;; Attempt to reload the buffers when they are edited outside emacs
(global-auto-revert-mode t)

;; (go-guru-hl-identifier-mode) or highlight the occurrence when cursor is on.
(add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)

;; install neotree
(unless (package-installed-p 'neotree)
  (package-refresh-contents)
  (package-install 'neotree))

;; neotree config
;; bind 'f8' to neo tree toggle
(global-set-key [f8] 'neotree-toggle)
;; wrap long lines within neotree
(add-hook 'neo-after-create-hook
          #'(lambda (_)
              (with-current-buffer (get-buffer neo-buffer-name)
                (setq truncate-lines t)
                (setq word-wrap nil)
                (make-local-variable 'auto-hscroll-mode)
                (setq auto-hscroll-mode nil))))

;; install ag or ack or the_silver_searcher package in OS
(unless (package-installed-p 'ag)
  (package-refresh-contents)
  (package-install 'ag))

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
  (insert "fmt.Printf(\"\\n ====== debug ======: %#v \\n\", )"))

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
(global-set-key (kbd "C-;") 'avy-goto-char-timer)
(global-set-key (kbd "C-'") 'avy-goto-char-2)
(global-set-key (kbd "M-g f") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)

;; Passing command line argument to `go test' using `--' -> `-- -c ./config/dev.toml'.
;; dlv test stageai.tech/zzzax/pay/calendar/ -- -c ./config/dev.toml -test.run TestListEvents

(add-to-list 'default-frame-alist
             '(font . "Fira Code-11"))

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

;; use lsp server by default for go development.
(add-hook 'go-mode-hook 'lsp-deferred)

;; increase the amount of data that emacs read from server.
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; disable ui doc frame and bind it ot keybind
(setq lsp-ui-doc-enable nil)

;; usefull lsp keybinds
(global-set-key (kbd "C-c C-l ,") 'lsp-ui-doc-show)
(global-set-key (kbd "C-c C-l r") 'lsp-rename)
(global-set-key (kbd "C-c C-l d") 'lsp-describe-thing-at-point)
(global-set-key (kbd "C-c C-l x") 'lsp-restart-workspace)

(setq lsp-ui-sideline-show-diagnostics nil)
(setq lsp-ui-sideline-show-hover nil)
(setq lsp-ui-sideline-show-code-actions nil)
(setq lsp-ui-sideline-update-mode nil)
(setq lsp-ui-sideline-delay nil)

(use-package company-box
  :after company
  :hook (company-mode . company-box-mode))

(global-set-key (kbd "C-x C-o") 'company-complete)

;; mode line
(setq-default mode-line-format
              (list

               ;; line and column
               " " ;; '%02' to set to 2 chars at least; prevents flickering
               (propertize "Ln:%02l " 'face 'font-lock-constant-face)
               (propertize "Col:%02c" 'face 'font-lock-constant-face)
               "  "

               ;; the buffer name; the file name as a tool tip
               '(:eval (propertize " %b " 'face 'font-lock-constant-face
                                   'help-echo (buffer-file-name)))

               ;; relative position, size of file
               "["
               (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
               "/"
               (propertize "%I" 'face 'font-lock-constant-face) ;; size
               "] "

               ;; the current major mode for the buffer.
               "["

               '(:eval (propertize "%m" 'face 'font-lock-constant-face
                                   'help-echo "The current major mode for the buffer"))
               "] "


               "[" ;; insert vs overwrite mode, input-method in a tooltip
               '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
                                   'face 'font-lock-constant-face
                                   'help-echo (concat "Buffer is in "
                                                      (if overwrite-mode "overwrite" "insert") " mode")))

               ;; was this buffer modified since the last save?
               '(:eval (when (buffer-modified-p)
                         (concat ","  (propertize "Mod"
                                                  'face 'magit-diff-removed-highlight
                                                  'help-echo "Buffer has been modified"))))

               ;; is this buffer read-only?
               '(:eval (when buffer-read-only
                         (concat ","  (propertize "RO"
                                                  'face 'font-lock-keyword-face
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

;; make scroll mouse nicer
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(0.07))

;; change color of modeline
(set-face-background 'mode-line "#4C566A")
(set-face-background 'mode-line-inactive "#4C566A")

(provide 'personal)
;;; personal.el ends here
