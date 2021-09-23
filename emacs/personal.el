;;; package --- Summary
;;; Commentary:
;;; Personal Emacs configuration to be used in conjunction with prelude
;;
;;; Code:
(setq user-full-name "Ivan V. Dimitrov"
      user-mail-address "ivan.v.dimitrov@pm.me")

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)

(projectile-register-project-type 'go '("go.mod")
                                  :compile "make clean build"
                                  :test "make clean build test"
                                  :test-suffix "_test")

;; Make the cursor blinking.
(blink-cursor-mode 1)

;; Remove the useless menu bar by default.
(menu-bar-mode -1)

;; Attempt to reload the buffers when they are edited outside emacs.
(global-auto-revert-mode t)

;; Set white space vertical colorization to be triggered after 250
;; symbol.
(setq whitespace-line-column 250)

;; Package with cool icons used by neotree.
(use-package all-the-icons
  :ensure t)

(use-package neotree
  :ensure t
  :config
  (setq neo-theme (if (display-graphic-p) 'icons))
  ;; Disable line-numbers minor mode for neotree
  (add-hook 'neo-after-create-hook
            (lambda (&rest _) (display-line-numbers-mode -1)))
  (add-hook 'neo-after-create-hook
            #'(lambda (_)
                (with-current-buffer (get-buffer neo-buffer-name)
                  (setq truncate-lines t)
                  (setq word-wrap nil)
                  (make-local-variable 'auto-hscroll-mode)
                  (setq auto-hscroll-mode nil))))
  :bind ([f8] . neotree-toggle))

;; (use-package dap-mode
;;   :ensure t)

;; (require 'dap-go)

;; (add-hook 'dap-stopped-hook
;;           (lambda (arg) (call-interactively #'dap-hydra)))

;; install ag or ack or the_silver_searcher package in OS
(use-package ag
  :ensure t)

;; smex is package that saves history of commands (in M-x for example).
(use-package smex
  :ensure t)

;;; ((( Custom printing snippets
(global-set-key (kbd "C-c C-c y") 'debug-print-golang)
(defun debug-print-golang ()
  "Insert Printing snippet."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent)
  (insert "fmt.Printf(\"\\n ====== debug ======: %+v \", )"))
;;; )))

;; ignore directories in the grep search
(eval-after-load "grep"
  '(progn
     (add-to-list 'grep-find-ignored-directories "vendor")))

(global-set-key (kbd "C-M-{") 'shrink-window-horizontally)
(global-set-key (kbd "C-M-}") 'enlarge-window-horizontally)

(global-set-key (kbd "C-x |") 'toggle-window-split)
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

;; wrap lines in the read-only buffers like Go-fmt etc.
(global-visual-line-mode t)

(key-chord-define-global "jj" 'avy-goto-char-timer)
(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-char-2)
(global-set-key (kbd "M-g f") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)

;; Passing command line argument to `go test' using `--' -> `-- -c ./config/dev.toml'.
;; dlv test stageai.tech/zzzax/pay/calendar/ -- -c ./config/dev.toml -test.run TestListEvents

;; (add-to-list 'default-frame-alist
             ;; '(font . "Fira Code-11"))

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

(global-set-key (kbd "C-x C-o") 'company-complete)
;; suggest previously used words in the buffer
(delete 'company-dabbrev company-backends)
;; disable autocompletion when typing
(setq company-idle-delay nil)

;; TODO test that
;; company-tooltip-align-annotations t

;; make scroll mouse nicer
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(0.07))

;;; ((( improve scrollers on other buffers.
(defun scroll-other-window-up-half ()
  (interactive)
  (scroll-other-window (window-half-height)))

(defun scroll-other-window-down-half ()
  (interactive)
  (scroll-other-window-down (window-half-height)))


(defun window-half-height ()
  (max 1 (/ (1- (window-height (selected-window))) 3)))

(global-set-key (kbd "<M-next>") 'scroll-other-window-up-half)
(global-set-key (kbd "<M-prior>") 'scroll-other-window-down-half)
;;; )))

;; Ugly workaround of the emacs-mac version which have that weird bug
;; that the GUI application looses focus when change spaces.
(dotimes (n 2)
  (toggle-frame-fullscreen))

;; markdown-mode custom command need any generating tool.
;;
;; For pandoc is good enough.
(custom-set-variables
 '(markdown-command "/usr/local/bin/pandoc"))

;; Fully-fledged terminal emulator
(use-package vterm
  :ensure t)

(use-package vterm-toggle
  :ensure t
  :bind ("C-x m" . vterm-toggle))

(defun base64-decode-utf8-region (start end)
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (base64-decode-region (point-min) (point-max))
    (decode-coding-region (point-min) (point-max) 'utf-8)))

(defun base64-encode-utf8-region (start end)
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (encode-coding-region (point-min) (point-max) 'utf-8)
    (base64-encode-region (point-min) (point-max))))

(global-set-key (kbd "C-c C-c e") 'base64-encode-utf8-region)
(global-set-key (kbd "C-c C-c d") 'base64-decode-utf8-region)

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\vendor\\'")
  ;; or
  ;;(add-to-list 'lsp-file-watch-ignored-files "[/\\\\]\\.my-files\\'")
  )

;; Ignore folders from being processed by projectile.
;; Changing to 'native is somehow necessary to successfully ignore folders.
(setq projectile-indexing-method 'native)
(add-to-list 'projectile-globally-ignored-directories "vendor")
(add-to-list 'projectile-globally-ignored-directories ".ci")

(setq prelude-whitespace nil)

(provide 'personal)
;;; personal.el ends here
