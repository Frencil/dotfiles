(global-font-lock-mode t)
(setq-default transient-mark-mode t)

; mouse wheel scrolling
(defun sd-mousewheel-scroll-up (event)
  "Scroll window under mouse up by five lines."
  (interactive "e")
  (let ((current-window (selected-window)))
    (unwind-protect
	(progn 
	  (select-window (posn-window (event-start event)))
	  (scroll-up 2))
      (select-window current-window))))
(defun sd-mousewheel-scroll-down (event)
  "Scroll window under mouse down by five lines."
  (interactive "e")
  (let ((current-window (selected-window)))
    (unwind-protect
	(progn 
	  (select-window (posn-window (event-start event)))
	  (scroll-down 2))
      (select-window current-window))))
 
(global-set-key (kbd "<mouse-5>") 'sd-mousewheel-scroll-up)
(global-set-key (kbd "<mouse-4>") 'sd-mousewheel-scroll-down)

; programming modes
(require 'php-mode)
(require 'html-helper-mode)
(require 'css-mode)
(custom-set-variables '(inhibit-startup-screen t))
(custom-set-faces)

; hook to force javascript tab sizes to two spaces
(add-hook 'javascript-mode-hook 
  '(lambda() 
    (setq tab-width 4)))

; define tabs as two spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)
(put 'upcase-region 'disabled nil)

; key bindings
(global-set-key "\C-l" 'goto-line)
(global-set-key [f2] 'split-window-vertically) 

; Org-mode stuff
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

; Tramp-mode (remote editing)
(setq tramp-default-method "ssh")

; Smaller monospaced font
(set-face-attribute 'default nil :height 100)
