;;------------------------------------------------------------
;; themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'manoj-dark t)

;;------------------------------------------------------------
;; global key bindings
(global-set-key "\C-co" 'compile)
(global-set-key "\C-cl" 'ag)
(global-set-key "\C-c\C-c" 'comment-region)
(global-set-key "\C-c\C-b" 'browse-url)

(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'ibuffer) (defalias 'list-buffers 'ibuffer))

;;------------------------------------------------------------
;; sane defaults
(display-time)
(global-font-lock-mode t)
(global-auto-revert-mode 1)
(auto-compression-mode t)
(delete-selection-mode 1)
(global-subword-mode 1) 

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default sentence-end-double-space nil)
(setq-default indicate-empty-lines t) 
(setq-default truncate-lines t)             

(setq inhibit-startup-message t)
(setq line-number-mode t)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(setq column-number-mode t)
(setq fill-column 80)
(setq x-select-enable-clipboard t)    
(setq global-auto-revert-non-file-buffers t)
(setq jump-char-lazy-highlight-face nil)    
(setq echo-keystrokes 0.1)                  
(setq delete-by-moving-to-trash t)          
(setq gc-cons-threshold 20000000)           
(setq auto-revert-verbose nil)
(setq vc-make-backup-files t)        ; make backup files even in version control

(setq bmkp-last-as-first-bookmark-file "~/.emacs.d/.bookmarks")
(setq backup-directory-alist `(("." . "~/.emacs.d/.backups")))

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;------------------------------------------------------------
;; Save point position between sessions
;; (require 'saveplace)
;; (setq-default save-place t)
;; (setq save-place-file (expand-file-name ".places" user-emacs-directory))

(add-to-list 'load-path user-emacs-directory)
;; (add-to-list 'load-path (concat user-emacs-directory "site-lisp"))
;; (add-to-list 'load-path (concat user-emacs-directory "el-get/el-get"))

;;------------------------------------------------------------
;; setup ELPA, defines: require-package
(require 'setup-package)

;;------------------------------------------------------------
;; os x
(when (equal system-type 'darwin)
  (require-package 'exec-path-from-shell)
  (exec-path-from-shell-initialize)
  ;; ; brew install aspell --lang=en
  (setq ispell-program-name "/usr/local/bin/aspell")
)

;;------------------------------------------------------------
;; 
(require-package 'ag)
(require-package 'rinari)
(require-package 'magit)
(require-package 'fold-dwim)
(require-package 'bookmark+)

;;------------------------------------------------------------
;; recent files
(add-hook 'recentf-mode-hook 
          (lambda ()
            (setq recentf-max-saved-items 25)
            (global-set-key "\C-cf" 'recentf-open-files)))
(recentf-mode 1)

;;------------------------------------------------------------
;; folding mode hooks
(add-hook 'hs-minor-mode-hook 
          (lambda ()
            (defun display-code-line-counts (ov)
              (when (eq 'code (overlay-get ov 'hs))
                (overlay-put ov 'display
                             (format " ...%d... "
                                     (count-lines (overlay-start ov)
                                                  (overlay-end ov))))))
            (setq hs-set-up-overlay 'display-code-line-counts)
            (local-set-key "\C-c_" 'hs-hide-all)
            (local-set-key "\C-c+" 'hs-show-all)
            (local-set-key "\C-c\C-_" 'hs-toggle-hiding)
            (local-set-key "\C-c=" 'hs-show-block)
            (local-set-key "\C-c-" 'hs-hide-block)))

;;------------------------------------------------------------
;; ruby mode hooks
(add-hook 'ruby-mode-hook
	  (lambda ()
	    ;; (rvm-activate-corresponding-ruby)
	    ;; (ruby-electric-mode)
	    (add-to-list 'hs-special-modes-alist
			 '(ruby-mode
			   "\\(def\\|do\\|{\\)"
			   "\\(end\\|}??\\)"
			   "#"
			   (lambda (arg) (ruby-end-of-block)) nil))
	    (hs-minor-mode 1)
	    (setq rinari-tags-file-name "TAGS")))


;;------------------------------------------------------------
;; dired mode hooks
(add-hook 'dired-mode-hook
          '(lambda ()
             (set (make-local-variable 'ido-enable-replace-completing-read) nil)))

;;------------------------------------------------------------
;; ido mode hooks
(ido-mode t)

;; install extensions if they're missing
(defun init--install-packages ()
  (packages-install
   '(
     ;; ac-inf-ruby
     ;; ac-js2
     ;; ac-slime
     ag
     anything
     auto-complete
     bash-completion
     browse-kill-ring
     change-inner
     cider
     clojure-mode
     coffee-mode
     css-eldoc
     dash
     dash-at-point
     direx
     edbi
     ;; elisp-slime-nav
     enh-ruby-mode
     epl
     eproject
     exec-path-from-shell
     expand-region
     fill-column-indicator
     find-file-in-project
     flx
     flx-ido
     flycheck
     haml-mode
     highlight-escape-sequences
     highlight-indentation
     htmlize
     ido-at-point
     ido-ubiquitous
     ido-vertical-mode
     idomenu
     inf-ruby
     js2-mode
     js2-refactor
     jump-char
     magit
     move-text
     multifiles
     multiple-cursors
     nodejs-repl
     org
     org-plus-contrib
     ;; paredit
     ;; pkg-info
     ;; popwin
     ;; pretty-symbols
     ;; projectile
     ;; project-explorer
     ;; rainbow-mode
     ;; robe
     ;; restclient
     ;; rsense
     rvm 
     ;; skewer-less
     ;; skewer-mode
     ;; slim-mode
     ;; smart-forward
     ;; smartparens
     ;; smex
     ;; smooth-scrolling
     ;; tagedit
     ;; visual-regexp
     ;; visual-regexp-steroids
     ;; w3m
     ;; web-mode
     ;; yasnippet
     ;; zenburn-theme
     )))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))


;;------------------------------------------------------------------------------
;; EL-GET
;;
;; (setq el-get-user-package-directory (expand-file-name "el-get-init-files" user-emacs-directory))
;; (unless (require 'el-get nil 'noerror)
;;   (with-current-buffer
;;       (url-retrieve-synchronously
;;        "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
;;     (let (el-get-master-branch)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp))))
;; (el-get 'sync)
;; ;; installed packages not in melpa
;; (require 'el-get)
;; (setq my-packages
;;       (append
;;        '(
;;          ri-emacs
;;          rcodetools
;;          ruby-electric
;;          ;;auto-complete
;;                                         ;deferred 
;;                                         ;epc
;;                                         ;ctable
;;                                         ;anything
;;                                         ;anything-show-completion
;;                                         ;anything-rcodetools
;;          )
;;        (mapcar 'el-get-source-name el-get-sources)))
;; (el-get-cleanup my-packages)
;; (el-get 'sync my-packages)
;;
;; END EL-GET
;;------------------------------------------------------------------------------




;; ;; Start autocomplete
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;; (ac-config-default)
;; (add-to-list 'ac-modes 'enh-ruby-mode)
;; (add-to-list 'ac-modes 'web-mode)

;; ;; Setup inf-ruby
;; (autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
;; (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
;; (add-hook 'after-init-hook 'inf-ruby-switch-setup)

;; ;; Setup ac-inf-ruby
;; (eval-after-load 'auto-complete
;;   '(add-to-list 'ac-modes 'inf-ruby-mode))
;; (add-hook 'inf-ruby-mode-hook 'ac-inf-ruby-enable)

;; ;; Setup coffee-mode
;; (load "setup-coffee-mode.el")

;; ;; Setup display time
;; (setq display-time-24hr-format t)
;; (display-time)

;; ;; Setup edbi
;; (setenv "PERL5LIB" (concat "/Users/" user-login-name "/perl5/lib/perl5"))

;; ;; Setup popwin
;; (require 'popwin)
;; (custom-set-variables
;;  '(display-buffer-function 'popwin:display-buffer))

;; ;; Setup rcodetools
;; (require 'rcodetools)

;; ;; Setup rinari
;; (require 'rinari)

;; (add-hook 'ruby-mode-hook 'robe-mode)
;; (add-hook 'robe-mode-hook 'robe-ac-setup)

;; (add-hook 'robe-mode-hook 'robe-start)

;; ;; Setup skewer mode
;; (add-hook 'js2-mode-hook 'skewer-mode)
;; (add-hook 'css-mode-hook 'skewer-css-mode)
;; (add-hook 'html-mode-hook 'skewer-html-mode)

;; ;; Setup slime
;; ;;(load (expand-file-name "~/quicklisp/slime-helper.el"))
;; (setq inferior-lisp-program "sbcl")

;; ;; (require 'ac-slime)
;; ;; (add-hook 'slime-mode-hook 'set-up-slime-ac)
;; ;; (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
;; ;; (eval-after-load "auto-complete"
;; ;;   '(add-to-list 'ac-modes 'slime-repl-mode))

;; ;; Lets start with a smattering of sanity
;; (require 'sane-defaults)

;; (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
;; (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; ;; guide-key
;; (require 'guide-key)
;; (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-x v" "C-x 8" "C-x +"))
;; (guide-key-mode 1)
;; (setq guide-key/recursive-key-sequence-flag t)
;; (setq guide-key/popup-window-position 'bottom)

;; ;; Setup extensions
;; (eval-after-load 'ido '(require 'setup-ido))
;; (require 'setup-org)
;; ;;(eval-after-load 'dired '(require 'setup-dired))
;; (eval-after-load 'magit '(require 'setup-magit))
;; (eval-after-load 'grep '(require 'setup-rgrep))
;; (eval-after-load 'shell '(require 'setup-shell))
;; ;(require 'setup-hippie)
;; ;(require 'setup-yasnippet)
;; ;(require 'setup-perspective)
;; (require 'setup-ffip)
;; (require 'setup-html-mode)
;; ;(require 'setup-paredit)

;; ;; Font lock dash.el
;; (eval-after-load "dash" '(dash-enable-font-lock))

;; ;; Default setup of smartparens
;; (require 'smartparens-config)
;; (setq sp-autoescape-string-quote nil)
;; (--each '(css-mode-hook
;;           restclient-mode-hook
;;           js-mode-hook
;;           java-mode
;;           ruby-mode
;;           markdown-mode
;;           groovy-mode)
;;   (add-hook it 'turn-on-smartparens-mode))

;; ;; Language specific setup files
;; (eval-after-load 'js2-mode '(require 'setup-js2-mode))
;; (eval-after-load 'ruby-mode '(require 'setup-ruby-mode))
;; (eval-after-load 'clojure-mode '(require 'setup-clojure-mode))
;; (eval-after-load 'markdown-mode '(require 'setup-markdown-mode))

;; ;; Load stuff on demand
;; (autoload 'skewer-start "setup-skewer" nil t)
;; (autoload 'skewer-demo "setup-skewer" nil t)
;; (autoload 'flycheck-mode "setup-flycheck" nil t)
;; (autoload 'auto-complete-mode "auto-complete" nil t)

;; ;; Map files to modes
;; (require 'mode-mappings)

;; ;; Highlight escape sequences
;; (require 'highlight-escape-sequences)
;; (hes-mode)
;; (put 'font-lock-regexp-grouping-backslash 'face-alias 'font-lock-builtin-face)

;; ;; Visual regexp
;; (require 'visual-regexp)
;; (define-key global-map (kbd "M-&") 'vr/query-replace)
;; (define-key global-map (kbd "M-/") 'vr/replace)

;; ;; Functions (load all files in defuns-dir)
;; (setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
;; (dolist (file (directory-files defuns-dir t "\\w+"))
;;   (when (file-regular-p file)
;;     (load file)))

;; (require 'expand-region)
;; (require 'multiple-cursors)
;; (require 'delsel)
;; (require 'jump-char)
;; (require 'eproject)
;; (require 'wgrep)
;; (require 'smart-forward)
;; (require 'change-inner)
;; (require 'multifiles)

;; ;; Fill column indicator
;; (require 'fill-column-indicator)
;; (setq fci-rule-color "#111122")

;; ;; Browse kill ring
;; (require 'browse-kill-ring)
;; (setq browse-kill-ring-quit-action 'save-and-restore)

;; ;; Smart M-x is smart
;; (require 'smex)
;; (smex-initialize)

;; ;; Projectile everywhere
;; (projectile-global-mode)

;; ;; Elisp go-to-definition with M-. and back again with M-,
;; ;; (autoload 'elisp-slime-nav-mode "elisp-slime-nav")
;; ;; (add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t) (eldoc-mode 1)))

;; ;; Fix whitespace on save, but only if the file was clean
;; (global-whitespace-cleanup-mode)

;; (require 'multiple-cursors)

;; ;; Show line numbers for all files
;; ;(add-hook 'find-file-hook (lambda () (linum-mode 1)))

;; ;; Conclude init by setting up specifics for the current user
;; (when (file-exists-p user-settings-dir)
;;   (mapc 'load (directory-files user-settings-dir nil "^[^#].*el$")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/.bookmarks"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
