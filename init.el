;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;;Setup load path
(load (expand-file-name "load-paths.el" user-emacs-directory))

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))


;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

;; Setup packages
(require 'setup-package)

;; Install extensions if they're missing
(defun init--install-packages ()
  (packages-install
   '(
     ac-inf-ruby
     ac-js2
     ac-slime
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
     elisp-slime-nav
     enh-ruby-mode
     epl
     eproject
     evil
     exec-path-from-shell
     expand-region
     fill-column-indicator
     find-file-in-project
     flx
     flx-ido
     flycheck
     fold-this
     gist
     git-commit-mode
     git-commit-mode
     git-rebase-mode
     gitconfig-mode
     gitconfig-mode
     gitignore-mode
     gitignore-mode
     god-mode
     guide-key
     haml-mode
     helm
     highlight-escape-sequences
     highlight-indentation
     htmlize
     ido-at-point
     ido-ubiquitous
     ido-vertical-mode
     idomenu
     inf-ruby
     jabber
     js2-mode
     js2-refactor
     jump-char
     magit
     magit
     move-text
     multifiles
     multiple-cursors
     nodejs-repl
     org
     org-plus-contrib
     paredit
     pkg-info
     popwin
     pretty-symbols
     projectile
     project-explorer
     rainbow-mode
     robe
     restclient
     rinari
     rsense
     rvm 
     s
     simple-httpd
     simplezen
     skewer-less
     skewer-mode
     slim-mode
     smart-forward
     smartparens
     smex
     smooth-scrolling
     tagedit
     undo-tree
     visual-regexp
     visual-regexp
     visual-regexp-steroids
     visual-regexp-steroids
     w3m
     web-mode
     wgrep
     whitespace-cleanup-mode
     window-numbering
     yasnippet
     zenburn-theme
     )))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))


;; Setup el-get
(setq el-get-user-package-directory (expand-file-name "el-get-init-files" user-emacs-directory))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(el-get 'sync)

;; installed packages not in melpa
(require 'el-get)

(setq my-packages
      (append
       '(
         ri-emacs
         rcodetools
         ruby-electric

         ;;auto-complete
                                        ;deferred 
                                        ;epc
                                        ;ctable
                                        ;anything
                                        ;anything-show-completion
                                        ;anything-rcodetools
         )
       (mapcar 'el-get-source-name el-get-sources)))

(el-get-cleanup my-packages)
(el-get 'sync my-packages)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Setup appearance
(load "appearance.el")

;; Start autocomplete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(add-to-list 'ac-modes 'enh-ruby-mode)
(add-to-list 'ac-modes 'web-mode)

;; Setup inf-ruby
(autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
(add-hook 'after-init-hook 'inf-ruby-switch-setup)

;; Setup ac-inf-ruby
(eval-after-load 'auto-complete
  '(add-to-list 'ac-modes 'inf-ruby-mode))
(add-hook 'inf-ruby-mode-hook 'ac-inf-ruby-enable)

;; Setup coffee-mode
(load "setup-coffee-mode.el")

;; Setup display time
(setq display-time-24hr-format t)
(display-time)

;; Setup edbi
(setenv "PERL5LIB" (concat "/Users/" user-login-name "/perl5/lib/perl5"))

;; Setup haml-mode
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))

;; Setup php-mode
(add-hook 'php-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil
                  tab-width 2)))

;; Setup popwin
(require 'popwin)
(custom-set-variables
 '(display-buffer-function 'popwin:display-buffer))

;; Setup rcodetools
(require 'rcodetools)

;; Setup rinari
(require 'rinari)

(add-hook 'ruby-mode-hook
          (lambda ()
            (rvm-activate-corresponding-ruby)
            (ruby-electric-mode)
            (setq rinari-tags-file-name "TAGS")))

(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'robe-mode-hook 'robe-ac-setup)

(add-hook 'robe-mode-hook 'robe-start)

;; Setup skewer mode
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

;; Setup slime
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

;; Lets start with a smattering of sanity
(require 'sane-defaults)

;; Setup environment variables from the user's shell.
(when is-mac
  (require-package 'exec-path-from-shell)
  (exec-path-from-shell-initialize))
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; guide-key
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-x v" "C-x 8" "C-x +"))
(guide-key-mode 1)
(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/popup-window-position 'bottom)

;; Setup extensions
(eval-after-load 'ido '(require 'setup-ido))
(require 'setup-org)
;;(eval-after-load 'dired '(require 'setup-dired))
(eval-after-load 'magit '(require 'setup-magit))
(eval-after-load 'grep '(require 'setup-rgrep))
(eval-after-load 'shell '(require 'setup-shell))
;(require 'setup-hippie)
;(require 'setup-yasnippet)
;(require 'setup-perspective)
(require 'setup-ffip)
(require 'setup-html-mode)
;(require 'setup-paredit)

;; Font lock dash.el
(eval-after-load "dash" '(dash-enable-font-lock))

;; Default setup of smartparens
(require 'smartparens-config)
(setq sp-autoescape-string-quote nil)
(--each '(css-mode-hook
          restclient-mode-hook
          js-mode-hook
          java-mode
          ruby-mode
          markdown-mode
          groovy-mode)
  (add-hook it 'turn-on-smartparens-mode))

;; Language specific setup files
(eval-after-load 'js2-mode '(require 'setup-js2-mode))
(eval-after-load 'ruby-mode '(require 'setup-ruby-mode))
(eval-after-load 'clojure-mode '(require 'setup-clojure-mode))
(eval-after-load 'markdown-mode '(require 'setup-markdown-mode))

;; Load stuff on demand
(autoload 'skewer-start "setup-skewer" nil t)
(autoload 'skewer-demo "setup-skewer" nil t)
(autoload 'flycheck-mode "setup-flycheck" nil t)
(autoload 'auto-complete-mode "auto-complete" nil t)

;; Map files to modes
(require 'mode-mappings)

;; Highlight escape sequences
(require 'highlight-escape-sequences)
(hes-mode)
(put 'font-lock-regexp-grouping-backslash 'face-alias 'font-lock-builtin-face)

;; Visual regexp
(require 'visual-regexp)
(define-key global-map (kbd "M-&") 'vr/query-replace)
(define-key global-map (kbd "M-/") 'vr/replace)

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;; Load reasonably default keybindings
(load "key-bindings.el")

(require 'expand-region)
(require 'multiple-cursors)
(require 'delsel)
(require 'jump-char)
(require 'eproject)
(require 'wgrep)
(require 'smart-forward)
(require 'change-inner)
(require 'multifiles)

;; Fill column indicator
(require 'fill-column-indicator)
(setq fci-rule-color "#111122")

;; Browse kill ring
(require 'browse-kill-ring)
(setq browse-kill-ring-quit-action 'save-and-restore)

;; Smart M-x is smart
(require 'smex)
(smex-initialize)

;; Projectile everywhere
(projectile-global-mode)

;; Elisp go-to-definition with M-. and back again with M-,
(autoload 'elisp-slime-nav-mode "elisp-slime-nav")
(add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t) (eldoc-mode 1)))

(when is-mac (require 'mac))

;; Fix whitespace on save, but only if the file was clean
(global-whitespace-cleanup-mode)

(require 'multiple-cursors)

;; Show line numbers for all files
;(add-hook 'find-file-hook (lambda () (linum-mode 1)))

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;; Conclude init by setting up specifics for the current user
(when (file-exists-p user-settings-dir)
  (mapc 'load (directory-files user-settings-dir nil "^[^#].*el$")))

;Increase GC threshold
;(setq gc-cons-threshold 20000000)
(put 'erase-buffer 'disabled nil)
