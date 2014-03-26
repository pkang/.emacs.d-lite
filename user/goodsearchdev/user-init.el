(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)

(global-unset-key (kbd "s-p"))

;; appearance
(load-theme 'monokai)

;; Coding font
(set-default-font "Anonymice Powerline-18")
(add-to-list 'default-frame-alist '(font . "Anonymice Powerline-18"))
