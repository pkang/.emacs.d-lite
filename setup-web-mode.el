(require 'web-mode)
(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  )
(add-hook 'web-mode-hook  'web-mode-hook)


(defun skip-to-next-blank-line ()
  (interactive)
  (let ((inhibit-changing-match-data t))
    (skip-syntax-forward " >")
    (unless (search-forward-regexp "^\\s *$" nil t)
      (goto-char (point-max)))))

(defun skip-to-previous-blank-line ()
  (interactive)
  (let ((inhibit-changing-match-data t))
    (skip-syntax-backward " >")
    (unless (search-backward-regexp "^\\s *$" nil t)
      (goto-char (point-min)))))

(defun html-wrap-in-tag (beg end)
  (interactive "r")
  (let ((oneline? (= (line-number-at-pos beg) (line-number-at-pos end))))
    (deactivate-mark)
    (goto-char end)
    (unless oneline? (newline-and-indent))
    (insert "</div>")
    (goto-char beg)
    (insert "<div>")
    (unless oneline? (newline-and-indent))
    (indent-region beg (+ end 11))
    (goto-char (+ beg 4))))

(defun --setup-simplezen ()
  (require 'simplezen)
  (set (make-local-variable 'yas-fallback-behavior)
       '(apply simplezen-expand-or-indent-for-tab)))

(add-hook 'web-mode-hook '--setup-simplezen)

(eval-after-load "web-mode"
  '(progn
     (define-key web-mode-map [remap forward-paragraph] 'skip-to-next-blank-line)
     (define-key web-mode-map [remap backward-paragraph] 'skip-to-previous-blank-line)
     (define-key web-mode-map (kbd "C-c C-w") 'html-wrap-in-tag)
     (define-key web-mode-map (kbd "/") nil) ;; no buggy matching of slashes

     (define-key web-mode-map (kbd "C-c C-d") 'ng-snip-show-docs-at-point)

     (require 'tagedit)

     ;; paredit lookalikes
     (define-key web-mode-map (kbd "s-<right>") 'tagedit-forward-slurp-tag)
     (define-key web-mode-map (kbd "C-)") 'tagedit-forward-slurp-tag)
     (define-key web-mode-map (kbd "s-<left>") 'tagedit-forward-barf-tag)
     (define-key web-mode-map (kbd "C-}") 'tagedit-forward-barf-tag)
     (define-key web-mode-map (kbd "M-r") 'tagedit-raise-tag)
     (define-key web-mode-map (kbd "s-s") 'tagedit-splice-tag)
     (define-key web-mode-map (kbd "M-S") 'tagedit-split-tag)
     (define-key web-mode-map (kbd "M-J") 'tagedit-join-tags)
     (define-key web-mode-map (kbd "M-?") 'tagedit-convolute-tags)

     (tagedit-add-experimental-features)
     (add-hook 'web-mode-hook (lambda () (tagedit-mode 1)))

     ;; no paredit equivalents
     (define-key web-mode-map (kbd "s-k") 'tagedit-kill-attribute)
     (define-key web-mode-map (kbd "s-<return>") 'tagedit-toggle-multiline-tag)))

;; after deleting a tag, indent properly
(defadvice sgml-delete-tag (after reindent activate)
  (indent-region (point-min) (point-max)))

(provide 'setup-web-mode)
