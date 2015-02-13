;;; .emacs --- My custom .emacs file

;;; Commentary:
;; here's some commentary

(require 'package)
;;; Code:

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(setq inhibit-startup-message t)

(defalias 'yes-or-no-p 'y-or-n-p)

(defconst my-packages
  '(company
    company-c-headers
    ggtags
    helm
    helm-gtags
    function-args
    ;;    clean-aindent-mode
    ;;    dtrt-indent
    ;;    ws-butler
    yasnippet
    helm-c-yasnippet
    smartparens
    projectile
    helm-projectile
    helm-proc
    window-number
    web-mode
    magit
    undo-tree
    php-mode
    clojure-cheatsheet
    clojure-mode
    clojure-snippets
    color-moccur
    diff-hl
    rainbow-delimiters
    ;; pcomplete-extension
    expand-region
    idle-highlight-mode
    js2-mode
    js2-refactor
    geben
    flycheck
    skewer-mode
    helm-themes
    lush-theme
    php-refactor-mode
    php-auto-yasnippets
    ace-jump-mode
;;    ace-isearch
;;    kibit-mode
    ess
    cider
    flycheck-rust
    helm-swoop
    json-mode
    lua-mode
    multiple-cursors
    nim-mode
    rust-mode
    scala-mode2
    simple-httpd))

(defun install-packages ()
  "Install all required packages."
  (interactive)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package my-packages)
    (unless (package-installed-p package)
      (package-install package))))

(install-packages)

;; setup ggtags
(require 'ggtags)
(add-hook 'prog-mode-hook 'ggtags-mode)
;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
;;               (ggtags-mode 1))))

(define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
(define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
(define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

;;(define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)

;; this variables must be set before load helm-gtags
;; you can change to any prefix key of your choice
(setq helm-gtags-prefix-key "\C-cg")

;;;;;;;;;;;;;;;;;;;;;; setup helm
(require 'helm)

;; must set before helm-config,  otherwise helm use default
;; prefix "C-x c", which is inconvenient because you can
;; accidentially pressed "C-x C-c"
(setq helm-command-prefix-key "C-c h")

(require 'helm-config)
(require 'helm-eshell)
(require 'helm-files)
(require 'helm-grep)

;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
;; (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
;; (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
(define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
(define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)

(setq
 helm-google-suggest-use-curl-p t
 helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
 helm-quick-update t ; do not display invisible candidates
 helm-idle-delay 0.01 ; be idle for this many seconds, before updating in delayed sources.
 helm-input-idle-delay 0.01 ; be idle for this many seconds, before updating candidate buffer
 helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.

 ;; you can customize helm-do-grep to execute ack-grep
 ;; helm-grep-default-command "ack-grep -Hn --smart-case --no-group --no-color %e %p %f"
 ;; helm-grep-default-recurse-command "ack-grep -H --smart-case --no-group --no-color %e %p %f"
 helm-split-window-default-side 'other ;; open helm buffer in another window
 helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window
 helm-buffers-favorite-modes (append helm-buffers-favorite-modes
				     '(picture-mode artist-mode))
 helm-candidate-number-limit 500 ; limit the number of displayed canidates
 helm-M-x-requires-pattern 0	 ; show all candidates when set to 0
 helm-boring-file-regexp-list
 '("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "\\.i$") ; do not show these files in helm buffer
 helm-ff-file-name-history-use-recentf t
					;helm-move-to-line-cycle-in-source t ; move to end or beginning of source
                                        ; when reaching top or bottom of source.
 ido-use-virtual-buffers t		; Needed in helm-buffers-list
 helm-buffers-fuzzy-matching t          ; fuzzy matching buffer names when non-nil
                                        ; useful in helm-mini that lists buffers
 )

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
;;(global-set-key (kbd "C-c h") 'helm-mini)
;;(global-set-key (kbd "C-x b") 'helm-for-files)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
;;(global-set-key (kbd "C-c <SPC>") 'helm-all-mark-rings)
(global-set-key (kbd "C-c h o") 'helm-occur)
;; (global-set-key (kbd "M-s o") 'helm-occur)
(global-set-key (kbd "C-c h g") 'helm-do-grep)
;;(global-set-key (kbd "M-g s") 'helm-do-grep)
(global-set-key (kbd "C-c h C-c w") 'helm-wikipedia-suggest)
(global-set-key (kbd "C-c h x") 'helm-register)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
;;(global-set-key (kbd "C-c i") 'helm-imenu)
;;(global-set-key (kbd "C-c C-p") 'helm-projectile)
;;(global-set-key (kbd "C-M-z") 'helm-resume)
;;(global-set-key (kbd "M-%") 'helm-regexp)
;; ;; (global-set-key (kbd "M-/") 'company-complete)
;; ;; (global-set-key (kbd "M-?") 'hippie-expand)
;; (global-set-key (kbd "M-?") 'hippie-expand)
(global-set-key (kbd "C-=") 'er/expand-region)


(define-key 'help-command (kbd "C-f") 'helm-apropos)
(define-key 'help-command (kbd "r") 'helm-info-emacs)
(define-key 'help-command (kbd "C-l") 'helm-locate-library)

;;; Save current position to mark ring
(add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

(helm-mode)




;;;;;;;;;;;;;;;;;;;;;;;;; setup helm gtags
(require 'helm-gtags)

(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t

 helm-gtags-suggested-key-mapping t
 )

;; Enable helm-gtags-mode in Dired so you can jump to any tag
;; when navigate project tree with Dired
(add-hook 'dired-mode-hook 'helm-gtags-mode)

;; Enable helm-gtags-mode in Eshell for the same reason as above
(add-hook 'eshell-mode-hook 'helm-gtags-mode)

;; Enable helm-gtags-mode in languages that GNU Global supports
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'java-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

;; key bindings
(define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)



;; setup cedet
(require 'cc-mode)
(require 'semantic)

(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(global-semantic-stickyfunc-mode 1)
(global-semantic-idle-summary-mode 1)

(semantic-mode 1)

;; Enable EDE only in C/C++
(require 'ede)
(global-ede-mode)


;; function-args
(require 'function-args)
(fa-config-default)
(define-key c-mode-map  [(tab)] 'moo-complete)
(define-key c++-mode-map  [(tab)] 'moo-complete)

;; company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
;;(delete 'company-semantic company-backends)
(define-key c-mode-map  [(control tab)] 'company-complete)
(define-key c++-mode-map  [(control tab)] 'company-complete)

;; company-c-headers
(add-to-list 'company-backends 'company-c-headers)

;; hs-minor-mode for folding source code
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;; Available C style:
;; “gnu”: The default style for GNU projects
;; “k&r”: What Kernighan and Ritchie, the authors of C used in their book
;; “bsd”: What BSD developers use, aka “Allman style” after Eric Allman.
;; “whitesmith”: Popularized by the examples that came with Whitesmiths C, an early commercial C compiler.
;; “stroustrup”: What Stroustrup, the author of C++ used in his book
;; “ellemtel”: Popular C++ coding standards as defined by “Programming in C++, Rules and Recommendations,” Erik Nyquist and Mats Henricson, Ellemtel
;; “linux”: What the Linux developers use for kernel development
;; “python”: What Python developers use for extension modules
;; “java”: The default style for java-mode (see below)
;; “user”: When you want to define your own style
;; (setq c-default-style "linux") ;; set style to "linux"

;;(global-set-key (kbd "RET") 'newline-and-indent)  ; automatically indent when press RET

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; use space to indent by default
;;(setq-default indent-tabs-mode nil)

;; set appearance of a tab that is represented by 4 spaces
;;(setq-default tab-width 4)

;; Compilation
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))

;; setup GDB
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

;; ;; Package: clean-aindent-mode
;; (require 'clean-aindent-mode)
;; (add-hook 'prog-mode-hook 'clean-aindent-mode)

;; ;; Package: dtrt-indent
;; (require 'dtrt-indent)
;; (dtrt-indent-mode 1)

;; ;; Package: ws-butler
;;(require 'ws-butler)
;;(add-hook 'prog-mode-hook 'ws-butler-mode)

;; Package: yasnippet
(require 'yasnippet)
(yas-global-mode -1)
(yas-reload-all)
(add-hook 'prog-mode-hook 'yas-minor-mode)

;; Package: smartparens
(require 'smartparens-config)
(show-smartparens-global-mode +1)
(smartparens-global-strict-mode 1)
(sp-use-smartparens-bindings)

;; Package: projejctile
(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)
;;;; don't auto-refresh remote caches
;; (setq projectile-file-exists-remote-cache-expire nil)

;; Package: window-number
(require 'window-number)
(window-number-mode 1)

;; Package: undo-tree
(global-undo-tree-mode)

;; Package: cider
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
					;(setq nrepl-hide-special-buffers t)

;; Package: rainbow-delimiters
;;(global-rainbow-delimiters-mode) ; disabled due to a bad interaction with helm
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Package: diff-hl
(global-diff-hl-mode)

;; Package: pcomplete-extension
;;(require 'pcomplete-extension)

;; (add-hook 'eshell-mode-hook
;;           #'(lambda ()
;;               (define-key eshell-mode-map
;;                 [remap eshell-pcomplete]
;;                 'helm-esh-pcomplete)))
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map
                (kbd "M-p")
                'helm-eshell-history)))

;; Package: idle-highlight-mode
(add-hook 'prog-mode-hook 'idle-highlight-mode)

;; Package: js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Package: js2-refactor
(require 'js2-refactor)

;; Package: web-mode
(add-to-list 'auto-mode-alist '("\\.ctp\\'" . web-mode))

;; Package: flycheck
(add-hook 'prog-mode-hook 'flycheck-mode)

;; Package: php-refactor-mode
(require 'php-refactor-mode)
(add-hook 'php-mode-hook 'php-refactor-mode)
(add-hook 'php-mode-hook
	  #'(lambda ()
	      (php-enable-wordpress-coding-style)))

;; Package: php-auto-yasnippets
(require 'php-auto-yasnippets)

;; Package: ace-jump-mode
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; Package: ace-isearch
;; (require 'ace-isearch)
;; (global-ace-isearch-mode +1)

;; Package: ess
(require 'ess-site)

;; Package: multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; misc
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;;;; startup message using fortune if possible
;; (if (executable-find "fortune")
;;     (setq initial-scratch-message
;; 	  (concat
;; 	   initial-scratch-message
;; 	   "\t;; "
;; 	   (replace-regexp-in-string "\n" "\n\t;; " (shell-command-to-string "fortune"))
;; 	   "\n")))

;; Package: kibit-mode
;; (require 'kibit-mode)
;; (add-hook 'clojure-mode-hook 'kibit-mode)

(defun jesse-scratchpad ()
  (require 'geben)
  (projectile-switch-project)
  (geben-set-breakpoint-call "UsersApiController->matchUsername")
  )




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(async-shell-command-buffer (quote rename-buffer))
 '(backup-by-copying t)
 '(backup-directory-alist (quote ((".*" . "~/.saves"))))
 '(column-number-mode t)
 '(comint-input-ignoredups t)
 '(company-idle-delay 2)
 '(compilation-message-face (quote default))
 '(custom-enabled-themes (quote (lush)))
 '(custom-safe-themes
   (quote
    ("3164a65923ef23e0f3dff9f9607b4da1e07ef1c3888d0f6878feef6c28357732" "c7cd81771525ff66c105413134cdf0330b0b5b88fd8096e5d56b0256872ba6c7" "1ba463f6ac329a56b38ae6ac8ca67c8684c060e9a6ba05584c90c4bffc8046c3" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "1e194b1010c026b1401146e24a85e4b7c545276845fc38b8c4b371c8338172ad" "d809ca3cef02087b48f3f94279b86feca896f544ae4a82b523fba823206b6040" default)))
 '(delete-old-versions t)
 '(dired-dwim-target t)
 '(dired-listing-switches "-alhv")
 '(ediff-diff-options "-w")
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(enable-recursive-minibuffers t)
 '(eshell-history-size 1000)
 '(explicit-shell-file-name "bash")
 '(flycheck-checker-error-threshold 2000)
 '(flycheck-phpcs-standard "CakePHP")
 '(fringe-mode 10 nil (fringe))
 '(geben-pause-at-entry-line nil)
 '(global-hl-line-mode t)
 '(global-prettify-symbols-mode t)
 '(helm-external-programs-associations (quote (("csv" . "oocalc"))))
 '(history-length 100)
 '(inhibit-startup-screen t)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(kept-new-versions 6)
 '(lazy-highlight-cleanup nil)
 '(linum-format " %6d ")
 '(magit-diff-use-overlays nil)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control)))))
 '(password-cache-expiry 3600)
 '(projectile-enable-caching t)
 '(projectile-mode-line "Proj")
 '(recentf-max-menu-items 25)
 '(recentf-max-saved-items 50)
 '(recentf-mode t)
 '(save-place t nil (saveplace))
 '(savehist-mode t)
 '(show-paren-mode t)
 '(sql-mysql-options (quote ("--prompt=mysql> ")))
 '(tool-bar-mode nil)
 '(tramp-backup-directory-alist (quote ((".*" . "~/.saves"))))
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(version-control t)
 '(window-number-meta-mode t)
 '(winner-mode t)
 '(ws-butler-global-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 98 :width normal)))))

(provide '.emacs)

;;; .emacs ends here
(put 'narrow-to-region 'disabled nil)
