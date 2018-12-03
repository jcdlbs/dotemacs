;; .emacs --- My custom .emacs file

;;; Commentary:
;;;;;; here's some commentary

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(async-shell-command-buffer (quote rename-buffer))
 '(backup-by-copying t)
 '(backup-directory-alist (quote ((".*" . "~/.saves"))))
 '(column-number-mode t)
 '(comint-input-ignoredups t)
 '(company-dabbrev-downcase nil)
 '(company-idle-delay 2)
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("0820d191ae80dcadc1802b3499f84c07a09803f2cb90b343678bdb03d225b26b" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "0e219d63550634bc5b0c214aced55eb9528640377daf486e13fb18a32bf39856" "cdbd0a803de328a4986659d799659939d13ec01da1f482d838b68038c1bb35e8" "40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" "f3d6a49e3f4491373028eda655231ec371d79d6d2a628f08d5aa38739340540b" default)))
 '(delete-old-versions t)
 '(dired-dwim-target t)
 '(dired-listing-switches "-alhv")
 '(eclim-eclipse-dirs
   (quote
    ("~/opt/eclipse" "/Applications/eclipse" "/usr/lib/eclipse" "/usr/local/lib/eclipse" "/usr/share/eclipse")))
 '(eclim-executable "~/opt/eclipse/eclim")
 '(eclim-java-documentation-root "/usr/share/javadoc/java")
 '(ediff-diff-options "-w")
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(enable-recursive-minibuffers t)
 '(eshell-history-size 5000)
 '(exec-path
   (quote
    ("~/bin" "/usr/local/sbin" "/usr/local/bin" "/usr/sbin" "/usr/bin" "/usr/libexec/emacs/25.3/x86_64-redhat-linux-gnu")))
 '(explicit-shell-file-name "bash")
 '(fci-rule-color "#383838")
 '(flycheck-checker-error-threshold 2000)
 '(flycheck-phpcs-standard "PSR1")
 '(fringe-mode 10 nil (fringe))
 '(geben-pause-at-entry-line nil)
 '(global-hl-line-mode nil)
 '(global-prettify-symbols-mode t)
 '(helm-external-programs-associations (quote (("csv" . "oocalc"))))
 '(helm-swoop-pre-input-function (lambda nil ""))
 '(help-at-pt-display-when-idle t nil (help-at-pt))
 '(history-length 100)
 '(httpd-port 7080)
 '(inhibit-startup-screen t)
 '(kept-new-versions 6)
 '(lazy-highlight-cleanup nil)
 '(linum-format " %6d ")
 '(magit-diff-use-overlays nil)
 '(magit-use-overlays nil)
 '(menu-bar-mode nil)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control)))))
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(nxml-slash-auto-complete-flag t)
 '(package-archives
   (quote
    (("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/"))))
 '(package-selected-packages
   (quote
    (markdown-mode yasnippet multiple-cursors company cider clojure-mode flycheck projectile helm helm-core helm-swoop lush-theme avy smooth-scrolling nginx-mode neotree spacemacs-theme smart-mode-line company-emacs-eclim eclim diminish helm-descbinds use-package helm-ag rjsx-mode restclient restclient-helm php-mode window-number web-mode undo-tree smartparens skewer-mode rust-mode rainbow-delimiters magit lua-mode json-mode js2-refactor helm-themes helm-projectile helm-proc helm-gtags helm-c-yasnippet ggtags geben function-args flycheck-rust flycheck-irony f expand-region elisp-slime-nav diff-hl company-quickhelp company-irony cmake-mode clojure-snippets clj-refactor)))
 '(password-cache-expiry 3600)
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(projectile-enable-caching t)
 '(projectile-globally-ignored-files (quote ("TAGS" "GPATH" "GRTAGS" "GTAGS")))
 '(projectile-mode-line "Proj")
 '(purpose-mode nil)
 '(purpose-preferred-prompt (quote auto))
 '(purpose-user-mode-purposes
   (quote
    ((java-mode . edit)
     (prog-mode . edit)
     (org-mode . edit)
     (fundamental-mode . edit)
     (web-mode . edit)
     (js-mode . edit)
     (js2-mode . edit)
     (lisp-mode . edit)
     (php-mode . edit)
     (dired . nav)
     (term-mode . terminal)
     (ansi-term-mode . terminal)
     (eshell-mode . terminal)
     (shell-mode . terminal)
     (compilation-mode . messages))))
 '(purpose-user-name-purposes (quote (("*shell*" . terminal) ("*eshell*" . terminal))))
 '(recentf-max-menu-items 25)
 '(recentf-max-saved-items 50)
 '(recentf-mode t)
 '(ring-bell-function (quote ignore))
 '(rm-text-properties
   (quote
    (("\\` Ovwrt\\'"
      (quote face)
      (quote font-lock-warning-face))
     ("\\[[0-9]\\]"
      (quote face)
      (quote success)))))
 '(save-place-mode t nil (saveplace))
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(show-smartparens-global-mode t)
 '(smartparens-global-mode nil)
 '(smartparens-global-strict-mode nil)
 '(sml/name-width 0)
 '(sml/shorten-directory t)
 '(sml/shorten-modes t)
 '(sml/theme (quote respectful))
 '(smooth-scrolling-mode t)
 '(sp-base-key-bindings (quote sp))
 '(spaceline-helm-mode t)
 '(sql-mysql-options (quote ("--prompt=mysql> ")))
 '(tool-bar-mode nil)
 '(tramp-backup-directory-alist (quote ((".*" . "~/.saves"))))
 '(tramp-default-method "ssh")
 '(tramp-histfile-override "~/.tramp_history")
 '(undo-limit 800000)
 '(undo-strong-limit 1200000)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(version-control t)
 '(window-number-meta-mode t)
 '(winner-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; experiencing crashes with the default gc-cons-threshold so I'm
;;bumping it up here. (garbage-collect)
(setq gc-cons-threshold (* 1024 1024 100)) ; 100 megs

(setq inhibit-startup-message t)

(defalias 'yes-or-no-p 'y-or-n-p)

(require 'package)
(package-initialize)

(use-package spacemacs-theme
  :defer t
  :init
  (load-theme 'spacemacs-dark t))

(use-package ggtags
  :config
  ;;(add-hook 'prog-mode-hook 'ggtags-mode)
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
                (ggtags-mode 1))))

  (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
  (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
  (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
  (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
  (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
  (define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)
  ;;(define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)
  )

;;;;;;;;;;;;;;;;;;;;;; setup helm
(use-package helm
  :diminish helm-mode
  :init
  (progn
    (setq
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
     helm-candidate-number-limit 500 ; limit the number of displayed canidates
     helm-M-x-requires-pattern 0   ; show all candidates when set to 0
     helm-boring-file-regexp-list
     '("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "\\.i$") ; do not show these files in helm buffer
     helm-ff-file-name-history-use-recentf t
                                        ;helm-move-to-line-cycle-in-source t ; move to end or beginning of source
                                        ; when reaching top or bottom of source.
     ido-use-virtual-buffers t    ; Needed in helm-buffers-list
     helm-buffers-fuzzy-matching t          ; fuzzy matching buffer names when non-nil
                                        ; useful in helm-mini that lists buffers

     ))

  :config
  (progn
    ;; must set before helm-config,    otherwise helm use default
    ;; prefix "C-x c", which is inconvenient because you can
    ;; accidentially pressed "C-x C-c"
    (setq
     helm-command-prefix-key "C-c h")

    (require 'helm-config)

    ;; not sure if the following are required - maybe remove?
    (require 'helm-eshell)
    (require 'helm-files)
    (require 'helm-grep)


    (setq helm-buffers-favorite-modes (append helm-buffers-favorite-modes
                                              '(picture-mode artist-mode)))
    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t))

    ;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
    ;; (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
    ;; (define-key helm-map (kbd "C-z")    'helm-select-action) ; list actions using C-z

    (define-key helm-grep-mode-map (kbd "<return>")    'helm-grep-mode-jump-other-window)
    (define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
    (define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)

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

    (define-key 'help-command (kbd "C-f") 'helm-apropos)
    (define-key 'help-command (kbd "r") 'helm-info-emacs)
    (define-key 'help-command (kbd "C-l") 'helm-locate-library)

;;; Save current position to mark ring
    (add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

    (helm-mode 1)))

(use-package helm-descbinds
  :defer t
  :bind (("C-h b" . helm-descbinds)
         ("C-h w" . helm-descbinds)))

;;;;;;;;;;;;;;;;;;;;;;;;; setup helm gtags
(use-package helm-gtags
  :init
  ;; this variables must be set before load helm-gtags
  ;; you can change to any prefix key of your choice
  (setq helm-gtags-prefix-key "\C-cg")
  :config
  (progn
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
    (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)))

(use-package helm-projectile
  :config
  (helm-projectile-on))

;; setup cedet
(require 'cc-mode)
;; (require 'semantic)
;; (global-semanticdb-minor-mode 1)
;; (global-semantic-idle-scheduler-mode 1)
;; (global-semantic-stickyfunc-mode 1)
;; (global-semantic-idle-summary-mode 1)

;; (semantic-mode 1)

;; ;; Enable EDE only in C/C++
;; (require 'ede)
;; (global-ede-mode)

;; function-args
(use-package function-args
  :config
  (fa-config-default)
  (define-key c-mode-map  [(tab)] 'moo-complete)
  (define-key c++-mode-map  [(tab)] 'moo-complete))

;; company
(use-package company
  :diminish company-mode " C"
  :config
  (progn
    (add-hook 'after-init-hook 'global-company-mode)
    (delete 'company-semantic company-backends)
    (define-key c-mode-map  [(control tab)] 'company-complete)
    (define-key c++-mode-map  [(control tab)] 'company-complete)
    (global-set-key (kbd "C-<tab>") 'company-manual-begin)
    (global-set-key (kbd "M-/") 'company-complete)))

(use-package company-quickhelp
  :config
  (company-quickhelp-mode 1))

;; company-c-headers
;; (add-to-list 'company-backends 'company-c-headers)

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

;; ;; show unncessary whitespace that can mess up your diff
;; (add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; use space to indent by default
;;(setq-default indent-tabs-mode nil)

;; set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 2)

;; ;; Compilation
;; (global-set-key (kbd "<f5>") (lambda ()
;;                                (interactive)
;;                                (setq-local compilation-read-command nil)
;;                                (call-interactively 'compile)))

;; setup GDB
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

;; (use-package clean-aindent-mode
;;   :config
;;   (add-hook 'prog-mode-hook 'clean-aindent-mode))

;; (use-package dtrt-indent
;;   :config
;;   (dtrt-indent-mode 1))

;;(use-package ws-butler)
;;(add-hook 'prog-mode-hook 'ws-butler-mode)

(use-package yasnippet
  :diminish yas-minor-mode " Y"
  :config
  (progn
    (yas-global-mode -1)
    (yas-reload-all)
    (add-hook 'prog-mode-hook 'yas-minor-mode)))

(use-package smartparens
  :config
  (progn
    (require 'smartparens-config)
    (add-hook 'prog-mode-hook #'smartparens-mode)
    ;; (show-smartparens-global-mode +1)
    ;; (smartparens-global-strict-mode 1)
    ;; (sp-use-smartparens-bindings)
    ))

(use-package projectile
  :diminish projectile-mode
  :config
  (progn
    (projectile-mode)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

    ;; attempts to deal with slow tramp issues
    (setq projectile-mode-line "Proj")
    (setq projectile-file-exists-remote-cache-expire (* 10 60))))

(use-package window-number
  :config
  (window-number-mode 1))

;; Package: undo-tree
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

(use-package cider
  :config
  (progn
;;;;; following might be obsolete
;;;;;(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
    ;;(setq nrepl-hide-special-buffers t)
    ))

(use-package rainbow-delimiters
  :config
  (progn
    ;;(global-rainbow-delimiters-mode) ; disabled due to a bad interaction with helm
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

(use-package diff-hl
  :config
  (global-diff-hl-mode))

;; (use-package pcomplete-extension
;;   :config
;;   (progn
;;    ;; (add-hook 'eshell-mode-hook
;;    ;;        #'(lambda ()
;;    ;;          (define-key eshell-mode-map
;;    ;;          [remap eshell-pcomplete]
;;    ;;          'helm-esh-pcomplete)))
;;    (add-hook 'eshell-mode-hook
;;          #'(lambda ()
;;            (define-key eshell-mode-map
;;            (kbd "M-p")
;;            'helm-eshell-history)))))

;; (use-package idle-highlight-mode
;;   :config
;;   (add-hook 'prog-mode-hook 'idle-highlight-mode))

(use-package js2-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(use-package js2-refactor)

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.ctp\\'" . web-mode)))

(use-package flycheck
  :config
  (progn
    (setq-default flycheck-emacs-lisp-load-path 'inherit)
    (add-hook 'prog-mode-hook 'flycheck-mode)))

;; (use-package php-refactor-mode
;;   :config
;;   (progn
;;    (add-hook 'php-mode-hook 'php-refactor-mode)
;;    (add-hook 'php-mode-hook
;;          #'(lambda ()
;;            (php-enable-wordpress-coding-style)))))

;; following php-auto-yasnippets seems to have an issue with java mode
;; (use-package php-auto-yasnippets
;;   :config
;;   (setq php-auto-yasnippet-php-program "~/.emacs.d/elpa/php-auto-yasnippets-20141128.1411/Create-PHP-YASnippet.php"))

;; (use-package ace-jump-mode
;;   :config
;;   (define-key global-map (kbd "C-c SPC") 'ace-jump-mode))

;; (use-package ace-isearch
;;   :config
;;   (global-ace-isearch-mode +1))

;; ;; Package: ess
;; (use-package ess-site)

(use-package multiple-cursors
  :config
  (progn
    (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
    (global-set-key (kbd "C->") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
    (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)))

;; misc
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;;;; startup message using fortune if possible
;; (if (executable-find "fortune")
;;     (setq initial-scratch-message
;;      (concat
;;       initial-scratch-message
;;       "\t;; "
;;       (replace-regexp-in-string "\n" "\n\t;; " (shell-command-to-string "fortune"))
;;       "\n")))

;; (use-package kibit-mode
;;   :config
;;   (add-hook 'clojure-mode-hook 'kibit-mode))

;; Package: clj-refactor
(use-package clj-refactor
  :config
  (add-hook 'clojure-mode-hook (lambda ()
                                 (clj-refactor-mode 1))))

;; EDE stuff
;; (ede-cpp-root-project "UniBrawl" :file "~/src/UniBrawl/CMakeLists.txt")

(use-package irony
  :config
  (progn
    (add-hook 'c++-mode-hook 'irony-mode)
    ;; (add-hook 'c-mode-hook 'irony-mode) ; not compatible with derived php-mode
    (add-hook 'c-mode-hook
              (lambda ()
                (unless (derived-mode-p 'php-mode)
                  (irony-mode))))
    (add-hook 'objc-mode-hook 'irony-mode)

    ;; replace the `completion-at-point' and `complete-symbol' bindings in
    ;; irony-mode's buffers by irony-mode's function
    (defun my-irony-mode-hook ()
      (define-key irony-mode-map [remap completion-at-point]
        'irony-completion-at-point-async)
      (define-key irony-mode-map [remap complete-symbol]
        'irony-completion-at-point-async))
    (add-hook 'irony-mode-hook 'my-irony-mode-hook)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)))

(use-package flycheck-irony
  :config
  (eval-after-load 'flycheck
    '(add-to-list 'flycheck-checkers 'irony)))

(use-package elisp-slime-nav
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'turn-on-elisp-slime-nav-mode)))

;; sudo for eshell
(use-package em-tramp)

(use-package eclim
  :config
  (global-eclim-mode))

(use-package eclimd
  :config
  (company-emacs-eclim-setup))

(use-package helm-swoop
  :config
  (progn
    (global-set-key (kbd "M-i") 'helm-swoop)
    (global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
    (global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
    (global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

    ;; When doing isearch, hand the word over to helm-swoop
    (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
    (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)

    ;; Save buffer when helm-multi-swoop-edit complete
    (setq helm-multi-swoop-edit-save t)))

;; ;; Packages: helm-company
;; (eval-after-load 'company
;;   '(progn
;;      (define-key company-mode-map (kbd "C-:") 'helm-company)
;;      (define-key company-active-map (kbd "C-:") 'helm-company)))

(use-package avy
  :bind ("C-c j" . avy-goto-char))

(use-package smart-mode-line
  :config
  (sml/setup))


;; (use-package spaceline
;;    :config
;;    (progn
;;      (require 'spaceline-config)
;;      (spaceline-emacs-theme)))

;;;;; this was breaking cider macroexpand
;; (use-package window-purpose
;;   :config
;;    (progn
;;      ;; don't hijack helms bindings
;;      (define-key purpose-mode-map (kbd "C-x b") nil)
;;      (define-key purpose-mode-map (kbd "C-x C-f") nil)
;;      (purpose-mode)))

;; (use-package multi-term
;;   :init
;;   (setq multi-term-program "/bin/bash"))

(use-package neotree
  :bind ("<f8>" . neotree-toggle))

;; need to validate that this change works
(use-package magit
  :config
  (add-hook 'git-commit-mode-hook
            '(lambda () (auto-fill-mode 0))))

(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1))

(use-package expand-region
  :bind (("M-SPC" . hippie-expand)
         ("C-=" . er/expand-region)
         ("M-=" . er/expand-region)))

;; (defun jesse-scratchpad ()
;;   (require 'geben)
;;   (projectile-switch-project)
;;   (geben-set-breakpoint-call "UsersApiController->matchUsername"))

;; ;;;;;;;;;;
;; ;;; save & shutdown when we get an "end of session" signal on dbus 
;; (require 'dbus)

;; (defun my-register-signals (client-path)
;;   "Register for the 'QueryEndSession' and 'EndSession' signals from
;; Gnome SessionManager.

;; When we receive 'QueryEndSession', we just respond with
;; 'EndSessionResponse(true, \"\")'.  When we receive 'EndSession', we
;; append this EndSessionResponse to kill-emacs-hook, and then call
;; kill-emacs.  This way, we can shut down the Emacs daemon cleanly
;; before we send our 'ok' to the SessionManager."
;;   (setq my-gnome-client-path client-path)
;;   (let ( (end-session-response (lambda (&optional arg)
;;                                  (dbus-call-method-asynchronously
;;                                   :session "org.gnome.SessionManager" my-gnome-client-path
;;                                   "org.gnome.SessionManager.ClientPrivate" "EndSessionResponse" nil
;;                                   t "") ) ) )
;;          (dbus-register-signal
;;           :session "org.gnome.SessionManager" my-gnome-client-path
;;           "org.gnome.SessionManager.ClientPrivate" "QueryEndSession"
;;           end-session-response )
;;          (dbus-register-signal
;;           :session "org.gnome.SessionManager" my-gnome-client-path
;;           "org.gnome.SessionManager.ClientPrivate" "EndSession"
;;           `(lambda (arg)
;;              (add-hook 'kill-emacs-hook ,end-session-response t)
;;              (kill-emacs) ) ) ) )

;; ;; DESKTOP_AUTOSTART_ID is set by the Gnome desktop manager when emacs
;; ;; is autostarted.    We can use it to register as a client with gnome
;; ;; SessionManager.
;; (dbus-call-method-asynchronously
;;  :session "org.gnome.SessionManager"
;;  "/org/gnome/SessionManager" 
;;  "org.gnome.SessionManager" "RegisterClient" 'my-register-signals
;;  "Emacs server" (getenv "DESKTOP_AUTOSTART_ID"))
;; ;;;;;;;;;;


(provide 'init)
;;; init ends here
