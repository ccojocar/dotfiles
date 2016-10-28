;; Package manager
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
			 '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize) ;; You might already have this line
(when (not package-archive-contents)
  (package-refresh-contents))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-show-menu 0.1)
 '(ac-auto-start 1)
 '(ac-candidate-limit 10)
 '(ac-delay 0.1)
 '(ac-trigger-key "TAB")
 '(company-backends
   (quote
	(company-bbdb company-nxml company-css company-eclim company-semantic company-clang company-xcode company-cmake company-capf
				  (company-dabbrev-code company-gtags company-etags company-keywords)
				  company-oddmuse company-files company-dabbrev)))
 '(company-selection-wrap-around t)
 '(company-show-numbers t)
 '(delete-selection-mode t)
 '(docker-global-mode nil)
 '(docker-images-run-arguments nil)
 '(docker-tramp-docker-executable "docker --tls")
 '(docker-tramp-docker-options (quote ("--tls")))
 '(exec-path (quote ("/home/cgc/go/bin" "/usr/lib/go/bin")))
 '(exec-path-from-shell-check-startup-files nil)
 '(exec-path-from-shell-variables
   (quote
	("PATH" "GOROOT" "GOPATH" "DOCKER_HOST" "DOCKER_SSL_VERIFY" "DOCKER_CERT_PATH")))
 '(jedi:use-shortcuts t)
 '(magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n256")))
 '(package-selected-packages
   (quote
	(py-autopep8 csharp-mode linum-relative tern js2-closure ac-js2 js2-refactor js2-mode evil-ediff evil-surround colemak-evil evil-smartparens evil-args evil-magit evil pyflakes smooth-scrolling multiple-cursors avy-zap avy cargo rust-mode function-args ggtags helm-gtags diffview gradle-mode maven-test-mode jenkins angular-mode js-doc serverspec fixmee fixme-mode neotree yaoddmuse yaml-mode wrap-region web-mode typescript-mode tss tidy textmate switch-window rw-language-and-country-codes rw-ispell rw-hunspell rvm ruby-tools ruby-refactor ruby-interpolation ruby-compilation ruby-block ruby-additional rspec-mode rsense robe real-auto-save rake puppetfile-mode puppet-mode popwin popup-kill-ring org-bullets org-autolist monokai-theme markup markdown-toc markdown-mode+ magit-gitflow magit-find-file log4j-mode lib-requires latex-preview-pane latex-extra jedi iedit http-post-simple helm-swoop helm-robe helm-projectile helm-ispell helm-go-package helm-git-grep helm-git helm-flyspell helm-ag header2 goto-last-change go-snippets go-projectile go-errcheck go-complete go-autocomplete git fuzzy-match flymake-yaml flymake-ruby flymake-puppet flymake-json flymake-go flycheck-d-unittest flex-autopair finder+ find-things-fast find-file-in-repository find-dired+ files+ expand-region exec-path-from-shell eruby-mode env-var-import enh-ruby-mode dockerfile-mode docker-tramp docker django-snippets direx d-mode company-restclient company-quickhelp company-jedi company-inf-ruby company-go bundler buffer-move autopair auto-yasnippet auto-complete-nxml aggressive-indent ace-jump-mode ac-inf-ruby ac-html ac-helm ac-dcd)))
 '(puppet-lint-command
   "puppet-lint --with-context --no-autoloader_layout-check --log-format \"%{path}:%{line}: %{kind}: %{message} (%{check})\"")
 '(restclient-log-request t)
 '(safe-local-variable-values (quote ((c-indent-level . 8))))
 '(tab-width 4)
 '(tls-program
   (quote
	("gnutls-cli --x509keyfile=/home/cgc/.docker/key.pem --x509certfile=/home/cgc/.docker/cert.pem --no-ca-verification -p %p %h"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; Evil mode
(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode 1)
(require 'evil-surround)
(global-evil-surround-mode 1)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "e" 'find-file
  "b" 'switch-to-buffer
  "k" 'kill-buffer
  "q" 'evil-quit
  "n" 'neotree-toggle
  "pp" 'helm-projectile-switch-project
  "pf" 'helm-projectile
  "ps" 'helm-projectile-ag
  "pa" 'helm-projectile-find-file-in-known-projects
  )

;; NeoTree
(setq neo-smart-open t)
(add-hook 'neotree-mode-hook
 (lambda ()
(define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
(define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
(define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
(define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

(defun gnutls-available-p ()
  "Function redefined in order not to use built-in GnuTLS support"
  nil)

;; Fix the ansi-color in compilation buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Color theme
(load-theme 'monokai t)

;; Global line number
(global-linum-mode t)
(require 'linum-relative)
(linum-relative-on)

;; Start up customization
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t)

;; Auto-save buffer
(defun save-buffer-if-visiting-file (&optional args)
  "Save the current buffer only if it is visiting a file"
  (interactive)
  (if (and (buffer-file-name) (buffer-modified-p))
      (save-buffer args)))
(add-hook 'auto-save-hook 'save-buffer-if-visiting-file)
(setq auto-save-interval 1
      auto-save-timeout 1)

;; File encoding utf-8-unix
(setq-default buffer-file-coding-system 'utf-8-unix)
(setq-default default-buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

;; Scrolling
(require 'smooth-scrolling)
(smooth-scrolling-mode 1)
(global-set-key (kbd "C-n")
(lambda () (interactive) (next-line 1)))
(global-set-key (kbd "C-p")
(lambda () (interactive) (previous-line 1)))

;; Window navigation
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

;; Code navigation
(global-set-key (kbd "C-M-d") 'up-list)
(global-set-key (kbd "C-c k u") 'kill-backward-up-list)

;; Move Buffer
(global-set-key (kbd "C-c C-<up>")  'buf-move-up)
(global-set-key (kbd "C-c C-<down>")  'buf-move-down)
(global-set-key (kbd "C-c C-<left>")  'buf-move-left)
(global-set-key (kbd "C-c C-<right>")  'buf-move-right)

;; Switch window
(require 'switch-window)
(global-set-key (kbd "C-x o") 'switch-window)

;; Ace jump mode
(global-set-key  (kbd "C-c SPC") 'ace-jump-mode)

;; Go to last change
(global-set-key (kbd "C-c c") 'goto-last-change)

;; Popup window manager
(require 'popwin)
(popwin-mode 1)
(global-set-key (kbd "C-z") popwin:keymap)

;; Direx directory management
(push '(direx:direx-mode :position left :width 25 :dedicated t)
      popwin:special-display-config)
(global-set-key (kbd "C-x t") 'direx:jump-to-directory-other-window)

;; Save all temporary files in /tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Key Bindings for find files
(global-set-key (kbd "C-x f") 'find-file-in-repository)
(global-set-key (kbd "C-c C-f") 'find-name-dired)

;; Helm
(setq helm-ff-transformer-show-only-basename nil
      helm-adaptive-history-file             "~/.emacs.d/data/helm-history"
      helm-yank-symbol-first                 t
      helm-move-to-line-cycle-in-source      t
      helm-buffers-fuzzy-matching            t
      helm-ff-auto-update-initial-value      t)

(autoload 'helm-descbinds      "helm-descbinds" t)
(autoload 'helm-eshell-history "helm-eshell"    t)
(autoload 'helm-esh-pcomplete  "helm-eshell"    t)

(global-set-key (kbd "C-h a")    #'helm-apropos)
(global-set-key (kbd "C-h i")    #'helm-info-emacs)
(global-set-key (kbd "C-h b")    #'helm-descbinds)

(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "TAB")     #'helm-esh-pcomplete)
              (define-key eshell-mode-map (kbd "C-c C-l") #'helm-eshell-history)))

(global-set-key (kbd "C-x b")   #'helm-mini)
(global-set-key (kbd "C-x C-b") #'helm-buffers-list)
(global-set-key (kbd "M-x")     #'helm-M-x)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "C-x C-r") #'helm-recentf)
(global-set-key (kbd "C-x r l") #'helm-filtered-bookmarks)
(global-set-key (kbd "M-y")     #'helm-show-kill-ring)
(global-set-key (kbd "M-s o")   #'helm-swoop)
(global-set-key (kbd "M-s /")   #'helm-multi-swoop)

(global-set-key (kbd "C-c C-g") #'helm-do-ag-project-root)
(global-set-key (kbd "C-c M-g") #'helm-do-grep-ag)

(require 'helm-config)
(helm-mode t)
;;(helm-adaptative-mode t)		
(global-set-key (kbd "C-x c!")   #'helm-calcul-expression)
(global-set-key (kbd "C-x c:")   #'helm-eval-expression-with-eldoc)
(define-key helm-map (kbd "M-o") #'helm-previous-source)
(global-set-key (kbd "M-s s")    #'helm-do-ag-this-file)

;; Projectfile and helm
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; helm-gtags
(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t
 )
(require 'helm-gtags)

;; Enable helm-gtags-mode
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
;; (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

;; Helm - Evil
(define-key helm-map (kbd "C-j") 'helm-next-line)
(define-key helm-map (kbd "C-k") 'helm-previous-line)

;; New line without breaking the current one
;; 1. move to end of the line.
;; 2. insert newline with index
(defun newline-without-break-of-line ()
  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))

(global-set-key (kbd "<C-return>") 'newline-without-break-of-line)

;; Duplicate line
(global-set-key "\C-c\C-d" "\C-a\C- \C-n\M-w\C-y")

;; Copy line
(defun quick-copy-line ()
  "Copy the whole line that point is on and move to the beginning of the next line.
    Consecutive calls to this command append each line to the
    kill-ring."
  (interactive)
  (let ((beg (line-beginning-position 1))
		(end (line-beginning-position 2)))
    (if (eq last-command 'quick-copy-line)
		(kill-append (buffer-substring beg end) (< end beg))
      (kill-new (buffer-substring beg end))))
  (beginning-of-line 2))

(global-set-key (kbd "C-c C-l") 'quick-copy-line)

;; Move up the current line.
(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

;; Move down the current line.
(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key [(control shift up)]  'move-line-up)
(global-set-key [(control shift down)]  'move-line-down)

;; Comment line
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; Rename file in buffer
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(global-set-key (kbd "C-c r") 'rename-file-and-buffer)

;; Get file path
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
			'(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(defun show-file-name ()
 "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(global-set-key [C-f1] 'show-file-name) ; Or any other key you want

;; Auto pair
(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers

;; Delete pair
(global-set-key (kbd "C-c k p") 'delete-pair)

;; Delete trailing white spaces
(global-set-key (kbd "C-c C-w") 'delete-trailing-whitespace)

;; Align
(global-set-key (kbd "C-c C-r") 'align-regexp)

;; Wrap region
(wrap-region-mode t)

;; Expand region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Flyspell
(setq ispell-program-name "hunspell")
(setq ispell-local-dictionary "en_US")
(setq ispell-local-dictionary-alist
      '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)))

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(global-set-key (kbd "<f6>") 'flyspell-mode)
(eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-;") 'helm-flyspell-correct))
(global-set-key (kbd "C-M-w") 'flyspell-auto-correct-previous-word)

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Shell
(global-set-key (kbd "<f5>") 'shell)
(exec-path-from-shell-initialize)

;; Company mode
(setq company-minimum-prefix-length 1)
(setq company-tooltip-limit 10)                      ; bigger popup window
(setq company-idle-delay 0)                          ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)                          ; remove annoying blinking


;; Ruby
(rvm-use-default) ;; use rvm's default ruby for the current Emacs session

(add-to-list 'auto-mode-alist
			 '("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist
			 '("\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))
(add-hook 'ruby-mode-hook 'aggressive-indent-mode)
(add-hook 'ruby-mode-hook 'ruby-tools-mode)
(add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch)
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'robe-mode-hook 'ac-robe-setup)
;; (add-hook 'ruby-mode-hook 'company-mode)
;; (add-hook 'ruby-mode-hook
;; 	  (lambda ()
;; 	    (setq-local company-backends '((company-robe)))))
(add-hook 'robe-mode-hook
		  (lambda ()
			(local-set-key (kbd "C-c C-s") '(lambda ()
											  (interactive)
											  (inf-ruby)
											  (robe-start)))))

(add-hook 'robe-mode-hook (lambda ()
							(local-set-key (kbd "C-c C-j") 'robe-jump-to-module)))

(add-hook 'ruby-mode-hook
		  (lambda() 
			(local-set-key  (kbd "C-c o") 'ff-find-other-file)))

(add-hook 'ruby-mode-hook 'fixmee-mode)
(add-hook 'ruby-mode-hook
		  (lambda ()
			(font-lock-add-keywords nil
									'(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))

;; Yas-snippets
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))
(yas-global-mode 1) ;; or M-x yas-reload-all if you've started YASnippet already.

;; Magit
(global-set-key (kbd "C-c g") 'magit-status)
(require 'evil-magit)

;; Puppet
(autoload 'puppet-mode "puppet-mode" "Major mode for editing puppet manifests")
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))
;;(require 'flymake-puppet)
;;(add-hook 'puppet-mode-hook (lambda () (flymake-puppet-load)))

;; Org Mode
;; fontify code in code blocks
(setq org-src-fontify-natively t)

(defun org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
   (let ((src-code-types
          '("emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++" "css"
            "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
            "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
            "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
            "scheme" "sqlite")))
     (list (ido-completing-read "Source code type: " src-code-types))))
  (progn
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code)))

;; auto-complete mode
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
			 "~/.emacs.d/elpa/auto-complete-20151112.2030/dict")
(ac-config-default)
(setq ac-ignore-case nil)
(add-to-list 'ac-modes 'ruby-mode)
(add-to-list 'ac-modes 'web-mode)

;; auto-complete
;; dirty fix for having AC everywhere
(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
                       (if (not (minibufferp (current-buffer)))
						   (auto-complete-mode 1))
                       ))
(real-global-auto-complete-mode t)

;; Markdown
(setq markdown-command "~/.emacs.d/bin/flavor.rb")

;; Go
;;(load "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el")
(load "$GOPATH/src/golang.org/x/tools/cmd/guru/go-guru.el")
(load "$GOPATH/src/github.com/dougm/goflymake/go-flycheck.el")

(eval-after-load 'flycheck
  '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))

(defun my-go-mode-hook ()
  (setq gofmt-command "goimports")
  ;;  (add-hook 'before-save-hook 'gofmt-before-save)
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
		   "go build -v && go test -v && go vet"))
  (local-set-key (kbd "C-c C-f") 'gofmt)
  (local-set-key (kbd "C-c C-m") 'godoc)
  (local-set-key (kbd "C-c C-g") 'go-goto-imports)
  (local-set-key (kbd "C-c C-u") 'go-remove-unused-imports)
  (local-set-key (kbd "C-c C-p") 'go-import-add)  
  (define-key evil-normal-state-map (kbd "C-]") 'godef-jump)
  (define-key evil-normal-state-map (kbd "C-t") 'pop-tag-mark))

(add-hook 'go-mode-hook 'my-go-mode-hook)

(add-hook 'go-mode-hook 'company-mode)
(add-hook 'go-mode-hook (lambda ()
						  (set (make-local-variable 'company-backends) '(company-go))
						  (company-mode)))
(add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'go-mode-hook
		  (lambda() 
			(local-set-key  (kbd "C-c o") 'ff-find-other-file)))

(add-hook 'go-mode-hook 'fixmee-mode)
(add-hook 'go-mode-hook
		  (lambda ()
			(font-lock-add-keywords nil
									'(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))
(add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)

(define-key go-mode-map (kbd "C-c C-t f") 'go-test-current-file)
(define-key go-mode-map (kbd "C-c C-t t") 'go-test-current-test)
(define-key go-mode-map (kbd "C-c C-t p") 'go-test-current-project)
(define-key go-mode-map (kbd "C-c C-t b") 'go-test-current-benchmark)
(define-key go-mode-map (kbd "C-c C-t x") 'go-run)

(define-key go-mode-map (kbd "C-c C-r") 'go-rename)

;; Textmate mode
(require 'textmate)
(textmate-mode)

;; Python
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
;;(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

;; C++
;; swhitch between header and imlementation
(add-hook 'c-mode-common-hook
		  (lambda() 
			(local-set-key  (kbd "C-c o") 'ff-find-other-file)))
(add-hook 'c-mode-common-hook 'fixmee-mode)
(add-hook 'c-mode-common-hook
		  (lambda ()
			(font-lock-add-keywords nil
									'(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))

;; imenu
(global-set-key (kbd "s-i") 'imenu)

;; ediff
(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set)
                'set-default)
            ',variable ,value))

(csetq ediff-window-setup-function 'ediff-setup-windows-plain)
;;(csetq ediff-split-window-function 'split-window-horizontally)
(csetq ediff-diff-options "-w")
(winner-mode)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)


;; Rust
;; Set path to racer binary
(setq racer-cmd "/Users/cosmin/.cargo/bin/racer")

;; Set path to rust src directory
(setq racer-rust-src-path "/Users/cosmin/.rust/src")

;; Load rust-mode when you open `.rs` files
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(add-to-list 'auto-mode-alist '("\\.toml\\'" . rust-mode))

;; Setting up configurations when you load rust-mode
(add-hook 'rust-mode-hook

		  '(lambda ()

			 (cargo-minor-mode)
			 
			 ;; Set the backend for company
			 (setq company-racer-executable "/Users/cosmin/.cargo/bin/racer")

			 ;; Enable racer
			 (racer-mode)

			 ;; Hook in racer with eldoc to provide documentation
			 (racer-turn-on-eldoc)

			 ;; Enable flycheck 
			 (flycheck-mode)

			 ;; Use flycheck-rust in rust-mode
			 (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

			 ;; Compnay mode
			 (company-mode)

			 ;; Use company-racer in rust mode
			 (set (make-local-variable 'company-backends) '(company-racer))

			 ;; Key binding to jump to method definition
			 (local-set-key (kbd "M-.") #'racer-find-definition)

			 ;; Key binding to auto complete and indent
			 (local-set-key (kbd "TAB") #'racer-complete-or-indent)

			 ;; Format buffer 
			 (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))


;; org mode
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files (file-expand-wildcards "~/org/*.org"))
;; Headlines
'("^\\(\\**\\)\\(\\* \\)\\(.*\xa\\)" (1 (org-get-level-face 1))
  (2 (org-get-level-face 2)) (3 (org-get-level-face 3)))
(setq org-return-follows-link t)
(defun org-todo-at-date (date)
  (interactive (list (org-time-string-to-time (org-read-date))))
  (cl-flet ((org-current-effective-time (&rest r) date)
			(org-today (&rest r) (time-to-days date)))
    (org-todo)))

;; avy
(global-set-key (kbd "M-s w")   #'avy-goto-word-1)


;; multiple cursor
(global-set-key (kbd "C-c s-.") 'mc/edit-lines)
(global-set-key (kbd "s-,") 'mc/mark-next-like-this)
(global-set-key (kbd "s-.") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c s-,") 'mc/mark-all-like-this)

;; yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs (list (concat user-emacs-directory "yasnippet-snippets")))
(yas-reload-all)
(add-hook 'go-mode-hook #'yas-minor-mode)

;; join line
(global-set-key (kbd "C-^")   #'join-line)
