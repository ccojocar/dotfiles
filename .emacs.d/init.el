;;Package manager
(require 'package) 
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) 
(when (not package-archive-contents)
  (package-refresh-contents))

(setq ad-redefinition-action 'accept)

;;Main Configuration 
(custom-set-variables
 '(ac-auto-show-menu 0.1)
 '(ac-auto-start 1)
 '(ac-candidate-limit 10)
 '(ac-trigger-key "M-TAB")
 '(company-minimum-prefix-length 1)
 '(company-show-numbers t)
 '(company-tooltip-align-annotations t)
 '(company-selection-wrap-around t)
 '(delete-selection-mode t)
 '(evil-search-module (quote evil-search))
 '(exec-path
   (quote
	("/usr/bin" "/bin" "/usr/sbin" "/sbin"  "~/go/bin" "/usr/local/go/bin" "/usr/lib/go/bin")))
 '(exec-path-from-shell-check-startup-files nil)
 '(exec-path-from-shell-variables
   (quote
	("PATH" "GOROOT" "GOPATH" "GEM_HOME")))
 '(magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n256")))
 '(gofmt-command "goimports")
 '(helm-gtags-auto-update t)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-path-style (quote relative))
 '(jedi:use-shortcuts t)
 '(ns-command-modifier (quote control))
 '(ns-control-modifier (quote super))
 '(package-selected-packages
   (quote
	(enh-ruby-mode markup markdown-toc markdown-mode+ function-args rust-mode tern js-doc js2-closure ac-js2 js2-refactor js2-mode pylint py-autopep8 linum-relative relative-line-numbers hydra evil-search-highlight-persist evil-smartparens evil-args evil-surround evil-org evil-magit evil-visual-replace evil ob-typescript tide tss typescript-mode smooth-scrolling elpy jedi protobuf-mode yaml-mode web-mode web textmate switch-window serverspec rvm rustfmt ruby-tools ruby-refactor ruby-interpolation ruby-end ruby-electric ruby-dev ruby-block ruby-additional rspec-mode rsense robe real-auto-save rake racer puppet-mode projectile-speedbar projectile-codesearch popwin popup-kill-ring org-repo-todo neotree monokai-theme markdown-mode+ magit-topgit magit-gitflow magit-find-file latex-preview-pane latex-extra helm-swoop helm-rubygems-org helm-robe helm-projectile helm-gtags helm-go-package helm-ispell helm-git-grep helm-git helm-flyspell helm-flymake helm-flycheck helm-company helm-anything helm-ag-r helm-ag helm-ad goto-last-change gotest go-stacktracer go-snippets go-projectile go-errcheck go-complete go-autocomplete git fuzzy-match flymake-yaml flymake-ruby flymake-puppet flycheck-rust flycheck-pos-tip fixmee finder+ find-things-fast find-file-in-repository find-dired+ files+ exec-path-from-shell erlang emr dockerfile-mode docker dired+ diffview company-web company-restclient company-racer company-quickhelp company-jedi company-inf-ruby company-go cargo bundler buffer-move aggressive-indent ac-inf-ruby ac-html ac-helm)))
 '(racer-cmd "~/.cargo/bin/racer" t)
 '(safe-local-variable-values (quote ((c-indent-level . 8))))
 '(puppet-lint-command
   "puppet-lint --with-context --no-autoloader_layout-check --log-format \"%{path}:%{line}: %{kind}: %{message} (%{check})\"")
 '(restclient-log-request t)
 '(tab-width 4)
 '(textmate-mode t)
 )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Disable the scroll bar
(scroll-bar-mode -1)

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

;; Save all temporary files in /tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Move up and down the current line.
(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))
(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))
(global-set-key [(control shift up)]  'move-line-up)
(global-set-key [(control shift down)]  'move-line-down)

;; Aggresive indent
(global-aggressive-indent-mode 1)
(add-to-list 'aggressive-indent-excluded-modes 'html-mode)

;; Auto complete - everywhere
(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
                       (if (not (minibufferp (current-buffer)))
						   (auto-complete-mode 1))
                       ))
(run-with-idle-timer 2 nil (lambda () (real-global-auto-complete-mode t)))

;; Fix the ansi-color in compilation buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Fly spell 
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(global-set-key (kbd "<f6>") 'flyspell-mode)
(eval-after-load 'flyspell-mode
  (lambda()
	(setq ispell-program-name "hunspell")
	(setq ispell-local-dictionary "en_US")
	(setq ispell-local-dictionary-alist
		  '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)))
	(define-key flyspell-mode-map (kbd "C-;") 'helm-flyspell-correct)
	(define-key flyspell-mode-map (kbd "C-M-w") 'flyspell-auto-correct-previous-word)
	))

;; imenu
(global-set-key (kbd "s-i") 'imenu)

;;; TLS configuration
(defun gnutls-available-p ()
  "Function redefined in order not to use built-in GnuTLS support"
  nil)

;; Auto-save buffer
(defun save-buffer-if-visiting-file (&optional args)
  "Save the current buffer only if it is visiting a file"
  (interactive)
  (if (and (buffer-file-name) (buffer-modified-p))
      (save-buffer args)))
(add-hook 'auto-save-hook 'save-buffer-if-visiting-file)
(setq auto-save-interval 1
      auto-save-timeout 1)

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

(defun show-file-name ()
 "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

;; Evil mode
(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode 1)
(require 'evil-surround)
(global-evil-surround-mode 1)
(eval-after-load 'magit '(require 'evil-magit))
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "b" 'switch-to-buffer
  "e" 'find-file
  "f" 'find-name-dired
  "d" 'find-file-in-repository
  "k" 'kill-buffer
  "q" 'evil-quit
  "n" 'neotree-toggle
  "m" 'magit-status
  "r" 'rename-file-and-buffer
  "w" 'switch-window
  "," 'goto-last-change
  "%" 'show-file-name
  "cl" 'comment-or-uncomment-region-or-line
  "cr" 'comment-or-uncomment-region
  "pp" 'helm-projectile-switch-project
  "pf" 'helm-projectile
  "ps" 'helm-projectile-ag
  "pa" 'helm-projectile-find-file-in-known-projects)
(kill-buffer "*Messages*")

;; NeoTree
(setq neo-smart-open t)
(add-hook 'neotree-mode-hook
		  (lambda ()
			(define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
			(define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
			(define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
;; ESC ALL THE THINGS = <C-g>
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
		(define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

;; Scrolling
(require 'smooth-scrolling)
(smooth-scrolling-mode 1)
(global-set-key (kbd "C-n")
				(lambda () (interactive) (next-line 1)))
(global-set-key (kbd "C-p")
				(lambda () (interactive) (previous-line 1)))

;; Window navigation
(global-set-key (kbd "C-M-d") 'up-list)
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

;; Move Buffer 
(global-set-key (kbd "C-c C-<up>")  'buf-move-up)
(global-set-key (kbd "C-c C-<down>")  'buf-move-down)
(global-set-key (kbd "C-c C-<left>")  'buf-move-left)
(global-set-key (kbd "C-c C-<right>")  'buf-move-right)

;; Switch window
(require 'switch-window)

;; Popup window manager
(require 'popwin)
(popwin-mode 1)
(global-set-key (kbd "C-z") popwin:keymap)

;; Company mode
(setq company-minimum-prefix-length 0)
;; bigger popup window
(setq company-tooltip-limit 10)                   
;; decrease delay before autocompletion pupup shows
(setq company-idle-delay 0)
;; remove annoying blinking
(setq company-echo-delay 0)

;; Flycheck 
(evil-leader/set-key-for-mode 'flycheck-mode
  "hc" 'helm-flycheck
  )

;; Helm
(setq helm-ff-transformer-show-only-basename nil
      helm-adaptive-history-file             "~/.emacs.d/data/helm-history"
      helm-yank-symbol-first                 t
      helm-move-to-line-cycle-in-source      t
      helm-buffers-fuzzy-matching            t
      helm-ff-auto-update-initial-value      t)
(autoload 'helm-descbinds      "helm-descbinds" t)
(autoload 'helm-ehell-history "helm-eshell"    t)
(autoload 'helm-esh-pcomplete  "helm-eshell"    t)
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "TAB")     #'helm-esh-pcomplete)
              (define-key eshell-mode-map (kbd "C-c C-l") #'helm-eshell-history)))

(require 'helm-config)
(helm-mode t)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(eval-after-load 'helm-mode
  (lambda()
	(helm-projectile-on)
	(define-key helm-map (kbd "C-j") 'helm-next-line)
	(define-key helm-map (kbd "C-k") 'helm-previous-line)
	))

(global-set-key (kbd "M-x") #'helm-M-x)
(evil-leader/set-key
  "ha" 'helm-apropos
  "hi" 'helm-info-emacs
  "hd" 'helm-descbinds
  "hb" 'helm-buffers-list
  "hf" 'helm-find-files
  "hr" 'helm-recentf
  "hm" 'helm-filtered-bookmarks
  "hk" 'helm-show-kill-ring
  "hs" 'helm-swoop 
  "hg" 'helm-do-ag-this-file 
  )

(evil-leader/set-key-for-mode 'helm-gtags-mode
  "htt" 'helm-gtags-find-tag
  "htr" 'helm-gtags-find-rtag
  "hts" 'helm-gtags-find-symbol
  "htp" 'helm-gtags-parse-file
  "ht<" 'helm-gtags-previous-history
  "ht>" 'helm-gtags-next-history
  "htb" 'helm-gtags-pop-stack
  )

;; Projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)

;; Org mode
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

;; Ediff
(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set)
                'set-default)
            ',variable ,value))

(csetq ediff-window-setup-function 'ediff-setup-windows-plain)
(csetq ediff-diff-options "-w")
(winner-mode)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

;; Yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/yasnippet-snippets"))
(add-hook 'go-mode-hook #'yas-minor-mode)
(add-hook 'ruby-mode-hook #'yas-minor-mode)
(add-hook 'python-mode-hook #'yas-minor-mode)

;; Shell
(global-set-key (kbd "<f5>") 'shell)
(exec-path-from-shell-initialize)

;; Ruby
(eval-after-load 'ruby-mode '(rvm-use-default))
(add-to-list 'auto-mode-alist
			 '("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist
			 '("\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))

(add-hook 'ruby-mode-hook
		  (lambda()
			(aggressive-indent-mode)
			(ruby-tools-mode)
			(ruby-refactor-mode-launch)
			(robe-mode)
			(fixmee-mode)
			(font-lock-add-keywords nil
									'(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))
			(setq tags-add-tables t)
			(setq tags-table-list
				  (list (concat (getenv "GEM_HOME") "/gems")))
			))
(add-hook 'robe-mode-hook 'ac-robe-setup)

(evil-leader/set-key-for-mode 'robe-mode
  "gm" 'robe-jump-to-module
  "s"  '(lambda()
		  (interactive)
		  (inf-ruby)
		  (robe-strart))
  )

;; Python
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

(evil-leader/set-key-for-mode 'python-mode
  "gd" 'jedi:goto-definition
  "go" 'jedi:show-doc
  "gf" 'jedi:get-in-function-call
  )

;; C/C++
(evil-leader/set-key-for-mode 'c-mode
  "gh" 'ff-find-other-file
  )
(add-hook 'c-mode-common-hook
		  (lambda ()
			(font-lock-add-keywords nil
									'(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))
;; Go
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(eval-after-load 'go-mode
  (lambda()
	(exec-path-from-shell-copy-env "GOPATH")
	(exec-path-from-shell-copy-env "GOROOT")
	(load "$GOPATH/src/github.com/dougm/goflymake/go-flycheck.el")
	(load "$GOPATH/src/golang.org/x/tools/cmd/guru/go-guru.el")
	))

(add-hook 'go-mode-hook
		  (lambda()
			;; gofmt configuration
			(setq gofmt-command "goimports")
			(add-hook 'before-save-hook 'gofmt-before-save)

			;; compiler configuration
			(if (not (string-match "go" compile-command))
				(set (make-local-variable 'compile-command)
					 "go build -v && go test -v && go vet"))
			
			;; projectile
			(require 'go-projectile)

			;; company mode
			(company-mode)

			;; flycheck mode
			(flycheck-mode)

			;; fixme mode
			(fixmee-mode)   
			(font-lock-add-keywords nil
									'(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))
			;; go guru configuration
			(go-guru-hl-identifier-mode)

			;; key bindings 
			(define-key evil-normal-state-map (kbd "C-]") 'godef-jump)
			(define-key evil-normal-state-map (kbd "C-t") 'pop-tag-mark)
			))

(evil-leader/set-key-for-mode 'go-mode
  "gd" 'godef-jump
  "gf" 'gofmt
  "gi" 'go-goto-imports
  "go" 'godoc
  "gu" 'go-remove-unused-imports
  "gs" 'ff-find-other-file
  "gr" 'go-rename
  "gx" 'go-run
  "gtf" 'go-test-current-file
  "gtt" 'go-test-current-test
  "gtp" 'go-test-current-project
  "gtb" 'go-test-current-benchmark
  )

;; Rust
(setq racer-cmd "~/.cargo/bin/racer")
(setq racer-rust-src-path "~/.rust/src")
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(add-to-list 'auto-mode-alist '("\\.toml\\'" . rust-mode))

(evil-leader/set-key-for-mode 'rust-mode
  "gd" 'racer-find-definition
  )

(add-hook 'rust-mode-hook
		  '(lambda ()
			 ;; Cargo minor mode
			 (cargo-minor-mode)

			 ;; Racer mode
			 (racer-mode)

			 ;; Hook in racer with eldoc to provide documentation
			 (racer-turn-on-eldoc)

			 ;; Flycheck
			 (flycheck-mode)
			 (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

			 ;; Compnay mode
			 (setq company-racer-executable "~/.cargo/bin/racer")
			 (company-mode)
			 (set (make-local-variable 'company-backends) '(company-racer))

			 ;; key bindings 
			 (define-key evil-normal-state-map (kbd "C-]") 'racer-find-definition) 
			 (define-key evil-normal-state-map (kbd "C-t") 'pop-tag-mark)
			 (local-set-key (kbd "TAB") #'racer-complete-or-indent)
			 (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))

;; Puppet
(autoload 'puppet-mode "puppet-mode" "Major mode for editing puppet manifests")
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

;; Markdown
(setq markdown-command "~/.emacs.d/bin/flavor.rb")

