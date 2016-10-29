;; Elmpa package manager
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
 '(delete-selection-mode t)
 '(evil-search-module (quote evil-search))
 '(exec-path
   (quote
	("/usr/bin" "/bin" "/usr/sbin" "/sbin" "/Applications/Emacs.app/Contents/MacOS/libexec" "/Applications/Emacs.app/Contents/MacOS/bin" "/Users/cosmin/go/bin" "/usr/local/go/bin")))
 '(gofmt-command "goimports")
 '(helm-gtags-auto-update t)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-path-style (quote relative))
 '(ns-command-modifier (quote control))
 '(ns-control-modifier (quote super))
 '(package-selected-packages
   (quote
	(linum-relative relative-line-numbers hydra evil-search-highlight-persist evil-surround evil-org evil-magit evil-visual-replace evil ob-typescript tide tss typescript-mode smooth-scrolling elpy jedi protobuf-mode yaml-mode web-mode web textmate switch-window serverspec rvm rustfmt ruby-tools ruby-refactor ruby-interpolation ruby-end ruby-electric ruby-dev ruby-block ruby-additional rspec-mode rsense robe real-auto-save rake racer puppetfile-mode puppet-mode projectile-speedbar projectile-codesearch popwin popup-kill-ring org-repo-todo neotree monokai-theme markdown-mode+ magit-topgit magit-gitflow magit-find-file latex-preview-pane latex-extra helm-swoop helm-rubygems-org helm-robe helm-projectile helm-gtags helm-go-package helm-git-grep helm-git helm-flyspell helm-flymake helm-flycheck helm-company helm-anything helm-ag-r helm-ag helm-ad goto-last-change gotest go-stacktracer go-snippets go-projectile go-errcheck go-complete go-autocomplete git fuzzy-match flymake-yaml flymake-ruby flymake-puppet flycheck-rust flycheck-pos-tip fixmee finder+ find-things-fast find-file-in-repository find-dired+ files+ exec-path-from-shell erlang emr dockerfile-mode docker dired+ diffview company-web company-restclient company-racer company-quickhelp company-jedi company-inf-ruby cargo bundler buffer-move aggressive-indent ac-inf-ruby)))
 '(racer-cmd "/Users/cosmin/.cargo/bin/racer" t)
 '(safe-local-variable-values (quote ((c-indent-level . 8))))
 '(tab-width 4)
 '(textmate-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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
(real-global-auto-complete-mode t)

;; Fix the ansi-color in compilation buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Fly spell 
(global-set-key (kbd "<f6>") 'flyspell-mode)

;; Textmate 
(textmate-mode)

;; imenu
(global-set-key (kbd "s-i") 'imenu)

;; Evil mode
(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode 1)
(require 'evil-surround)
(global-evil-surround-mode 1)

(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "b" 'switch-to-buffer
  "e" 'find-file
  "f" 'find-name-dired
  "r" 'find-file-in-repository
  "k" 'kill-buffer
  "," 'goto-last-change
  "n" 'neotree-toggle
  "cl" 'comment-or-uncomment-region-or-line
  "cr" 'comment-or-uncomment-region)

;; NeoTree
(setq neo-smart-open t)
(add-hook 'neotree-mode-hook
		  (lambda ()
			(define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
			(define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
			(define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
			(define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))
(require 'evil-magit)

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
(global-set-key (kbd "C-x o") 'switch-window)

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
(autoload 'helm-eshell-history "helm-eshell"    t)
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

(global-set-key (kbd "M-x")     #'helm-M-x)
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
(define-key helm-map (kbd "C-j") 'helm-next-line)
(define-key helm-map (kbd "C-k") 'helm-previous-line)
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
(helm-projectile-on)
(require 'go-projectile)

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

;; Magit
(global-set-key (kbd "C-c g") 'magit-status)

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
(setq yas/root-directory "~/.emacs.d/yasnippet-snippets")
(yas-reload-all)
(add-hook 'go-mode-hook #'yas-minor-mode)

;; Ruby
(rvm-use-default)
(add-hook 'ruby-mode-hook 'ruby-tools-mode)
(add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch)
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'robe-mode-hook 'ac-robe-setup)
(evil-leader/set-key-for-mode 'robe-mode
  "gm" 'robe-jump-to-module
  )

;; Python
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

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
(exec-path-from-shell-copy-env "GOPATH")
(exec-path-from-shell-copy-env "GOROOT")
(load "$GOPATH/src/github.com/dougm/goflymake/go-flycheck.el")
(load "$GOPATH/src/golang.org/x/tools/cmd/guru/go-guru.el")

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

(add-hook 'go-mode-hook
		  (lambda()
			;; gofmt configuration
			(setq gofmt-command "goimports")
			(add-hook 'before-save-hook 'gofmt-before-save)

			;; compiler configuration
			(if (not (string-match "go" compile-command))
				(set (make-local-variable 'compile-command)
					 "go build -v && go test -v && go vet"))

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
