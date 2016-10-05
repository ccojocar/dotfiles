;;; serverspec-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "serverspec" "serverspec.el" (22213 59908 805958
;;;;;;  13000))
;;; Generated autoloads from serverspec.el

(defvar serverspec::key-map (make-sparse-keymap) "\
Keymap for Serverspec.")

(autoload 'serverspec "serverspec" "\
Serverspec minor mode.

\(fn &optional ARG)" t nil)

(defvar serverspec::helm-spec-files-source '((name . "Spec files") (candidates . serverspec::list-spec-files) (display-to-real . serverspec::helm-spec-files-display-to-real) (action . find-file)) "\
Spec file helm source.")

(autoload 'serverspec::find-spec-files "serverspec" "\
Find spec files.

\(fn)" t nil)

(autoload 'serverspec::snippets-initialize "serverspec" "\


\(fn)" nil nil)

(eval-after-load 'yasnippet '(serverspec::snippets-initialize))

(autoload 'serverspec::dict-initialize "serverspec" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads nil nil ("serverspec-pkg.el") (22213 59907 413945
;;;;;;  790000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; serverspec-autoloads.el ends here
