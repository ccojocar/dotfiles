;;; env-var-import-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "env-var-import" "env-var-import.el" (22112
;;;;;;  33266 864510 635000))
;;; Generated autoloads from env-var-import.el

(autoload 'read-shell-env "env-var-import" "\
Read all environment variables specified in VARS and return their values in a hash table.

\(fn VARS)" nil nil)

(autoload 'env-var-import "env-var-import" "\
Import environment variables from the shell.
`env-var-import-exec-path-var` is imported and used to set `exec-path`.
Any variables specified in OTHER-VARS are imported as well.

\(fn &optional OTHER-VARS)" nil nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; env-var-import-autoloads.el ends here
