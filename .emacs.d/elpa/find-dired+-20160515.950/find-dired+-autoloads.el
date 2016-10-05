;;; find-dired+-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "find-dired+" "find-dired+.el" (0 0 0 0))
;;; Generated autoloads from find-dired+.el

(autoload 'find-dired "find-dired+" "\
Run `find' and put its output in a buffer in Dired Mode.
Then run `find-dired-hook' and `dired-after-readin-hook'.
The `find' command run (after changing into DIR) is essentially this,
where LS-SWITCHES is `(car find-ls-option)':

  find . \\( ARGS \\) LS-SWITCHES

Optional args:

* DEPTH-LIMITS:   Minimum and maximum depths: (MIN-DEPTH MAX-DEPTH).
* EXCLUDED-PATHS: Strings matching paths to be excluded.
                  Uses `find' switch `-path'.

When both optional args are non-nil, the `find' command run is this:

  find . -mindepth MIN-DEPTH -maxdepth MAX-DEPTH
         \\( -path *edir1* -o -path *edir2* ... \\)
         -prune -o \\( ARGS \\) LS-SWITCHES

\(fn DIR ARGS &optional DEPTH-LIMITS EXCLUDED-PATHS)" t nil)

(autoload 'find-name-dired "find-dired+" "\
Search directory DIR recursively for files matching globbing PATTERN,
and run `dired' on those files.  PATTERN may use shell wildcards, and
it need not be quoted.  It is not an Emacs regexp.

By default, the command run (after changing into DIR) is essentially
this, where LS-SWITCHES is `(car find-ls-option)':

  find . -name 'PATTERN' LS-SWITCHES

See `find-name-arg' to customize the `find' file-name pattern arg.

Optional arg DEPTH-LIMITS is a list (MIN-DEPTH MAX-DEPTH) of the
 minimum and maximum depths.  If nil, search directory tree under DIR.

Optional arg EXCLUDED-PATHS is a list of strings that match paths to
 exclude from the search.  If nil, search all directories.

When both optional args are non-nil, the `find' command run is this:

  find . -mindepth MIN-DEPTH -maxdepth MAX-DEPTH
         \\( -path *edir1* -o -path *edir2* ... \\)
         -prune -o name 'PATTERN' \\( ARGS \\) LS-SWITCHES

\(fn DIR PATTERN &optional DEPTH-LIMITS EXCLUDED-PATHS)" t nil)

(autoload 'find-grep-dired "find-dired+" "\
Find files in DIR containing a regexp REGEXP.
The output is in a Dired buffer.
The `find' command run (after changing into DIR) is essentially this,
where LS-SWITCHES is `(car find-ls-option)':

  find . -exec grep find-grep-options REGEXP {} \\; LS-SWITCHES

Thus REGEXP can also contain additional grep options.

Optional arg DEPTH-LIMITS is a list (MIN-DEPTH MAX-DEPTH) of the
 minimum and maximum depths.  If nil, search directory tree under DIR.

Optional arg EXCLUDED-PATHS is a list of strings that match paths to
 exclude from the search.  If nil, search all directories.

When both optional args are non-nil, the `find' command run is this:

  find . -mindepth MIN-DEPTH -maxdepth MAX-DEPTH
         \\( -path *edir1* -o -path *edir2* ... \\)
         -prune -o -exec grep find-grep-options REGEXP {} \\;
         LS-SWITCHES

\(fn DIR REGEXP &optional DEPTH-LIMITS EXCLUDED-PATHS)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; find-dired+-autoloads.el ends here
