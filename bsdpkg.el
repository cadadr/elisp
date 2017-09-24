;;; bsdpkg.el --- Emacs interface for FreeBSD pkg(1)  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Göktuğ Kayaalp

;; Author: Göktuğ Kayaalp <self@gkayaalp.com>
;; Keywords: unix, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;;; Synopsis:

;; ‘bsdpkg’ is an Emacs interface to FreeBSD pkg(1).  It's planned to
;; genericise the package to support all *BSD packaging systems, and
;; the module is written with that sort of extensibility in mind.

;;;; Notes:



;;; Code:

(require 'cl-lib)
(require 'tabulated-list)
(require 'button)
(require 'tramp)



;;;; Configuration:

;; TODO: Extend as this is (possibly) ported around.
(defvar bsdpkg-system
  (let ((freebsd (string-match "freebsd" system-configuration)))
    (cond (freebsd
           (cons 'freebsd
                 (substring system-configuration (+ freebsd 7))))))
  "Operating system and type.
A cons pair of a symbol and a string, the system type and the
system version, respectively.  The default value of this variable
is calculated via a heuristic, but the user should make sure that
it corresponds to their system, and possibly help improve it for
their system.  The meaning of the car of this pair is
self-evident, but here is a list of expected values and their
meanings:

- freebsd: FreeBSD, GhostBSD, other compatible FreeBSD derivatives
- openbsd: OpenBSD
- dragonfly: DragonflyBSD
- netbsd: NetBSD

At the time being, the module works only on FreeBSD.")

(defvar bsdpkg-command
  (cl-case (car bsdpkg-system)
    ('freebsd (let ((ver (mapcar #'string-to-number
                                 (split-string (cdr bsdpkg-system)
                                               "\\."))))
                ;; pkg was introduced in FreeBSD 9.1.  The system
                ;; before that was comprised of multiple executables.
                (when (or (and (= (car ver) 9) (>= (cadr ver) 1))
                          (and (> (car ver) 9)))
                  (or (executable-find "pkg")
                      (error "‘pkg’ is not installed on Freebsd %d.%d"
                             (car ver) (cadr ver)))))))
  "The path to the binary packaging system's main executable.
The default value of this variable is computed based on that of
‘bsdpkg-system’.  If the packaging system does not have a main
executable like FreeBSD's ‘pkg’, then this variable's value will
be set to nil.")



;;;; Utilites:

;;;;; Buffers:
(defvar bsdpkg--scratch-buf "*bsdpkg process interaction*")

(defun bsdpkg--scratch ()
  "Return the buffer for storing and processing command outputs.
Erases the buffer before returning."
  (let ((bufnam bsdpkg--scratch-buf))
    (when-let (buf (get-buffer bufnam)) (kill-buffer buf))
    (generate-new-buffer bufnam)))

(defun bsdpkg--listing ()
  "Return the package listing buffer.
Erases the buffer before returning."
  (with-current-buffer (get-buffer-create "*bsdpkg search*")
    (fundamental-mode)
    (read-only-mode -1)
    (erase-buffer)
    (current-buffer)))

(defun bsdpkg--descrbuf (pkg)
  "Return the package description buffer for PKG.
Erases the buffer before returning."
  (with-current-buffer (get-buffer-create (format "*bsdpkg descr (%s)*" pkg))
    (fundamental-mode)
    (read-only-mode -1)
    (erase-buffer)
    (current-buffer)))



;;;;; Strings:

(defvar bsdpkg--err-unexpected-ret "Process terminated with unexpected error (%d)")



;;;; Portability abstractions:

;; The functions in this section provide an abstraction for running
;; common features w/o needing to know the underlying system.  The
;; user interface functions may only call these.

(defun bsdpkg--system ()
  "Return a symbol denoting the system type.
This is equivalent to (car ‘bsdpkg-system’)"
  (car bsdpkg-system))

(defun bsdpkg--system-name ()
  "Return a stylized name for the current system."
  (cl-case (bsdpkg--system)
    ('freebsd "FreeBSD")
    ('openbsd "OpenBSD")
    ('dragonfly "DragonflyBSD")
    ('netbsd "NetBSD")
    (otherwise "UNKNOWN SYSTEM")))

(defun bsdpkg--call (feature &rest args)
  "Execute the function implementing FEATURE for current system.
The FEATURE function is run on ARGS with ‘apply’.  Will raise a
‘not-implemented’ error, whose cdr is a cons pair, the FEATURE
symbol itself and a default error message."
  (let ((sym (intern (format "bsdpkg--%s--%s" (bsdpkg--system) feature))))
    (unless (functionp sym)
      (error "‘%s’ not implemented on %s" feature (bsdpkg--system-name)))
    (apply sym args)))



;;;;; FreeBSD:
;;;;;; Call pkg-ng:

(defun bsdpkg--freebsd-call-pkg-ng (&rest args)
  "Call FreeBSD pkg(1) command with ARGS.
ARGS are a list of strings, each an argument passed to the
command, a la ‘call-process’.  If the process returns 0, return
the output (may be empty string), signal otherwise, with
appropriate info attached."
  (let* ((buf (bsdpkg--scratch))
         (ret (apply 'call-process
                     bsdpkg-command nil `(,buf nil) nil args)))
    (if (zerop ret)
        (with-current-buffer buf (buffer-string))
      ret)))

(defun bsdpkg--freebsd-call-pkg-ng-su (&rest args)
  "Like ‘bsdpkg--freebsd-call-pkg-ng’ but as sudo."
  (with-current-buffer (bsdpkg--scratch)
    (cd (concat "/sudo::" (expand-file-name default-directory)))
    (prog1
        (shell-command
         (concat bsdpkg-command  " "
                 (mapconcat (lambda (c) (concat "'" c "'")) args " ")) (current-buffer))
      (cd default-directory))))



;;;;;; Search:

(defun bsdpkg--freebsd-pkg-ng-search (pkg)
  "Search pakcages PKG using pkg(1)."
  (let ((ret (bsdpkg--freebsd-call-pkg-ng "search" pkg)))
    (cond
     ((stringp ret)
      (with-temp-buffer
        (insert ret) (goto-char (point-min))
        (let ((line (buffer-substring (line-beginning-position) (line-end-position)))
              (go t) data)
          (while go
            (unless
                (equal 0 (string-match "^\\([^ ]+\\) +\\(.*\\)$" line))
              (error "Regexp did not match line while searching"))
            (setf data (nconc data 
                              (list (cons (match-string 1 line)
                                          (match-string 2 line)))))
            ;; Break on EOF.
            (if (= (1+ (line-end-position)) (point-max))
                (setf go nil)
              (progn
                (forward-line)
                (setf line (buffer-substring (line-beginning-position)
                                             (line-end-position))))))
          data)))
     ((eq 70 ret) nil)
     (:otherwise (error bsdpkg--err-unexpected-ret ret)))))

(defun bsdpkg--freebsd--search (pkg)
  "Return a list of matching packages.
PKG is a regular expression.  Returns a list of lists of three
strings: the package name, version and package comment,
respectively."
  (unless bsdpkg-command
    (throw :not-implemented 'search))
  (bsdpkg--freebsd-pkg-ng-search pkg))



;;;;;; Package completion:

;; see ‘(elisp)Programmed Completion’.
(defun bsdpkg--freebsd-completions (prefix pred flag)
  "Complete package names for PREFIX."
  (when (> (length prefix) 2)
    (mapcar 'car (bsdpkg--call 'search (concat "^" prefix)))))

(defun bsdpkg--freebsd--complete-pkg ()
  "Completing-read the name of a single, existing package."
  (completing-read "Package name (at least three letters to auto-complete): "
                   'bsdpkg--freebsd-completions nil t))



;;;;;; Describe:

(defun bsdpkg--freebsd--describe (pkg)
  "Return a string describing PKG."
  (let ((ret (bsdpkg--freebsd-call-pkg-ng
              "search"  "-Q" "name" "-Q" "version"  "-Q" "repository" 
              "-Q" "categories" "-Q" "license" "-Q" "maintainer" "-Q" "size"
              "-Q" "comment" "-Q" "options" "-Q" "annotations" 
              "-Q" "pkg-size" "-Q" "depends-on" "-Q" "description" 
              (format "^%s$" pkg))))
    (if (stringp ret)
        ret
      (if (eq 70 ret)
          (user-error "Query ‘%s’ did not match anything" pkg)
        (error "Process returned error (%S) when querying ‘%s’" ret pkg)))))

;;;;;; Install/Remove:

(defun bsdpkg--freebsd--installed-p (pkg)
  "Check whether or not PKG is installed.
Returns t if installed, nil otherwise."
  (let ((ret (bsdpkg--freebsd-call-pkg-ng "info" "-e" pkg)))
    (cond ((stringp ret) t)
          ((and (numberp ret) (not (= 1 ret)))
           (error bsdpkg--err-unexpected-ret ret)))))

(defun bsdpkg--freebsd--install (pkg)
  "Install PKG.
Ensure via ‘yes-or-no-p’ before installing."
  (if (bsdpkg--freebsd--installed-p pkg)
      (user-error "‘%s’ is already installed" pkg)
    (when (yes-or-no-p (format "Install %s? " pkg))
      (let ((ret (bsdpkg--freebsd-call-pkg-ng-su "install" "-y" pkg)))
        (cond ((equal 0 ret) (with-current-buffer bsdpkg--scratch-buf
                               (buffer-string)))
              ((equal 70 ret) (user-error "Package not found (%s)" pkg))
              ((equal 77 ret) (user-error "User %s cannot install packages"
                                          (user-login-name) pkg))
              (t (error bsdpkg--err-unexpected-ret ret)))))))

(defun bsdpkg--freebsd--remove (pkg)
  "Remove PKG.
Ensure via ‘yes-or-no-p’ before removing."
  (when (yes-or-no-p (format "Remove %s? " pkg))
    (let ((ret (bsdpkg--freebsd-call-pkg-ng-su "remove" "-y" pkg)))
      (cond ((equal 0 ret) (with-current-buffer bsdpkg--scratch-buf
                             (buffer-string)))
            ((equal 65 ret) (user-error "Package not found (%s)" pkg))
            ((equal 77 ret) (user-error "User %s cannot remove packages"
                                        (user-login-name) pkg))
            (t (error bsdpkg--err-unexpected-ret ret))))))



;;;; User interface:
;;;;; Helper functions:
;;;;;; Search:

(defun bsdpkg--search-table (pkgs)
  "Build a package search table out of PKGS."
  (switch-to-buffer
   (with-current-buffer (bsdpkg--listing)
     (setf tabulated-list-entries
           (mapcar 'bsdpkg--search-table-btnify pkgs))
     (setf tabulated-list-format [("Package" 30 nil)
                                  ("Comment" 90 nil)])
     (bsdpkg-search-mode)
     (current-buffer))))

(defun bsdpkg--search-table-btnify (entry)
  (interactive)
  (let ((pkg (car entry)))
    (list pkg
          (vector (cons pkg
                        `(action bsdpkg--search-table-btn-action pkg ,pkg))
                  (cdr entry)))))

(defun bsdpkg--search-table-btn-action (marker)
  (interactive)
  (bsdpkg-describe (button-get (button-at marker) 'pkg)))



;;;;;; Describe:

(defun bsdpkg--describe (pkg descr)
  "Create a description buffer for PKG and switch to it."
  (switch-to-buffer
   (with-current-buffer (bsdpkg--descrbuf pkg)
     (setq-local bsdpkg--pkg pkg)
     (let ((installed (bsdpkg--call 'installed-p pkg)))
       (if installed 
           (progn (insert "Installed, ")
                  (insert-text-button "Remove"
                                      'action (lambda (mark)
                                                (interactive)
                                                (bsdpkg--call 'remove pkg)
                                                (bsdpkg-describe pkg))))
         (insert-text-button "Install"
                             'action (lambda (mark)
                                       (interactive)
                                       (bsdpkg--call 'install pkg)
                                       (bsdpkg-describe pkg)))))
     (insert "\n\n")
     (insert descr)
     (goto-char (point-min))
     (bsdpkg-describe-mode)
     (current-buffer))))



;;;;; Modes:

(define-derived-mode bsdpkg-search-mode tabulated-list-mode
  "BSD Package Search"
  "Mode for interacting with package search results."
  (tabulated-list-init-header)
  (tabulated-list-print t)
  (hl-line-mode))

(let ((map bsdpkg-search-mode-map))
    (define-key map "s" 'bsdpkg-search))

(define-derived-mode bsdpkg-describe-mode special-mode
  "BSD Describe Package"
  "Mode for interacting with package status and details"
  (goto-address-mode +1))



;;;;; Public commands:

(defun bsdpkg-search (pkg)
  "Search the package repository for PKG.
PKG may be a regex that the OS packaging tool accepts. "
  (interactive (list (read-string "Package name (regexp): ")))
  (let ((pkgs (bsdpkg--call 'search pkg)))
    (if pkgs
        (bsdpkg--search-table pkgs)
      (user-error "No match for ‘%s’" pkg))))

(defun bsdpkg-describe (pkg)
  "Search the package repository for PKG."
  (interactive (list (bsdpkg--call 'complete-pkg)))
  (let ((descr (bsdpkg--call 'describe pkg)))
    (bsdpkg--describe pkg descr)))

(defun bsdpkg-install (pkg)
  "Install PKG."
  (interactive (list (bsdpkg--call 'complete-pkg)))
  (bsdpkg--call 'install pkg))

(defun bsdpkg-remove (pkg)
  "Remove PKG."
  (interactive (list (bsdpkg--call 'complete-pkg)))
  (bsdpkg--call 'remove pkg))

(defun bsdpkg-clean ()
  "Remove packages installed as a dependency for a now removed package."
  (interactive)
  (bsdpkg--call 'clean))

;; TODO: In search buffer, RET on a line shows package description
;; TODO: describe, install, remove, update, upgrade
;; TODO: Show package status (installed or not) in search buffer



;;;; Export:

(provide 'bsdpkg)
;;; bsdpkg.el ends here
