;;; setenv-file.el --- Set env variables from a file  -*- lexical-binding: t; -*-

;; Package-Requires: ((dash "2.17.0") (f "0.20.0") (s "1.12.0"))
;; Package-Version: 0.0.1

;;; Commentary:

;; Set or unset environment variables from an "env" or dotenv file.
;;
;; This package provides two interactive functions:
;;
;; 1.  `setenv-file`: Set all the environment variables defined in an env file.
;; 2.  `setenv-file-unset`: Unset all the environment variables defined in an
;;     env file.
;;
;; When used interactively, each function prompts for a file. By default, the
;; prompt begins at `setenv-file-dir`.
;;
;;
;; # Usage
;;
;; Start by creating an env file in `setenv-file-dir` (by default, `~/.env/`).
;; For example, create this file in `~/.env/foo`:
;;
;;     FOO=~/foo
;;     BAR=$FOO/bar
;;     ОФИС=ДОМ
;;     BAZ=nosubst:FOO$BAR
;;
;; Now, you can run:
;;
;; -   `M-x setenv-file`, which will prompt you for a file. All the environment
;;     variables defined in the file will be **set**.
;; -   `M-x setenv-file-unset`, which will prompt you for a file. All the
;;     environment variables defined in the file will be **unset**.
;;
;;
;; ## Usage from Elisp
;;
;; To set env variables defined in `~/.env/foo`:
;;
;;     (setenv-file (expand-file-name "~/.env/foo"))
;;
;; Or, if you have a string instead of a file:
;;
;;     (setenv-file-str "FOO=foo\nBAR=bar")
;;
;;
;; ## Usage from org-mode
;;
;; The example below shows a convenient way to declare and set environment
;; variables in an `org` document:
;;
;;     #+NAME: env
;;     | Var  | Value           |
;;     |------+-----------------|
;;     | FOO  | ~/foo           |
;;     | BAR  | $FOO/bar        |
;;     | ОФИС | ДОМ             |
;;     | BAZ  | nosubst:FOO$BAR |
;;
;;     #+begin_src emacs-lisp :var env=env
;;       (setenv-file-set-pairs env)
;;     #+end_src
;;
;;
;; # File Format
;;
;; Each line in the file should be in a `KEY=VALUE` format, with one entry per
;; line. This package does not invoke a shell to interpret the file, so most
;; shell-isms will not work. However, the env file may:
;;
;; -   Use existing environment variables
;; -   Define an environment variable and use it in successive lines
;; -   A `~` is expanded if it is the first character in the value
;; -   If a value starts with \`nosubst:\`, no variable substitution will be
;;     performed. You need this if there is a literal `$` in the value.
;;
;;; Code:

(require 'dash)
(require 'f)
(require 's)

;;; Options

(defgroup setenv-file nil
  "Source environment variable files in Emacs."
  :group 'environment
  :prefix "setenv-file-"
  :link '(url-link :tag "GitHub" "https://github.com/cfclrk/setenv-file"))

(defcustom setenv-file-dir (expand-file-name "~/")
  "Directory with env files."
  :group 'setenv-file
  :type 'file)

;;; Public

(defun setenv-file (file-path)
  "Set or unset environment variables from file FILE-PATH.

When used interactively, `setenv-file' prompts for the file
to load, defaulting to the directory `source-env-dir'.

The env file FILE-PATH may make use of existing environment
variables, and tildes are expanded if they are the first
character of the value. However, other shell-isms will not work.

Prefixed with one \\[universal-argument], unset the environment
variables defined in file F."
  (interactive (list (read-file-name "ENV file: " setenv-file-dir)))
  (let ((str (f-read-text file-path)))
    (if current-prefix-arg
        (setenv-file-unset-str str)
      (setenv-file-str str))))

(defun setenv-file-unset (file-path)
  "Unset environment variables from file FILE-PATH.

See the documentation for `setenv-file'."
  (interactive (list (read-file-name "ENV file: " setenv-file-dir)))
  (let ((str (f-read-text file-path)))
    (setenv-file-unset-str str)))

(defun setenv-file-str (str)
  "Set environment variables from string STR.

Parse STR as an env file. See the documentation for
`setenv-file'."
  (let* ((lines (s-lines (s-trim str)))
         (pairs (--map (s-split "=" it) lines)))
    (setenv-file-set-pairs pairs)))

(defun setenv-file-unset-str (str)
  "Unset environment variables from string STR.

Parse STR as an env file. See the documentation for
`setenv-file'."
  (let* ((lines (s-lines (s-trim str)))
         (pairs (--map (s-split "=" it) lines)))
    (setenv-file-unset-pairs pairs)))

(defun setenv-file-set-pairs (pairs)
  "Add PAIRS to `process-environment'.

PAIRS is a list of pairs, where each pair is an environment
variable name and value."
  (-each pairs #'setenv-file--export-pair))

(defun setenv-file-unset-pairs (pairs)
  "Remove PAIRS from `process-environment'.

PAIRS is a list of pairs, where each pair is an environment
variable name and value.

The key of each pair is the environment variable name. The value
of each pair is discarded, as the environment variable will be
unset regardless of its value."
  (setenv-file--unset-names (-map 'car pairs)))

;;; Private

(defun setenv-file--export-pair (pair)
  "Set an environment variable PAIR.
PAIR is a list of size 2, where first element is an environment
variable name and the second element is the value.

If the second element begins with a ~, it is treated as a file
path and expanded.

If the second element begins with nosubst:, it is treated as a
literal string, and no variable interpolation is performed."
  (let* ((name (car pair))
         (val (car (cdr pair)))

         ;; if the value of the pair is an number, convert it to a string
         (string_val (if (numberp val)
                         (number-to-string val)
                       val))

         ;; if the value starts with ~, expand it like a path
         (full_val (if (string-prefix-p "~" string_val)
                       (expand-file-name string_val)
                     string_val)))

    ;; if the value starts with "nosubst:", do not do variable interpolation
    (if (string-prefix-p "nosubst:" full_val)
        (setenv name (s-chop-prefix "nosubst:" full_val))
      (setenv name full_val t))))

(defun setenv-file--unset-names (names)
  "Remove NAMES from `process-environment'.
NAMES is a list of environment variable names which may or may
not be currently set. This function removes each given name from
`process-environment' if it is set."
  (-each names #'setenv-file--unset-name))

(defun setenv-file--unset-name (name)
  "Unset the environment variable NAME.
Unset environment variable NAME by removing it from
`process-environment' if it is there.

Note: calling `setenv' with a prefix argument sets the variable's
value to nil, but the variable is still present. This function
completely removes the variable from `process-environment'."
  (let* ((name (if (multibyte-string-p name)
                   (encode-coding-string name locale-coding-system t)
                 name))
         (index (-elem-index name (setenv-file--get-names))))
    (if index
        (setq process-environment (-remove-at index process-environment))
      process-environment)))

(defun setenv-file--get-names ()
  "Return names of all current environment variables."
  (--map (car (s-split "=" it)) process-environment))

(provide 'setenv-file)
;;; setenv-file.el ends here
