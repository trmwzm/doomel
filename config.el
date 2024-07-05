;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;;;; General and Global
(global-set-key (kbd "M-p")  'move-line-up)
(global-set-key (kbd "M-n")  'move-line-down)
;;;;; Adjust Fonts
(global-set-key (kbd "C-M-=") 'doom/increase-font-size)
(global-set-key (kbd "C-M-<mouse-4>") 'doom/increase-font-size)
(global-set-key (kbd "C-M-<mouse-5>") 'doom/decrease-font-size)
(global-set-key (kbd "C-M--") 'doom/decrease-font-size)

(defconst *sys/gui* (display-graphic-p))
(defconst *sys/is-mac* (eq system-type 'darwin))
(defconst *sys/is-linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)))
(defconst *sys/is-unix* (or *sys/is-linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix)))
(defconst *sys/is-windows* (or (eq system-type 'ms-dos) (eq system-type 'windows-nt)))
(defconst *sys/is-cygwin* (eq system-type 'cygwin))
(defconst *sys/is-wsl* (and *sys/is-linux* (getenv "WSLENV")))
(defconst *sys/is-not-wsl-linux* (and *sys/is-linux* (not *sys/is-wsl*)))

(when *sys/is-wsl*
  (setq browse-url-generic-program  "/mnt/c/Windows/System32/cmd.exe"
        browse-url-generic-args     '("/c" "start")
        browse-url-browser-function #'browse-url-generic))


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Thierry Michel"
      user-mail-address "trm@jpl.nasa.gov")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(when *sys/is-not-wsl-linux*
    ;; (setq doom-font (font-spec :family "monospace" :size 11 :weight 'semi-light)
    ;;     doom-variable-pitch-font (font-spec :family "sans" :size 11))
    (setq doom-font (font-spec :family "Fira Code Nerd Font" :size 14 )
        doom-variable-pitch-font (font-spec :family "sans" :size 11)) )
(when *sys/is-wsl*
  (setq doom-font (font-spec :family "DejaVu Sans Mono" :size 12)
        doom-variable-pitch-font (font-spec :family "sans" :size 12)))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/env/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; TRM start
(when *sys/is-wsl*
  ;; wsl-copy
  (defun wsl-copy (start end)
    (interactive "r")
    (shell-command-on-region start end "clip.exe")
    (deactivate-mark))

  (global-set-key
   (kbd "C-c f c")
   'wsl-copy)

  ;; wsl-paste
  (defun wsl-paste ()
    (interactive)
    (let ((clipboard
           (shell-command-to-string "pwsh.exe -command 'Get-Clipboard' 2> /dev/null")))
      (setq clipboard (replace-regexp-in-string "\r" "" clipboard)) ; Remove Windows ^M characters
      (setq clipboard (substring clipboard 0 -1)) ; Remove newline added by Powershell
      (insert clipboard)))

  (global-set-key
   (kbd "C-c f v")
   'wsl-paste))

(use-package! dired-subtree
  :after dired
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-toggle)))

(load! "lisp/yorick.el" doom-private-dir)
(load! "lisp/yorick-auto.el" doom-private-dir)
(load! "lisp/setenv-file.el" doom-private-dir)
(setq setenv-file-dir (expand-file-name "~/.env/"))
(load! "lisp/chezmoi.el")

(use-package! vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Setup yorick
;;(use-package yorick)
;;(load "yorick-auto")

(defun trm/ediff-doom-config (file)
  "ediff the current config with the examples in doom-emacs-dir

There are multiple config files, so FILE specifies which one to
diff.
"
  (interactive
    (list (read-file-name "Config file to diff: " doom-private-dir)))
  (let* ((stem (file-name-base file))
          (customized-file (format "%s.el" stem))
          (template-file-regex (format "^%s.example.el$" stem)))
    (ediff-files
      (concat doom-private-dir customized-file)
      (car (directory-files-recursively
             doom-emacs-dir
             template-file-regex
             nil
             (lambda (d) (not (string-prefix-p "." (file-name-nondirectory d)))))))))

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(add-to-list 'auto-mode-alist '("\\.[fF]95$"                . f90-mode))            ;; Use f90 mode with fortran 1995
(add-to-list 'auto-mode-alist '("\\.[fF]0[38]$"             . f90-mode))            ;; Use f90 mode with fortran 2003 and 2008
(add-to-list 'auto-mode-alist '("\\.[mM][oO][dD]$"          . f90-mode))            ;; Use f90 mode with fortran modules
(add-to-list 'auto-mode-alist '("\\.[fF]200[38]$"           . f90-mode))            ;; Use f90 mode with fortran 2003 and 2008
(add-to-list 'auto-mode-alist '("\\.fypp$"                  . f90-mode))            ;; Use f90 mode with fortran 2003 and 2008
(add-to-list 'auto-mode-alist '("\\.[fF]77$"                . fortran-mode))        ;; Use fortran mode for f77
(add-to-list 'auto-mode-alist '("\\.[fF][oO][rR]$"          . fortran-mode))        ;; Use fortran mode for f77

;;
;; TRM end

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
