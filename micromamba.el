;;; micromamba.el --- A library for working with micromamba environments -*- lexical-binding: t -*-

;; Copyright (C) 2023 Korytov Pavel

;; Author: Korytov Pavel <thexcloud@gmail.com>
;; Maintainer: Korytov Pavel <thexcloud@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (pythonic "0.1.0"))
;; Homepage: https://github.com/SqrtMinusOne/micromamba.el
;; Published-At: 2023-06-20

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; mamba is a reimplementation of the conda package manager in C++.
;; mamba is notably much faster and essentially compatible with conda,
;; so it also works with conda.el.  micromamba, however, implements
;; only a subset of mamba commands, and as such requires a separate
;; integration.
;;
;; The package has two entrypoints:
;; - `micromamba-activate' - activate the environment
;; - `micromamba-deactivate' - deactivate the environment
;;
;; Also see the README at
;; <https://github.com/SqrtMinusOne/micromamba.el> for more
;; information.

;;; Code:
(require 'json)
(require 'pythonic)
(require 'seq)

(defgroup micromamba nil
  "Micromamba (environment manager) integration for Emacs."
  :group 'python)

(defcustom micromamba-home (expand-file-name "~/.micromamba") ;; adapted from conda.el
  "The directory where micromamba stores its files."
  :type 'directory
  :group 'micromamba)

(defcustom micromamba-executable (executable-find "micromamba")
  "Path to micromamba executable."
  :type 'string
  :group 'micromamba)

(defcustom micromamba-message-on-environment-switch t ;;adapted from conda.el
  "Whether to message when switching environments.  Default true."
  :type 'boolean
  :group 'micromamba)

(defcustom micromamba-activate-base-by-default nil ;;adapted from conda.el
  "Whether to activate the base environment by default if no other is preferred.
Default nil."
  :type 'boolean
(defcustom micromamba-fallback-environment nil
  "An environment that micromamba.el activates by default."
  :type 'string
  :group 'micromamba)

(defcustom micromamba-preactivate-hook nil
  "Hook run before a micromamba environment is activated."
  :type 'hook
  :group 'micromamba)

(defcustom micromamba-postactivate-hook nil
  "Hook run after a micromamba environment is activated."
  :type 'hook
  :group 'micromamba)

(defcustom micromamba-predeactivate-hook nil
  "Hook run before a micromamba environment is deactivated."
  :type 'hook
  :group 'micromamba)

(defcustom micromamba-postdeactivate-hook nil
  "Hook run after a micromamba environment is deactivated."
  :type 'hook
  :group 'micromamba)

(defvar micromamba-env-current-prefix nil
  "Current activated micromamba environment.")

(defvar eshell-path-env)

;; internal variables that you probably shouldn't mess with

(defvar micromamba-env-executables-dir  ;; copied from virtualenv.el b/w/o conda.el
  (if (eq system-type 'windows-nt) "Scripts" "bin")
  "Name of the directory containing executables.  It is system dependent.")

(defvar micromamba-env-meta-dir "conda-meta" ;; copied from conda.el
  "Name of the directory containing metadata.
This should be consistent across platforms.")

;; internal utility functions
(defun micromamba--call-json (&rest args)
  "Call micromamba and parse the return value as JSON.

Pass ARGS as arguments to the program."
  (unless micromamba-executable
    (user-error "Micromamba-executable is not set!"))
  (with-temp-buffer
    (apply #'call-process micromamba-executable nil t nil args)
    (goto-char (point-min))
    (json-read)))

(defvar micromamba--config nil
  "Cached copy of configuration that Micromamba sees (including `condarc', etc).
Set for the lifetime of the process.")

(defun micromamba--get-config()
  "Return current Conda configuration.  Cached for the lifetime of the process."
  (if (not (eq micromamba--config nil))
      micromamba--config
    (let ((cfg (micromamba--call-json "config" "list" "--json")))
      (setq micromamba--config cfg))))

(defun micromamba-envs ()
  "Get micromamba environments.

Returns an alist with environments, where the key is the name and the
value is the prefix.  Duplicate names are replaced with prefixes."
  (let* ((envs
          (append (alist-get 'envs (micromamba--call-json "env" "list" "--json")) nil))
         (base-env (alist-get 'base\ environment
                              (micromamba--call-json "info" "--json")))
         ;; I have some environments from conda with the same name :-(
         (dupe-name-map (make-hash-table :test 'equal))
         (env-names
          (cl-loop for env in envs
                   for env-name = (if (eq base-env env-name) "base"
                                    (file-name-nondirectory env))
                   do (if (gethash env-name dupe-name-map)
                          (puthash env-name 'dupe dupe-name-map)
                        (puthash env-name t dupe-name-map))
                   collect env-name)))
    (cl-loop for env in envs
             for env-name in env-names
             for name = (if (eq (gethash env-name dupe-name-map) 'dupe)
                            env env-name)
             collect (cons name env))))

(defun micromamba--parse-script-buffer ()
  "Parse bash script buffer generated by micromamba.

E.g. micromamba shell -s bash activate <prefix>.

Returns an alist with the following keys:
- path
- vars-unset
- vars-export
- scripts."
  (let (path vars-unset vars-export scripts)
    (while (not (eobp))
      (cond
       ((looking-at (rx bol "export PATH='"))
        (setq path
              (split-string (buffer-substring-no-properties
                             (+ 13 (point)) (1- (point-at-eol)))
                            ":")))
       ((looking-at (rx bol "unset"))
        (push
         (buffer-substring-no-properties (+ 6 (point)) (point-at-eol))
         vars-unset) )
       ((looking-at (rx bol "export"))
        (save-excursion
          (let ((var-point (+ 7 (point))))
            (re-search-forward (rx "="))
            (let ((var-name (buffer-substring-no-properties var-point (1- (point))))
                  (var-contents (buffer-substring-no-properties
                                 (point) (point-at-eol))))
              (when (string-match-p (rx bos "'" (* nonl) "'" eos) var-contents)
                (setq var-contents
                      (substring var-contents 1 (1- (length var-contents)))))
              (push (cons var-name var-contents) vars-export)))))
       ((looking-at (rx bol ". \""))
        (push (buffer-substring-no-properties (+ 4 (point))
                                              (1- (point-at-eol)))
              scripts)))
      (forward-line))
    `((path . ,path)
      (vars-unset . ,vars-unset)
      (vars-export . ,vars-export)
      (scripts . ,scripts))))

(defun micromamba--env-dir-is-valid (candidate)
  "Confirm that CANDIDATE is a valid conda environment."
  (let ((dir (file-name-as-directory candidate)))
    (and (not (s-blank? candidate))
         (f-directory? dir)
         (or (f-directory? (concat dir micromamba-env-executables-dir))
             (f-directory? (concat dir micromamba-env-meta-dir))))))

(defun micromamba--contains-env-yml? (candidate) ;; adapted from conda.el
  "Does CANDIDATE contain an environment.yml?"
  (f-exists? (f-expand "environment.yml" candidate)))

(defun micromamba--find-env-yml (dir) ;; adapted from conda.el
  "Find an environment.yml in DIR or its parent directories."
  ;; TODO: implement an optimized finder with e.g. projectile? Or a series of
  ;; finder functions, that stop at the project root when traversing
  (let ((containing-path (f-traverse-upwards 'micromamba--contains-env-yml? dir)))
    (when containing-path
        (f-expand "environment.yml" containing-path))))

(defun micromamba--get-name-from-env-yml (filename) ;; adapted from conda.el
  "Pull the `name` property out of the YAML file at FILENAME."
  ;; TODO: find a better way than slurping it in and using a regex...
  (when filename
    (let ((env-yml-contents (f-read-text filename)))
      (when (string-match "name:[ ]*\\([A-z0-9-_.]+\\)[ ]*$" env-yml-contents)
          (match-string 1 env-yml-contents)))))

(defun micromamba--infer-env-from-buffer () ;; adapted from conda.el
  "Search up the project tree for an `environment.yml` defining a conda env."
  (let* ((filename (buffer-file-name))
         (working-dir (if filename
                          (f-dirname filename)
                        default-directory)))
    (when working-dir
      (or
       (micromamba--get-name-from-env-yml (micromamba--find-env-yml working-dir))
       micromamba-fallback-environment))))

(defun micromamba--get-activation-parameters (prefix)
  "Get activation parameters for the environment PREFIX.

The parameters value is an alist as defined by
`micromamba--parse-script-buffer'."
  (with-temp-buffer
    (call-process micromamba-executable nil t nil
                  "shell" "activate" prefix "-s" "bash")
    (goto-char (point-min))
    (micromamba--parse-script-buffer)))

(defun micromamba--get-deactivation-parameters ()
  "Get deactivation parameters for the current evironment.

The parameters value is an alist as defined by
`micromamba--parse-script-buffer'."
  (with-temp-buffer
    (call-process micromamba-executable nil t nil
                  "shell" "deactivate" "-s" "bash")
    (goto-char (point-min))
    (micromamba--parse-script-buffer)))

(defun micromamba--apply-env (parameters)
  "Apply PARAMETERS to the current environment.

The parameters value is an alist as defined by
`micromamba--parse-script-buffer'."
  (unless (alist-get 'path parameters)
    (user-error "Something went wrong.  Cannot get PATH"))
  (setq exec-path (alist-get 'path parameters))
  (setenv "PATH" (string-join (alist-get 'path parameters) ":"))
  (dolist (var-name (alist-get 'vars-unset parameters))
    (setenv var-name nil))
  (dolist (var (alist-get 'vars-export parameters))
    (setenv (car var) (cdr var)))
  (setq eshell-path-env (getenv "PATH")))

;; "public" functions

(defun micromamba-env-default-location ()
  "Default location of the conda environments -- under the Anaconda installation."
  (let ((candidates (alist-get 'envs_dirs (micromamba--get-config))))
    (f-full (aref candidates 0))))


(defun micromamba-env-name-to-dir (name)
  "Translate NAME to the directory where the environment is located."
  (if (and (string= name "base")
           (micromamba--env-dir-is-valid micromamba-home))
      (file-name-as-directory (expand-file-name micromamba-home))
    (let* ((default-location (file-name-as-directory (micromamba-env-default-location)))
           (initial-possibilities (list name (concat default-location name)))
           (possibilities (if (boundp 'venv-location)
                              (if (stringp venv-location)
                                  (cons venv-location initial-possibilities)
                                (nconc venv-location initial-possibilities))
                            initial-possibilities))
           (matches (-filter 'micromamba--env-dir-is-valid possibilities)))
      (if (> (length matches) 0)
          (file-name-as-directory (expand-file-name (car matches)))
        (error "No such conda environment: %s" name)))))

;;;###autoload
(defun micromamba-activate (prefix)
  "Switch to environment with PREFIX (path).  Prompt if called interactively.

If some environments have duplicate names, these names are replaced by
full paths."
  (interactive
   (list (let ((envs (micromamba-envs)))
           (alist-get
            (completing-read "Choose a micromamba environment: " envs
                             nil t)
            envs nil nil #'equal))))
  ;; To allow calling the function with env name as well
  (unless (string-match-p (rx bos "/") prefix)
    (let ((envs (micromamba-envs)))
      (setq prefix (alist-get prefix envs nil nil #'equal)))
    (unless prefix
      (user-error "Environment %s not found" prefix)))
  (micromamba-deactivate)
  (setq micromamba-env-current-prefix prefix)
  (run-hooks 'micromamba-preactivate-hook)
  ;; conda.el was doing that, so why not?
  (pythonic-activate prefix)
  (setq python-shell-virtualenv-root prefix)
  (micromamba--apply-env
   (micromamba--get-activation-parameters prefix))
  (run-hooks 'micromamba-postactivate-hook)
  (message "Switched to micromamba environment: %s" prefix))

;;;###autoload
(defun micromamba-deactivate ()
  "Deactivate the current environment."
  (interactive)
  (when (bound-and-true-p micromamba-env-current-prefix)
    (run-hooks 'micromamba-predeactivate-hook)
    (setq python-shell-virtualenv-root nil)
    (micromamba--apply-env
     (micromamba--get-deactivation-parameters))
    (setq micromamba-env-current-prefix nil)
    (run-hooks 'micromamba-postdeactivate-hook)))

;;;###autoload
(defun micromamba-env-activate-for-buffer ()
  "Activate the conda environment implied by the current buffer.

This can be set by a buffer-local or project-local variable (e.g. a
`.dir-locals.el` that defines `conda-project-env-path`), or inferred from an
`environment.yml` or similar at the project level."
  (interactive)
  (let ((inferred-env (micromamba--infer-env-from-buffer)))
    (when inferred-env
    (micromamba-activate inferred-env))))

(defun micromamba--switch-buffer-auto-activate (&rest args)
  "Add Conda environment activation if a buffer has a file, handling ARGS."
  (let ((filename (buffer-file-name)))
    (when filename
      ;; (message "switch-buffer auto-activating on <%s>" filename)
      (with-demoted-errors "Error: %S"
        (micromamba-env-activate-for-buffer)))))

;;;###autoload
(define-minor-mode micromamba-env-autoactivate-mode
  "Toggle conda-env-autoactivate mode.

This mode automatically tries to activate a conda environment for the current
buffer."
  ;; The initial value.
  :init-value nil
  ;; The indicator for the mode line.
  :lighter nil
  ;; The minor mode bindings.
  :keymap nil
  ;; Kwargs
  :group 'micromamba
  :global t
  ;; Forms
  (if micromamba-env-autoactivate-mode ;; already on, now switching off
    (add-to-list 'window-selection-change-functions #'micromamba--switch-buffer-auto-activate)
    (delete #'micromamba--switch-buffer-auto-activate window-selection-change-functions)))

(provide 'micromamba)
;;; micromamba.el ends here
