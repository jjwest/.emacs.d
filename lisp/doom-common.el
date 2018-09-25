;;; doom-common.el
(require 'f)
(require 's)
(require 'evil)
(require 'projectile)
(require 'all-the-icons)
(require 'dash)

(defmacro def-popup! (&rest params)
  `(push ',params shackle-rules))
(defmacro after! (feature &rest forms)
  "A smart wrapper around `with-eval-after-load', that supresses warnings
during compilation."
  (declare (indent defun) (debug t))
  `(,(if (or (not (boundp 'byte-compile-current-file))
             (not byte-compile-current-file)
             (if (symbolp feature)
                 (require feature nil :no-error)
               (load feature :no-message :no-error)))
         'progn
       (message "after: cannot find %s" feature)
       'with-no-warnings)
    (with-eval-after-load ',feature ,@forms)))

(defmacro add-hook! (hook &rest func-or-forms)
  "A convenience macro for `add-hook'.

HOOK can be one hook or a list of hooks. If the hook(s) are not quoted, -hook is
appended to them automatically. If they are quoted, they are used verbatim.

FUNC-OR-FORMS can be a quoted symbol, a list of quoted symbols, or forms. Forms will be
wrapped in a lambda. A list of symbols will expand into a series of add-hook calls.

Examples:
    (add-hook! 'some-mode-hook 'enable-something)
    (add-hook! some-mode '(enable-something and-another))
    (add-hook! '(one-mode-hook second-mode-hook) 'enable-something)
    (add-hook! (one-mode second-mode) 'enable-something)
    (add-hook! (one-mode second-mode) (setq v 5) (setq a 2))"
  (declare (indent defun) (debug t))
  (unless func-or-forms
    (error "add-hook!: FUNC-OR-FORMS is empty"))
  (let* ((val (car func-or-forms))
         (quoted (eq (car-safe hook) 'quote))
         (hook (if quoted (cadr hook) hook))
         (funcs (if (eq (car-safe val) 'quote)
                    (if (cdr-safe (cadr val))
                        (cadr val)
                      (list (cadr val)))
                  (list func-or-forms)))
         (forms '()))
    (mapc
     (lambda (f)
       (let ((func (cond ((symbolp f) `(quote ,f))
                         (t `(lambda (&rest _) ,@func-or-forms)))))
         (mapc
          (lambda (h)
            (push `(add-hook ',(if quoted h (intern (format "%s-hook" h))) ,func) forms))
          (-list hook)))) funcs)
    `(progn ,@forms)))

(defvar doom--transient-counter 0)

(defmacro add-transient-hook! (hook &rest forms)
  "Attaches transient forms to a HOOK.

HOOK can be a quoted hook or a sharp-quoted function (which will be advised).

These forms will be evaluated once when that function/hook is first invoked,
then it detaches itself."
  (declare (indent 1))
  (let ((append (eq (car forms) :after))
        (fn (intern (format "doom-transient-hook-%s" (cl-incf doom--transient-counter)))))
    `(when ,hook
       (fset ',fn
             (lambda (&rest _)
               ,@forms
               (cond ((functionp ,hook) (advice-remove ,hook #',fn))
                     ((symbolp ,hook)   (remove-hook ,hook #',fn)))
               (unintern ',fn nil)))
       (cond ((functionp ,hook)
              (advice-add ,hook ,(if append :after :before) #',fn))
             ((symbolp ,hook)
              (add-hook ,hook #',fn ,append))))))



(defvar doom-disabled-packages ()
  "A list of packages that should be ignored by `def-package!'.")

(defmacro def-package! (name &rest plist)
  "A thin wrapper around `use-package'."
  ;; Ignore package if NAME is in `doom-disabled-packages'
  (when (and (memq name doom-disabled-packages)
             (not (memq :disabled plist)))
    (setq plist `(:disabled t ,@plist)))
  ;; If byte-compiling, ignore this package if it doesn't meet the condition.
  ;; This avoids false-positive load errors.
  (unless (and (bound-and-true-p byte-compile-current-file)
               (or (and (plist-member plist :if)     (not (eval (plist-get plist :if))))
                   (and (plist-member plist :when)   (not (eval (plist-get plist :when))))
                   (and (plist-member plist :unless) (eval (plist-get plist :unless)))))
    `(use-package ,name ,@plist)))


;;;###autoload
(defmacro without-project-cache! (&rest body)
  "Run BODY with projectile's project-root cache disabled. This is necessary if
you want to interactive with a project other than the one you're in."
  `(let ((projectile-project-root-cache (make-hash-table :test 'equal))
         projectile-project-name
         projectile-require-project-root)
     ,@body))

;;;###autoload
(defmacro project-file-exists-p! (files)
  "Checks if the project has the specified FILES.
Paths are relative to the project root, unless they start with ./ or ../ (in
which case they're relative to `default-directory'). If they start with a slash,
they are absolute."
  `(file-exists-p! ,files (doom-project-root)))


;;
;; Commands

;;;###autoload
(defun doom/reload-project ()
  "Reload the project root cache."
  (interactive)
  (projectile-invalidate-cache nil)
  (projectile-reset-cached-project-root)
  (dolist (fn projectile-project-root-files-functions)
    (remhash (format "%s-%s" fn default-directory) projectile-project-root-cache)))


;;
;; Library

;;;###autoload
(defun doom-project-p (&optional nocache)
  "Return t if this buffer is currently in a project.
If NOCACHE, don't fetch a cached answer."
  (if nocache
      (without-project-cache! (doom-project-p nil))
    (let ((projectile-require-project-root t))
      (and (projectile-project-p) t))))

;;;###autoload
(defun doom-project-name (&optional nocache)
  "Return the name of the current project.
If NOCACHE, don't fetch a cached answer."
  (if nocache
      (without-project-cache! (doom-project-name nil))
    (projectile-project-name)))

;;;###autoload
(defun doom-project-root (&optional nocache)
  "Returns the root of your project, or `default-directory' if none was found.
If NOCACHE, don't fetch a cached answer."
  (if nocache
      (without-project-cache! (doom-project-root nil))
    (let (projectile-require-project-root)
      ;; NOTE `projectile-project-root' should return default-directory if we're
      ;; not in a project. Seems to be a bug upstream.
      (or (projectile-project-root)
          default-directory))))

;;;###autoload
(defalias 'doom-project-expand #'projectile-expand-root)

;;;###autoload
(defun doom-project-find-file (dir)
  "Fuzzy-find a file under DIR."
  (without-project-cache!
   (let* ((default-directory (file-truename dir))
          (projectile-project-root default-directory))
     (call-interactively
      ;; completion modules may remap this command
      (or (command-remapping #'projectile-find-file)
          #'projectile-find-file)))))

;;;###autoload
(defun doom-project-browse (dir)
  "Traverse a file structure starting linearly from DIR."
  (let ((default-directory (file-truename dir)))
    (call-interactively
     ;; completion modules may remap this command
     (or (command-remapping #'find-file)
         #'find-file))))

(provide 'doom-projects)


(provide 'doom-common)
;;; doom-common.el ends here
