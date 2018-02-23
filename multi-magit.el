;;; multi-magit.el --- multi-repo support for Magit  -*- lexical-binding: t -*-

;; Author: Luis Oliveira <loliveira@common-lisp.net>
;; URL: https://github.com/luismbo/multi-magit
;; Package-Requires: ((magit "2.11"))
;; Keywords: git tools vc magit

;;; Commentary:

;; multi-magit, Multi-Repository Magit, is a set of Magit extensions for
;; handling several respositories simultaneously.

;;; Code:

(require 'magit)
(require 'magit-repos)
(require 'magit-status)
(require 'tabulated-list)
(require 'dash)
(require 'cl-lib)

(defgroup multi-magit nil
  "Controlling multiple repositories using Magit."
  :link '(url-link "https://github.com/luismbo/multi-magit")
  :group 'magit-extensions)

(defgroup magit-faces nil
  "Faces used by Multi-Magit."
  :group 'multi-magit)

(defvar multi-magit-status-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" 'multi-magit-status)
    map)
  "Keymap for `multi-magit-status-mode'.")

(define-derived-mode multi-magit-status-mode magit-status-mode "Multi-Magit"
  "A magit-status for multiple repositories."
  :group 'multi-magit)

(easy-menu-define multi-magit-status-mode-menu multi-magit-status-mode-map
  "Multi-Magit menu"
  '("Multi-Magit"
    ["Checkout"          multi-magit-checkout t]
    ["Git command"       multi-magit-git-command t]
    ["Shell command"     multi-magit-shell-command t]
    ["List repositories" multi-magit-list-repositories t]
    "---"
    ["Quit"    magit-mode-bury-buffer t]
    ["Refresh" multi-magit-status t]))

(defun multi-magit--find-current-section ()
  (save-excursion
    (while (and (or (null (magit-current-section))
                    (eq 'multi-magit-toplevel
                        (magit-section-type (magit-current-section))))
                (not (eql -1 (forward-line -1)))))
    (magit-current-section)))

(defun multi-magit--current-repo (&optional section)
  (setq section (or section (multi-magit--find-current-section)))
  (when section
    (let ((parent (magit-section-parent section)))
      (when parent
        (if (null (magit-section-parent parent))
            (magit-section-value section)
            (multi-magit--current-repo parent))))))

;;;###autoload
(defun multi-magit-insert-uncommitted-changes ()
  "Insert a diffstat of changes in worktree relative to HEAD."
  (magit-insert-section (diffstat)
    (magit-insert-heading "Uncommitted changes")
    (magit-git-wash #'magit-diff-wash-diffs
                    "diff" "HEAD" "--stat" "--numstat" "--no-prefix")
    (insert "\n")))

;;;###autoload
(defun multi-magit-insert-committed-changes ()
  "Insert a diffstat and commit log of commits since the
merge-base betweenn HEAD and @{upstream}."
  (let ((merge-base (magit-git-string "merge-base" "HEAD" "@{upstream}")))
    (when merge-base
      (magit-insert-section (diffstat)
        (magit-insert-heading "Committed changes")
        (magit-git-wash #'magit-diff-wash-diffs
                        "diff" merge-base "--stat" "--numstat" "--no-prefix")
        (insert "\n")
        (magit-insert-log (format "@{upstream}..") magit-log-section-arguments)))))

(defvar multi-magit-selected-repositories nil
  "The list of selected repositories that will be displayed by
`multi-magit-status'.")

(defun multi-magit-select-repository (&optional directory)
  "Select DIRECTORY's repository."
  (interactive)
  (let ((repo (magit-toplevel directory)))
    (if (null repo)
        (user-error "multi-magit: couldn't find a repository here.")
      (setq multi-magit-selected-repositories
            (cl-merge 'list
                      (list repo)
                      (cl-copy-list multi-magit-selected-repositories)
                      #'string<))
      (message "multi-magit: %s selected." repo))))

(defun multi-magit-unselect-repository (&optional directory)
  "Unselect DIRECTORY's repository."
  (interactive)
  (let ((repo (magit-toplevel directory)))
    (if (null repo)
        (user-error "multi-magit: couldn't find a repository here.")
      (setq multi-magit-selected-repositories
            (remove repo multi-magit-selected-repositories))
      (message "multi-magit: %s unselected." repo))))

;;;; Multi-repository Commands

(defvar multi-magit-record-process-setup nil)
(defvar multi-magit-pending-process-sections nil)

(defun multi-magit-process-buffer ()
  (get-buffer-create "*multi-magit-process"))

(defun multi-magit--after-magit-process-finish (arg &optional process-buf
                                                    _command-buf _default-dir
                                                    section)
  (unless (integerp arg)
    (setq process-buf (process-buffer arg))
    (setq section     (process-get arg 'section))
    (setq arg         (process-exit-status arg)))
  (when (and section (bufferp process-buf))
    (with-current-buffer (multi-magit-process-buffer)
      (let ((inhibit-read-only t)
            (target-section (cdr (assq section multi-magit-pending-process-sections))))
        (when target-section
          (let ((repo-name (with-current-buffer process-buf
                             (file-name-nondirectory
                              (directory-file-name default-directory)))))
            (setq multi-magit-pending-process-sections
                  (delq section multi-magit-pending-process-sections))
            (save-excursion
              (goto-char (magit-section-start target-section))
              ;; 1+ at the end because we've inserted an extra
              ;; newline in `multi-magit--around-magit-process-setup'.
              (delete-region (magit-section-start target-section)
                             (1+ (magit-section-end target-section)))
              (insert (propertize (concat "[" repo-name "]")
                                  'face (if (= arg 0)
                                            'magit-process-ok
                                          'magit-process-ng)))
              (insert-buffer-substring process-buf
                                       (magit-section-start section)
                                       (magit-section-end section)))))))))

(advice-add 'magit-process-finish :after
            #'multi-magit--after-magit-process-finish)

(defun multi-magit--around-magit-process-setup (original-function program args)
  (let ((result (funcall original-function program args)))
    (when multi-magit-record-process-setup
      (let ((section (cdr result)))
        (assert (magit-section-p section))
        (push (cons section
                    (with-current-buffer (multi-magit-process-buffer)
                      (let ((inhibit-read-only t))
                        (goto-char (point-max))
                        ;; `magit-process-insert-section' jumps to (1-
                        ;; (point-max)) for some reason and this gives
                        ;; us trouble. This is probably not the right
                        ;; fix.
                        (insert "\n"))
                      (prog1 (magit-process-insert-section
                              default-directory program args
                              nil nil)
                        (backward-char 1))))
              multi-magit-pending-process-sections)))
    result))

(advice-add 'magit-process-setup
            :around
            #'multi-magit--around-magit-process-setup)

(defun call-with-multi-magit-process (fn)
  (let ((buffer (multi-magit-process-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (setq default-directory temporary-file-directory) ; HACK
        (erase-buffer)
        (magit-process-mode)))
    (let ((multi-magit-record-process-setup t)
          (magit-process-popup-time -1))
      (funcall fn))
    (magit-display-buffer buffer)))

(defmacro with-multi-magit-process (&rest body)
  (declare (indent defun) (debug (body)))
  `(call-with-multi-magit-process (lambda () ,@body)))

(defun multi-magit-list-common-branches ()
  (-reduce '-intersection
           (--map (let ((default-directory it))
                    (magit-list-refs "refs/heads/" "%(refname:short)"))
                  multi-magit-selected-repositories)))

(defun multi-magit-checkout (branch)
  "Checkout BRANCH for each selected repository."
  (interactive (list (magit-completing-read
                      "Checkout"
                      (multi-magit-list-common-branches)
                      nil nil nil 'magit-revision-history)))
  (with-multi-magit-process
    (dolist (repo multi-magit-selected-repositories)
      (let ((default-directory repo)
            (inhibit-message t))
        (magit-checkout branch)))))

(defun multi-magit--shell-command (command)
  (with-multi-magit-process
    (dolist (repo multi-magit-selected-repositories)
      (let ((default-directory repo)
            (inhibit-message t)
            (process-environment process-environment))
        (with-current-buffer (magit-process-buffer t)
          (push "GIT_PAGER=cat" process-environment)
          (magit-start-process shell-file-name nil
                               shell-command-switch command))))))

(defun multi-magit-git-command (command)
  "Execute COMMAND asynchronously for each selected repository.

Interactively, prompt for COMMAND in the minibuffer. \"git \" is
used as initial input, but can be deleted to run another command.

COMMAND is run in the top-level directory of each repository."
  (interactive (list (read-shell-command "Shell command: "
                                         "git "
                                         'magit-git-command-history)))
  (multi-magit--shell-command command))

(defun multi-magit-shell-command (command)
  "Execute COMMAND asynchronously for each selected repository.

Interactively, prompt for COMMAND in the minibuffer. COMMAND is
run in the top-level directory of each repository."
  (interactive (list (read-shell-command "Shell command: "
                                         nil
                                         'magit-git-command-history)))
  (multi-magit--shell-command command))

;;;; Select/unselect Repositories

(defun multi-magit-repolist-column-status (_id)
  "Insert letters if there are uncommitted changes.

Show N if there is at least one untracked file.
Show U if there is at least one unstaged file.
Show S if there is at least one staged file."
  (concat (if (magit-untracked-files) "N" "")
          (if (magit-unstaged-files)  "U" "")
          (if (magit-staged-files)    "S" "")))

(defcustom multi-magit-repolist-columns
  '(("Name"   25 magit-repolist-column-ident ())
    ("Dirty"   5 multi-magit-repolist-column-status
     ((:right-align t)
      (:help-echo "N - untracked, U - unstaged, S - staged")))
    ("Branch" 25 magit-repolist-column-branch ())
    ("B>U"     3 magit-repolist-column-unpushed-to-upstream
     ((:right-align t)
      (:help-echo "Local changes not in upstream")))
    ("Path"   99 magit-repolist-column-path ()))
  "List of column displayed by `multi-magit-list-repositories'.

Each element has the form (HEADER WIDTH FORMAT PROPS).

HEADER is the string displayed in the header.  WIDTH is the width
of the column.  FORMAT is a function that is called with one
argument, the repository identification (usually its basename),
and with `default-directory' bound to the toplevel of its working
tree.  It has to return a string to be inserted or nil.  PROPS is
an alist that supports the keys `:right-align' and `:pad-right'.
Some entries also use `:help-echo', but `tabulated-list' does not
actually support that yet."
  :group 'multi-magit
  :type `(repeat (list :tag "Column"
                       (string   :tag "Header Label")
                       (integer  :tag "Column Width")
                       (function :tag "Inserter Function")
                       (repeat   :tag "Properties"
                                 (list (choice :tag "Property"
                                               (const :right-align)
                                               (const :pad-right)
                                               (symbol))
                                       (sexp   :tag "Value"))))))

(defun multi-magit-repolist-toggle-repository ()
  "Select or unselect DIRECTORY's repository."
  (interactive)
  (let* ((item (tabulated-list-get-id))
         (repo (when item (magit-toplevel item))))
    (cond ((null repo)
           (user-error "There is no repository at point"))
          ((member repo multi-magit-selected-repositories)
           (multi-magit-unselect-repository repo)
           (tabulated-list-put-tag " "))
          (t
           (multi-magit-select-repository repo)
           (tabulated-list-put-tag "*")))))

(defvar multi-magit-repolist-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "g" 'multi-magit-list-repositories)
    (define-key map (if (featurep 'jkl) [return] (kbd "C-m"))
      'multi-magit-repolist-toggle-repository)
    map)
  "Local keymap for Multi-Magit-Repolist mode buffers.")

(define-derived-mode multi-magit-repolist-mode magit-repolist-mode "Repos"
  "Major mode for managing the list of selected Git repositories."
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

;;;###autoload
(defun multi-magit-list-repositories ()
  "Display a list of repositories for selection.

Use the options `magit-repository-directories' and
`magit-repository-directories-depth' to control which
repositories are displayed."
  (interactive)
  (if magit-repository-directories
      (with-current-buffer (get-buffer-create "*Multi-Magit Repositories*")
        (let ((magit-repolist-columns multi-magit-repolist-columns))
          (multi-magit-repolist-mode))
        (setq tabulated-list-entries
              (mapcar (-lambda ((id . path))
                        (let ((default-directory path))
                          (list path
                                (vconcat (--map (or (funcall (nth 2 it) id) "")
                                                multi-magit-repolist-columns)))))
                      (magit-list-repos-uniquify
                       (--map (cons (file-name-nondirectory (directory-file-name it))
                                    it)
                              (magit-list-repos)))))
        (tabulated-list-print)
        (save-excursion
          (goto-char (point-min))
          (while (tabulated-list-get-id)
            (when (member (magit-toplevel (tabulated-list-get-id))
                          multi-magit-selected-repositories)
              (tabulated-list-put-tag "*"))
            (forward-line)))
        (switch-to-buffer (current-buffer)))
    (message "You need to customize `magit-repository-directories' %s"
             "before you can list repositories")))

;;;; Multi-Magit Status

(defcustom multi-magit-status-sections-hook
  '(magit-insert-untracked-files
    multi-magit-insert-uncommitted-changes
    multi-magit-insert-committed-changes)
  "Hook run to insert section into a `multi-magit-status' buffer."
  :group 'multi-magit
  :type 'hook)

(defface multi-magit-repo-heading
  '((((class color) (background light)) :inherit magit-section-heading :box t)
    (((class color) (background  dark)) :inherit magit-section-heading :box t))
  "Face for repository headings."
  :group 'multi-magit-faces)

(defvar multi-magit-repo-header-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'magit-status)
    map)
  "Keymap for repository headers.")

(defun multi-magit--insert-repo-heading (repo-name)
  (let* ((repo (propertize (format " %s " repo-name)
                           'face 'multi-magit-repo-heading))
         (branch (--if-let (magit-get-current-branch)
                     (propertize it 'face 'magit-branch-current)
                   (propertize "(detached)" 'face 'warning))))
    (magit-insert-heading (propertize (concat repo " " branch)
                                      'keymap
                                      multi-magit-repo-header-section-map))))

(defun multi-magit-status-refresh-buffer ()
  (multi-magit-status))

(defvar multi-magit-status-buffer-name "*Multi-Magit Status")

(defun multi-magit--around-magit-mode-get-buffers (original-function &rest args)
  (let ((buffers (apply original-function args))
        (multi-magit-status-buffer (get-buffer multi-magit-status-buffer-name)))
    (if (and multi-magit-status-buffer
             (member default-directory multi-magit-selected-repositories))
        (cons multi-magit-status-buffer buffers)
        buffers)))

(advice-add 'magit-mode-get-buffers
            :around
            'multi-magit--around-magit-mode-get-buffers)

(defun multi-magit--maybe-refresh ()
  (--when-let (and (member default-directory multi-magit-selected-repositories)
                   (get-buffer multi-magit-status-buffer-name))
    (multi-magit--refresh-status it)))

(add-hook 'magit-post-refresh-hook 'multi-magit--maybe-refresh)

(defun multi-magit--set-current-repo ()
  (let ((repo (or (multi-magit--current-repo)
                  (first multi-magit-selected-repositories))))
    (setq default-directory repo)
    (setq magit--default-directory repo)))

;; Similar to `magit-list-repos-uniquify' but the resulting alist maps
;; repo paths to repo names not the other way around.
(defun multi-magit--uniquify-repo-names (alist)
  (let ((result nil)
        (name->repos (make-hash-table :test 'equal)))
    (cl-loop for (name . repo) in alist
             do (puthash name
                         (cons repo (gethash name name->repos))
                         name->repos))
    (maphash
     (lambda (key value)
       (if (= (length value) 1)
           (push (cons (car value) key) result)
           (setq result
                 (append (multi-magit--uniquify-repo-names
                          (--map (cons (concat
                                        key "\\"
                                        (file-name-nondirectory
                                         (directory-file-name
                                          (substring it 0 (- (1+ (length key)))))))
                                       it)
                                 value))
                         result))))
     name->repos)
    result))

(defun multi-magit--selected-repo-names ()
  (let ((repo->name (multi-magit--uniquify-repo-names
                     (--map (cons (file-name-nondirectory (directory-file-name it)) it)
                            multi-magit-selected-repositories))))
    (--map (cdr (assoc it repo->name))
           multi-magit-selected-repositories)))

(defun multi-magit--refresh-status (buffer)
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (multi-magit--set-current-repo)
      (multi-magit-status-mode)
      (save-excursion
        (magit-insert-section (multi-magit-toplevel)
          (cl-loop for repo in multi-magit-selected-repositories
                   for repo-name in (multi-magit--selected-repo-names)
                   do (let ((default-directory repo)
                            (magit--default-directory repo))
                        (magit-insert-section (multi-magit-status repo)
                          (multi-magit--insert-repo-heading repo-name)
                          (insert "\n")
                          (magit-run-section-hook 'multi-magit-status-sections-hook))))))
      (add-hook 'post-command-hook 'multi-magit--set-current-repo nil :local))))

;;;###autoload
(defun multi-magit-status ()
  (interactive)
  (if (null multi-magit-selected-repositories)
      (when (y-or-n-p "`multi-magit-selected-repositories' is empty. Would you \
like to select some using `multi-magit-list-repositories'? ")
        (multi-magit-list-repositories))
    (let ((buffer (get-buffer-create multi-magit-status-buffer-name)))
      (multi-magit--refresh-status buffer)
      (magit-display-buffer buffer))))

;;;; Magit-status Sections

(defvar magit-multi-magit-repo-section-map
  (let ((map (make-sparse-keymap)))
    (unless (featurep 'jkl)
      (define-key map "\C-j"   'multi-magit-repo-visit))
    (define-key map [C-return] 'multi-magit-repo-visit)
    (define-key map [remap magit-visit-thing] 'multi-magit-repo-visit)
    map)
  "Keymap for `multi-magit-repo' sections.")

(defun multi-magit-repo-visit (repo &optional _other-window)
  "Visit REPO by calling `magit-status' on it."
  (interactive (list (magit-section-when multi-magit-repo)
                     current-prefix-arg))
  (when repo
    (magit-status-internal repo)))

;;;###autoload
(defun multi-magit-insert-repos-overview ()
  "Insert sections for all selected repositories."
  (when multi-magit-selected-repositories
    (let* ((repos multi-magit-selected-repositories)
           (repo-names (multi-magit--selected-repo-names))
           (path-format (format "%%-%is "
                                (min (apply 'max (mapcar 'length repo-names))
                                     (/ (window-width) 2))))
           (branch-format (format "%%-%is " (min 25 (/ (window-width) 3)))))
      (magit-insert-heading (format "%s (%d)"
                                    (propertize "Selected repositories"
                                                'face 'magit-section-heading)
                                    (length repos)))
      (cl-loop for repo in repos
               for repo-name in repo-names
               do (let ((default-directory repo))
                    (magit-insert-section (multi-magit-repo repo t)
                      (insert (propertize (format path-format repo-name)
                                          'face 'magit-diff-file-heading))
                      (insert (format branch-format
                                      (--if-let (magit-get-current-branch)
                                          (propertize it 'face 'magit-branch-local)
                                        (propertize "(detached)" 'face 'warning))))
                      (insert (mapconcat
                               'identity
                               (remove nil
                                       (list (--when-let (magit-untracked-files)
                                               (format "%d untracked" (length it)))
                                             (--when-let (magit-unstaged-files)
                                               (format "%d unstaged" (length it)))
                                             (--when-let (magit-staged-files)
                                               (format "%d staged" (length it)))))
                               ", "))
                      (insert "\n"))))))
  (insert "\n"))

;;;; Manage Branches

(define-derived-mode multi-magit-branches-mode magit-mode "Multi-Magit Branches"
  :group 'multi-magit
  (hack-dir-local-variables-non-file-buffer))

;; XXX: move me elsewhere
(defun multi-magit--all-repositories ()
  (magit-list-repos-uniquify
   (--map (cons (file-name-nondirectory (directory-file-name it))
                it)
          (magit-list-repos))))

(defun multi-magit--selected-repositories ()
  (magit-list-repos-uniquify
   (--map (cons (file-name-nondirectory (directory-file-name it))
                it)
          multi-magit-selected-repositories)))

;; TODO
;; 1. add option for listing all repositories or just the selected ones
;; 2. disable the magit-mode commands
;; 3. add delete command
;; 4. add select command

(defun multi-magit-insert-branches ()
  "Insert sections showing all local branches."
  (magit-insert-section (local nil)
    (magit-insert-heading "Branches:")
    (let ((branch->repos (make-hash-table :test 'equal))
          (branch->paths (make-hash-table :test 'equal)))
      (cl-loop for (repo . path) in (multi-magit--all-repositories)
               for default-directory = path
         do (-map (lambda (branch)
                    (push repo (gethash branch branch->repos))
                    (push path (gethash branch branch->paths)))
                  (magit-list-refs "refs/heads/" "%(refname:short)")))
      (maphash (lambda (branch repos)
                 (magit-insert-section it (branch branch t)
                   (insert (propertize (format "%-30s " branch)
                                       'face 'magit-branch-local
                                       'multi-magit-repos (gethash branch branch->paths)))
                   (insert (first repos))
                   (--map (insert (format ", %s" it)) (rest repos)))
                 (insert ?\n))
               branch->repos)))
  (insert ?\n)
  (magit-make-margin-overlay nil t))

(defcustom multi-magit-branches-sections-hook
  '(multi-magit-insert-branches)
  "Hook run to insert sections into a branches buffer."
  :group 'multi-magit
  :type 'hook)

(defun multi-magit-branches-refresh-buffer ()
  (setq magit-set-buffer-margin-refresh (not (magit-buffer-margin-p)))
  (magit-insert-section (branchbuf)
    (run-hooks 'multi-magit-branches-sections-hook)))

;; (magit-insert-error-header magit-insert-branch-description magit-insert-local-branches magit-insert-remote-branches magit-insert-tags)

(defun multi-magit-list-all-branches ()
  "List all branches in all repositories in
`magit-repository-directories' aggregating branches with common
names."
  (interactive)
  (magit-mode-setup #'multi-magit-branches-mode))

(provide 'multi-magit)
;;; multi-magit.el ends here
