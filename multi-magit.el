;;; multi-magit.el --- multi-repo support for Magit  -*- lexical-binding: t -*-

;; Author: Luis Oliveira <loliveira@common-lisp.net>
;; URL: https://github.com/luismbo/multi-magit
;; Package-Requires: ((emacs "24.4") (magit "2.11"))
;; Keywords: git tools vc magit
;; Version: 0.1

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
    (define-key map "q" 'magit-mode-bury-buffer)
    map)
  "Keymap for `multi-magit-status-mode'.")

(define-derived-mode multi-magit-status-mode magit-status-mode "Multi-Magit"
  "A magit-status for multiple repositories."
  :group 'multi-magit)

(easy-menu-define multi-magit-status-mode-menu multi-magit-status-mode-map
  "Multi-Magit menu"
  '("Multi-Magit"
    ["Checkout"          multi-magit-checkout t]
    ["Delete branches"   multi-magit-branch-delete t]
    ["List branches"     multi-magit-list-branches t]
    "---"
    ["List repositories" multi-magit-list-repositories t]
    "---"
    ["Git command"       multi-magit-git-command t]
    ["Shell command"     multi-magit-shell-command t]
    "---"
    ["Quit"    kill-this-buffer t] ; see note in `multi-magit-status-mode-map'
    ["Refresh" multi-magit-status t]))

(defun multi-magit--find-current-section ()
  (save-excursion
    (while (and (or (null (magit-current-section))
                    (eq 'multi-magit-toplevel
                        (oref (magit-current-section) type)))
                (not (eql -1 (forward-line -1)))))
    (magit-current-section)))

(defun multi-magit--current-repo (&optional section)
  (setq section (or section (multi-magit--find-current-section)))
  (when section
    (let ((parent (oref section parent)))
      (when parent
        (if (null (oref parent parent))
            (oref section value)
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
                        "diff" merge-base "HEAD" "--stat" "--numstat" "--no-prefix")
        (insert "\n")
        (magit-insert-log (format "@{upstream}.."))))))

(defvar multi-magit-selected-repositories nil
  "The list of selected repositories that will be displayed by
`multi-magit-status'.")

(defun multi-magit--repo-name (repo)
  (file-name-nondirectory (directory-file-name repo)))

(defun multi-magit--all-repositories ()
  (magit-list-repos-uniquify
   (--map (cons (multi-magit--repo-name it) it)
          (magit-list-repos))))

(defun multi-magit--selected-repositories ()
  (magit-list-repos-uniquify
   (--map (cons (multi-magit--repo-name it)
                it)
          multi-magit-selected-repositories)))

;;;###autoload
(defun multi-magit-select-repository (&optional directory)
  "Select DIRECTORY's repository."
  (interactive)
  (let ((repo (magit-toplevel directory)))
    (if (null repo)
        (user-error "multi-magit: couldn't find a repository here.")
      (setq multi-magit-selected-repositories
            (cl-delete-duplicates
             (cl-merge 'list
                       (list repo)
                       (cl-copy-list multi-magit-selected-repositories)
                       #'string<)
             :test #'string=))
      (message "multi-magit: %s selected." repo))))

;;;###autoload
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
                             (multi-magit--repo-name default-directory))))
            (setq multi-magit-pending-process-sections
                  (delq section multi-magit-pending-process-sections))
            (save-excursion
              (goto-char (oref target-section start))
              ;; 1+ at the end because we've inserted an extra
              ;; newline in `multi-magit--around-magit-process-setup'.
              (delete-region (oref target-section start)
                             (1+ (oref target-section end)))
              (insert (propertize (concat "[" repo-name "]")
                                  'face (if (= arg 0)
                                            'magit-process-ok
                                          'magit-process-ng)))
              (insert-buffer-substring process-buf
                                       (oref section start)
                                       (oref section end)))))))))

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

(defun multi-magit--call-with-process (fn)
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

(defmacro multi-magit--with-process (&rest body)
  (declare (indent defun) (debug (body)))
  `(multi-magit--call-with-process (lambda () ,@body)))

(defun multi-magit-list-common-branches ()
  (-reduce '-intersection
           (--map (let ((default-directory it))
                    (magit-list-refs "refs/heads/" "%(refname:short)"))
                  multi-magit-selected-repositories)))

(defun multi-magit--read-branch (prompt)
  (magit-completing-read prompt
                         (multi-magit-list-common-branches)
                         nil nil nil
                         'magit-revision-history))

;;;###autoload
(defun multi-magit-checkout (branch)
  "Checkout BRANCH for each selected repository."
  (interactive (list (multi-magit--read-branch "Checkout")))
  (multi-magit--with-process
    (dolist (repo multi-magit-selected-repositories)
      (let ((default-directory repo)
            (inhibit-message t))
        (magit-checkout branch)))))

;;;###autoload
(defun multi-magit-branch-delete (branch)
  "Delete BRANCH for each selected repository."
  (interactive (list (multi-magit--read-branch "Delete")))
  (multi-magit--with-process
    (dolist (repo multi-magit-selected-repositories)
      (let ((default-directory repo)
            (inhibit-message t))
        (magit-branch-delete (list branch) t)))))

(defun multi-magit--shell-command (command)
  (multi-magit--with-process
    (dolist (repo multi-magit-selected-repositories)
      (let ((default-directory repo)
            (inhibit-message t)
            (process-environment process-environment))
        (with-current-buffer (magit-process-buffer t)
          (push "GIT_PAGER=cat" process-environment)
          (magit-start-process shell-file-name nil
                               shell-command-switch command))))))

;;;###autoload
(defun multi-magit-git-command (command)
  "Execute COMMAND asynchronously for each selected repository.

Interactively, prompt for COMMAND in the minibuffer. \"git \" is
used as initial input, but can be deleted to run another command.

COMMAND is run in the top-level directory of each repository."
  (interactive (list (read-shell-command "Shell command: "
                                         "git "
                                         'magit-git-command-history)))
  (multi-magit--shell-command command))

;;;###autoload
(defun multi-magit-shell-command (command)
  "Execute COMMAND asynchronously for each selected repository.

Interactively, prompt for COMMAND in the minibuffer. COMMAND is
run in the top-level directory of each repository."
  (interactive (list (read-shell-command "Shell command: "
                                         nil
                                         'magit-git-command-history)))
  (multi-magit--shell-command command))

;;;; Select/unselect Repositories

(defface multi-magit-repolist-repo-face
  '((((class color) (background light)) :inherit magit-branch-local)
    (((class color) (background  dark)) :inherit magit-branch-local))
  "Face for repository names in `multi-magit-list-repositories'."
  :group 'multi-magit-faces)

(defun multi-magit-repolist-column-status (_id)
  "Insert letters if there are uncommitted changes.

Show N if there is at least one untracked file.
Show U if there is at least one unstaged file.
Show S if there is at least one staged file."
  (concat (if (magit-untracked-files) "N" "")
          (if (magit-unstaged-files)  "U" "")
          (if (magit-staged-files)    "S" "")))

(defun multi-magit-repolist-column-repo (repo)
  "Insert the identification of the repository."
  (propertize repo 'face 'multi-magit-repolist-repo-face))

(defcustom multi-magit-repolist-columns
  '(("Name"   25 multi-magit-repolist-column-repo ())
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
                      (multi-magit--all-repositories)))
        (tabulated-list-print)
        (save-excursion
          (goto-char (point-min))
          (while (tabulated-list-get-id)
            (when (member (file-name-as-directory (tabulated-list-get-id))
                          multi-magit-selected-repositories)
              (tabulated-list-put-tag "*"))
            (forward-line)))
        (switch-to-buffer (current-buffer)))
    (message "You need to customize `magit-repository-directories' %s"
             "before you can list repositories")))

;;;; List Branches

(defvar multi-magit-branchlist-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "g" 'multi-magit-list-branches)
    (define-key map "q" 'kill-buffer)
    (define-key map (kbd "C-k")  'multi-magit-branchlist-delete)
    (define-key map (if (featurep 'jkl) [return] (kbd "C-m"))
      'multi-magit-branchlist-checkout)
    map)
  "Local keymap for Multi-Magit-Branchlist mode buffers.")

(easy-menu-define multi-magit-branchlist-mode-menu multi-magit-branchlist-mode-map
  "multi-magit-branchlist-mode menu."
  '("Multi-Magit Branches"
    ["Checkout" multi-magit-branchlist-checkout t]
    ["Delete"   multi-magit-branchlist-delete t]
    "---"
    ["Quit"     kill-buffer t]
    ["Refresh"  multi-magit-list-branches t]))

(defun multi-magit--human-readable-time-since (seconds)
  (let* ((seconds (truncate (float-time (time-since (seconds-to-time seconds)))))
         (minutes (truncate seconds 60)))
    (if (zerop minutes)
        (format "%ss" seconds)
      (let ((hours (truncate minutes 60)))
        (if (zerop hours)
            (format "%sm" minutes)
          (let ((days (truncate hours 24)))
            (if (zerop days)
                (format "%sh" hours)
              (let ((weeks (truncate days 7)))
                (if (zerop weeks)
                    (format "%sd" days)
                  ;; just an approximation
                  (let ((months (truncate days 30)))
                    (if (zerop months)
                        (format "%sw" weeks)
                      ;; ditto
                      (let ((years (truncate days 365)))
                        (if (zerop years)
                            (format "%smo" months)
                          (format "%sy" years))))))))))))))

(defun multi-magit--tabulated-list-printer (id cols)
  (let ((new-cols (vector (elt cols 0)
                          (multi-magit--human-readable-time-since (elt cols 1))
                          (elt cols 2))))
    (tabulated-list-print-entry id new-cols)))

(define-derived-mode multi-magit-branchlist-mode tabulated-list-mode "Branches"
  "Major mode for managing the list of selected Git repositories."
  (setq x-stretch-cursor        nil)
  (setq tabulated-list-padding  0)
  (setq tabulated-list-sort-key (cons "Branch" nil))
  (setq tabulated-list-format
        [("Branch" 30 t)
         ("Chg" 5 (lambda (e1 e2)
                    (> (elt (cl-second e1) 1)
                       (elt (cl-second e2) 1))))
         ("Repositories" 99 t)])
  (setq tabulated-list-printer 'multi-magit--tabulated-list-printer)
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun multi-magit-branchlist-checkout ()
  "Checkout branch at point in its respective repositories. Point
`multi-magit-selected-repositories' to these repositories."
  (interactive)
  (cl-destructuring-bind (&optional branch &rest repos)
      (tabulated-list-get-id)
    (when (null branch)
      (user-error "There is no branch at point"))
    (when (yes-or-no-p (format "Select %s and checkout `%s'? "
                               (mapconcat #'multi-magit--repo-name repos ", ")
                               branch))
      (setq multi-magit-selected-repositories
            (--map (file-name-as-directory it) repos))
      (multi-magit-checkout branch))))

(defun multi-magit-branchlist-delete ()
  "Delete branch at point in its respective repositories."
  (interactive)
  (cl-destructuring-bind (&optional branch &rest repos)
      (tabulated-list-get-id)
    (when (null branch)
      (user-error "There is no branch at point"))
    (let ((repo-names (mapconcat #'multi-magit--repo-name repos ", ")))
      (--when-let (--filter (let ((default-directory it))
                              (string= (magit-get-current-branch) branch))
                            repos)
        (user-error "Refusing to delete. `%s' is currently checked out in %s"
                    branch repo-names))
      (when (yes-or-no-p (format "Delete `%s' in %s? " branch repo-names))
        (tabulated-list-delete-entry)
        (let ((multi-magit-selected-repositories repos))
          (multi-magit-branch-delete branch))))))

(defun multi-magit--repo-branches+mtime (repo-path)
  ;; (magit-list-refs "refs/heads/" "%(refname:short)") would be the proper way
  ;; to do this, but it's comparatively very slow (at least on Windows),
  ;; particularly if we want to grab the modification time from the commit
  ;; metadata too. Also, note that we want to exclude secondary worktrees.
  (when (file-directory-p (expand-file-name ".git" repo-path))
    (cl-remove-duplicates ; earlier occurences are removed, i.e.,
                          ; .git/refs/heads takes precedence over
                          ; .git/packed-refs.
     (append (let ((packed-refs (expand-file-name ".git/packed-refs" repo-path)))
               ;; We're certainly asking for trouble parsing packed-refs. :-(
               (when (file-exists-p packed-refs)
                 (let* ((mtime (float-time (cl-sixth (file-attributes packed-refs))))
                        (lines (with-temp-buffer
                                 (insert-file-contents packed-refs)
                                 (split-string (buffer-string) "\n" t)))
                        (heads (--filter (when it (string-match-p "^refs/heads/" it))
                                         (--map (second (split-string it " " t))
                                                lines))))
                   (--map (list (file-name-nondirectory it) mtime) heads))))
             (--map (list (file-name-nondirectory it)
                          (float-time (cl-sixth (file-attributes it))))
                    (directory-files (expand-file-name ".git/refs/heads/" repo-path)
                                     t "[^.]")))
     :test #'string=
     :key #'cl-first)))

;;;###autoload
(defun multi-magit-list-branches ()
  "Display a list of branches in all repositories, selected or unselected.

Use the options `magit-repository-directories' and
`magit-repository-directories-depth' to control which
repositories are displayed."
  (interactive)
  (if magit-repository-directories
      (with-current-buffer (get-buffer-create "*Multi-Magit Branches*")
        (multi-magit-branchlist-mode)
        (let ((branch->info (make-hash-table :test 'equal)))
          (cl-loop for (repo . path) in (multi-magit--all-repositories)
                   for default-directory = path
                   do (-map (-lambda ((branch mtime))
                              (let ((info (or (gethash branch branch->info)
                                              (setf (gethash branch branch->info)
                                                    (list nil nil nil)))))
                                (push repo (cl-first info))
                                (push path (cl-second info))
                                (setf (cl-third info)
                                      (max (or (cl-third info) 0) mtime))))
                            (multi-magit--repo-branches+mtime path)))
          (setq tabulated-list-entries
                (cl-loop for branch being the hash-keys in branch->info
                         using (hash-value info)
                         collect (cl-destructuring-bind (repos paths last-changed) info
                                   (list (cons branch paths)
                                         (vector (propertize branch 'face 'magit-branch-local)
                                                 last-changed
                                                 (mapconcat 'identity repos ", ")))))))
        (tabulated-list-print)
        (switch-to-buffer (current-buffer)))
    (message "You need to customize `magit-repository-directories' %s"
             "before you can list branches.")))

;;;; Multi-Magit Status

(defcustom multi-magit-status-sections-hook
  '(magit-insert-untracked-files
    magit-insert-unstaged-changes
    magit-insert-staged-changes
    ;; multi-magit-insert-uncommitted-changes
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

(defcustom multi-magit-refresh-status-buffer t
  "Whether the multi-magit-status buffer is refreshed after running git.

When this variable and `magit-refresh-status-buffer' are both
non-nil, multi-magit's status buffer is automatically refreshed
after running git for side-effects on a selected repository."
  :group 'multi-magit
  :type 'boolean)

(defun multi-magit--maybe-refresh ()
  (--when-let (and magit-refresh-status-buffer
                   multi-magit-refresh-status-buffer
                   (member default-directory multi-magit-selected-repositories)
                   (get-buffer multi-magit-status-buffer-name))
    (multi-magit--refresh-status it)))

(add-hook 'magit-post-refresh-hook 'multi-magit--maybe-refresh)

(defun multi-magit--set-current-repo ()
  (let ((repo (or (multi-magit--current-repo)
                  (cl-first multi-magit-selected-repositories))))
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

(provide 'multi-magit)
;;; multi-magit.el ends here
