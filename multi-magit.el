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

(eval-when-compile
  (require 'cl))

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
    ["Quit" magit-mode-bury-buffer t]
    ["Refresh" multi-magit-status t]))

(defun multi-magit--find-current-section ()
  (save-excursion
    (while (and (or (null (magit-current-section))
                    (eq 'toplevel (magit-section-type (magit-current-section))))
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
      (add-to-list 'multi-magit-selected-repositories repo t)
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

(defface multi-magit-detached-head
    '((((class color) (background light))
       :inherit magit-branch-current :foreground "firebrick")
      (((class color) (background  dark))
       :inherit magit-branch-current :foreground "tomato"))
  "Face for denoting a detached head."
  :group 'multi-magit-faces)

(defvar multi-magit-repo-header-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'magit-status)
    map)
  "Keymap for repository headers.")

(defun multi-magit--insert-repo-heading (repo-name)
  (let* ((repo-string (propertize (format " %s " repo-name)
                                  'face 'multi-magit-repo-heading))
         (current-branch (magit-get-current-branch))
         (branch-string (propertize (or current-branch "detached HEAD")
                                    'face (if current-branch
                                              'magit-branch-current
                                              'multi-magit-detached-head))))
    (magit-insert-heading (propertize (concat repo-string " " branch-string)
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

;;;###autoload
(defun multi-magit-status ()
  (interactive)
  (if (null multi-magit-selected-repositories)
      (user-error "multi-magit: no selected repositories. Select some using `multi-magit-select-repository'.")
    (let ((buffer (get-buffer-create multi-magit-status-buffer-name))
          (inhibit-read-only t))
      (with-current-buffer buffer
        (erase-buffer)
        (multi-magit--set-current-repo)
        (multi-magit-status-mode))
      (magit-display-buffer buffer)
      (with-current-buffer buffer
        (save-excursion
          (magit-insert-section (toplevel)
            (cl-loop for repo in multi-magit-selected-repositories
                     for repo-name in (multi-magit--selected-repo-names)
                     do (let ((default-directory repo)
                              (magit--default-directory repo))
                          (magit-insert-section (status repo)
                            (multi-magit--insert-repo-heading repo-name)
                            (insert "\n")
                            (magit-run-section-hook 'multi-magit-status-sections-hook))))))
        (add-hook 'post-command-hook 'multi-magit--set-current-repo nil :local)))))

(provide 'multi-magit)
;;; multi-magit.el ends here
