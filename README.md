# multi-magit

A set of extensions to [Magit](https://magit.vc) for handling multiple
repositories simultaneously. This documentation will only make sense
if you're familiar with Magit.

## Browsing Multiple Repositories

### \[custom variable\] multi-magit-selected-repositories

This list determines which repositories the various multi-magit should
operate on. You can easily select and unselect repositories using
`multi-magit-list-repositories`.

### \[command\] multi-magit-list-repositories

Similar to `magit-list-repositories` but lets you select/unselect
repositories using `<kbd>RET</kbd>`.

The format of this listing is controlled via
`multi-magit-repolist-columns` custom variable, which has the same
format as `magit-repolist-columns`. Use `magit-repository-directories`
and `magit-repository-directories-depth` to control which repositories
will be listed.

### \[command\] multi-magit-status

Like `magit-status` but aggregates all of the
`multi-magit-selected-repositories`.

We recommend binding it globally to `<kbd>C-x G</kbd>`:

```elisp
(global-set-key (kbd "C-x G") 'multi-magit-status)
```

`multi-magit-status-sections-hook` determines which sections will be
inserted for each repo. It accepts the same sections as
`magit-status-sections-hook` but defaults to lightweight sections
focused on giving you a quick overview of each repository.

## Multi-repository Commands

### \[command\] multi-magit-checkout

Checkout a given branch on each of the selected repositories. Lists
suggestions based on branch names that are common across every
repository.

### \[command\] multi-magit-git-command

Execute a git command for each selected repository.

### \[command\] multi-magit-shell-command

Execute a shell command for each selected repository.

## Repository Overview for `magit-status`

### \[section\] multi-magit-insert-repos-overview

Add this to `magit-status-sections-hook` to include a one-line
overview for each selected repository showing the repository name, the
current branch and quick status showing an untracked/staged/unstaged
file count.

```elisp
(magit-add-section-hook 'magit-status-sections-hook
                        'multi-magit-insert-repos-overview
                         nil t)
```
