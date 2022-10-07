# org-github-issues

[![Sponsored](https://img.shields.io/badge/chilicorn-sponsored-brightgreen.svg?logo=data%3Aimage%2Fpng%3Bbase64%2CiVBORw0KGgoAAAANSUhEUgAAAA4AAAAPCAMAAADjyg5GAAABqlBMVEUAAAAzmTM3pEn%2FSTGhVSY4ZD43STdOXk5lSGAyhz41iz8xkz2HUCWFFhTFFRUzZDvbIB00Zzoyfj9zlHY0ZzmMfY0ydT0zjj92l3qjeR3dNSkoZp4ykEAzjT8ylUBlgj0yiT0ymECkwKjWqAyjuqcghpUykD%2BUQCKoQyAHb%2BgylkAyl0EynkEzmkA0mUA3mj86oUg7oUo8n0k%2FS%2Bw%2Fo0xBnE5BpU9Br0ZKo1ZLmFZOjEhesGljuzllqW50tH14aS14qm17mX9%2Bx4GAgUCEx02JySqOvpSXvI%2BYvp2orqmpzeGrQh%2Bsr6yssa2ttK6v0bKxMBy01bm4zLu5yry7yb29x77BzMPCxsLEzMXFxsXGx8fI3PLJ08vKysrKy8rL2s3MzczOH8LR0dHW19bX19fZ2dna2trc3Nzd3d3d3t3f39%2FgtZTg4ODi4uLj4%2BPlGxLl5eXm5ubnRzPn5%2Bfo6Ojp6enqfmzq6urr6%2Bvt7e3t7u3uDwvugwbu7u7v6Obv8fDz8%2FP09PT2igP29vb4%2BPj6y376%2Bu%2F7%2Bfv9%2Ff39%2Fv3%2BkAH%2FAwf%2FtwD%2F9wCyh1KfAAAAKXRSTlMABQ4VGykqLjVCTVNgdXuHj5Kaq62vt77ExNPX2%2Bju8vX6%2Bvr7%2FP7%2B%2FiiUMfUAAADTSURBVAjXBcFRTsIwHAfgX%2FtvOyjdYDUsRkFjTIwkPvjiOTyX9%2FAIJt7BF570BopEdHOOstHS%2BX0s439RGwnfuB5gSFOZAgDqjQOBivtGkCc7j%2B2e8XNzefWSu%2BsZUD1QfoTq0y6mZsUSvIkRoGYnHu6Yc63pDCjiSNE2kYLdCUAWVmK4zsxzO%2BQQFxNs5b479NHXopkbWX9U3PAwWAVSY%2FpZf1udQ7rfUpQ1CzurDPpwo16Ff2cMWjuFHX9qCV0Y0Ok4Jvh63IABUNnktl%2B6sgP%2BARIxSrT%2FMhLlAAAAAElFTkSuQmCC)](http://spiceprogram.org/oss-sponsorship)

A helper function to create org-mode TODOs out of open issues and review requests in a github repository.

## Installation

### Using straight.el

An example of how you can install this package using [straight.el](https://github.com/raxod502/straight.el):

```org
(use-package org-github-issues :straight (org-github-issues :host github :repo "iensu/org-github-issues")
  :defer t
  :config
  (setq org-github-issues-user "iensu"                                                                                   ;; Specify Github user
        github-repositories '("dekorateio/dekorate" "quarkusio/quarkus")                                                 ;; My repositories
        org-github-issues-org-file "~/Documents/org/github.org"                                                          ;; My org file
        org-github-issues-tags '("github" "triage")                                                                      ;; Always add these labels
        org-github-issues-issue-tags '("issue")                                                                          ;; Add these only on issues
        org-github-issues-pull-tags '("pull")                                                                            ;; Add these only on pull requests
        org-github-issues-auto-schedule "+0d"                                                                            ;; Enable automatic scheduling
        org-github-issues-filter-by-assignee t                                                                           ;; Enable filter by assignee
        org-github-issues-headline-prefix t)                                                                             ;; Prefix all headlines with repository name
  (mapcar (lambda (r) (run-with-idle-timer 3600 t (lambda () (org-github-issues-sync-issues r)))) github-repositories))  ;; When idle for an hour loop over my projects and sync
```


## Usage

Before use you need to set the variable `org-github-issues-org-file` to point to an existing file in which to write the fetched issues.

In the specified file, create a header for each of your github projects, .e.g `iensu/org-github-issues`.
Each header must match the repository name (OWNER/REPO) **exactly**.

Each header can be a top level header or nested and may optionally contain tags:

```org
* Emacs
** iensu/org-github-issues
** sigma/gh
** magit/forge                :magit:
** magit/magit                :magit:
* Other projects
** foo/bar
```

Any header that doesn't match the (OWNER/REPO) pattern will be ignored.

After setup you can run one of the following commands:

- `M-x org-github-issues-sync-issues` Will prompt you for the repository you want to fetch issues for.
- `M-x org-github-issues-sync-pulls`  Will prompt you for the repository you want to fetch issues for.
- `M-x org-github-issues-sync-all`    Will directly fetch all issues and pull requests for all repositories found in the `org-github-issues-org-file`.

## Authentication

If you are experiencing authentication issues you need to set `org-github-issues-user` to the user you want to authenticate with and then [create a Personal Access Token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token).

If you want to track private repositories you will need to select the **repo** scope and **org:read** if you want to also read repositories belonging to your organizations. See [this issue](https://github.com/octokit/octokit.net/issues/1775) for more info.

You can provide the token by using `auth-sources` and add an entry to your `~/.authinfo.gpg` file ([this article](https://www.masteringemacs.org/article/keeping-secrets-in-emacs-gnupg-auth-sources) gives a good introduction to working with `auth-sources`):

```
machine org-github-issues login <USERNAME> password <TOKEN>
```

The less secure way is to set `org-github-issues-token`, but please go with the `auth-sources` option if your token has access to private repositories.

## Customization

The following custom options are available:

| Option                               | Type                             | Description                                                                          | Default Value                |
|--------------------------------------|----------------------------------|--------------------------------------------------------------------------------------|------------------------------|
| org-github-issues-org-file           | string                           | Path to an existing `org-mode` file in which to write issues                         | "~/Dropbox/org/projects.org" |
| org-github-issues-filter-by-assignee | boolean                          | Flag to enable filtering issues by assignee.                                         | nil                          |
| org-github-issues-assignee           | string                           | The assignee to use for filtering                                                    | `user-login-name`            |
| org-github-issues-headline-prefix    | boolean                          | Flag to enable prefixing headlines with the repository name                          | nil                          |
| org-github-issues-auto-schedule      | string                           | Threshold for automatically scheduling new issues                                    | "+0d"                        |
| org-github-tag-transformations       | alsit :value-type (group-string) | An alsit with trasnformations to apply to github labels when converting them to tags | '(("[\s/-]+" "_")            |


