# org-github-issues

A helper function to create org-mode TODOs out of open issues in a github repository.

## Usage

Before use you need to set the variable `org-github-issues-org-file` to point to an existing
file in which to write the fetched issues.

In the specified file, create a header for your github project, .e.g `iensu/org-github-issues`.
The header must match the repository name (OWNER/REPO) **exactly**.

After setup you can run `M-x org-github-issues-sync-issues`, which will prompt you for the repository
you want to fetch issues for.
