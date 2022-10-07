;;; org-github-issues.el --- Package for creating todos from Github issues  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Jens Östlund

;; Author: Jens Östlund <jostlund@gmail.com>
;; Keywords: tools
;; Version: 0.0.3
;; URL: https://github.com/iensu/org-github-issues

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; org-github-issues exposes a interactive function `org-github-issues-sync-issues'
;; which prompts for repository name and owner, and then creates org-modo TODOs of
;; all of the open issues in the repository.

;;; Code:

(require 'gh)
(require 'dash)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization options
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup org-github-issues nil
  "Tool for creating org-mode todos out of Github issues"
  :group 'tools)

(defcustom org-github-issues-user nil
  "Github username with which you authenticate to Github."
  :type 'string
  :group 'org-github-issues)

(defcustom org-github-issues-token nil
  "Github Personal Access Token to use for authentication.

This variable exists purely for convenience and should be avoided. Please use `auth-sources' as described in the README instead."
  :type 'string
  :group 'org-github-issues)

(defcustom org-github-issues-org-file "~/Dropbox/org/projects.org"
  "Path to an existing `org-mode' file in which to write issues."
  :type '(file :must-match t)
  :group 'org-github-issues)

(defcustom org-github-issues-filter-by-assignee nil
  "Flag to enable filtering issues by assignee."
  :type 'boolean
  :group 'org-github-issues)

(defcustom org-github-issues-assignee nil
  "The assignee to use for issue filtering if different from `org-github-issuess-user'."
  :type 'string
  :group 'org-github-issues)

(defcustom org-github-issues-headline-prefix nil
  "Flag to enable prefixing headlines with the repostiory name."
  :type 'boolean
  :group 'org-github-issues)

(defcustom org-github-issues-auto-schedule "+0d"
  "Threshold for automatically scheduling new issues."
  :type 'string
  :group 'org-github-issues)

(defcustom org-github-issues-tags nil
  "An alist of org-mode tags to add to all org entries."
  :type '(alist :value-type (group string))
  :group 'org-github-issues)

(defcustom org-github-issues-issue-tags nil
  "An alist of org-mode tags to add to entries corresponding to issues."
  :type '(alist :value-type (group string))
  :group 'org-github-issues)

(defcustom org-github-issues-pull-tags nil
  "An alist of org-mode tags to add to entries corresponding to pull requests."
  :type '(alist :value-type (group string))
  :group 'org-github-issues)

(defcustom org-github-issues-tag-transformations '(("[\s/-]+" "_"))
  "An alist with transformation to apply to github labels when converting them to org-mode tags."
  :type '(alist :value-type (group string))
  :group 'org-github-issues)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repository structure
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ogi--repository-create  (name level tags)
  "Create a repository entry with the specified NAME LEVEL and TAGS."
  (list name level tags))

(defun ogi--repository-name  (repo)
  "Get the repository name."
  (nth 0 repo))

(defun ogi--repository-level  (repo)
  "Get the repository level."
  (nth 1 repo))

(defun ogi--repository-tags  (repo)
  "Get the repository tags."
  (nth 2 repo))

(defun ogi--repository-named  (repositories name)
  "Get the first repository from REPOSITORIES with a matching NAME, or nil if there is no match."
  (let* ((filtered (seq-filter (lambda (r) (string= name (ogi--repository-name r))) repositories)))
    (if filtered (car filtered) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ogi--get-auth-token (username)
  "Returns Github Personal Access Token from `auth-sources' or `org-github-issues-token' if it exists."
  (if org-github-issues-token
      org-github-issues-token
    (let* ((auth-entries (auth-source-search :host "org-github-issues"
                                             :user username
                                             :max 1))
           (secret       (plist-get (car auth-entries) :secret)))
      (if (functionp secret)
          (funcall secret)
        secret))))

(defun ogi--connect ()
  "Return a Github Issues api connection."
  (if org-github-issues-user
      (gh-issues-api "api" :auth (gh-oauth-authenticator :username org-github-issues-user
                                                         :token (ogi--get-auth-token org-github-issues-user)))
    (gh-issues-api "api")))


(defun ogi--pulls-connect ()
  "Return a Github Issues api connection."
  (if org-github-issues-user
      (gh-pulls-api "api" :auth (gh-oauth-authenticator :username org-github-issues-user
                                                         :token (ogi--get-auth-token org-github-issues-user)))
    (gh-pulls-api "api")))

(defun ogi--fetch-pulls (owner repo &optional connection)
  "Return a list of gh-issues-pull objects from OWNER/REPO over CONNECTION."
  (let ((conn (or connection
                  (ogi--pulls-connect))))
    (delete-dups
     (append
      ;; Get pull requests assigned
      (oref (ogi--pulls-pull-list conn
                                  owner
                                  repo
                                  (if (and org-github-issues-filter-by-assignee
                                           (or org-github-issues-assignee
                                               org-github-issues-user))
                                      (or org-github-issues-assignee org-github-issues-user)
                                    nil))
            data)
      ;; Get pull requests requested for review
      (oref (ogi--pulls-pull-list conn
                                  owner
                                  repo)
            data)))))

(defun ogi--fetch-issues (owner repo &optional connection)
  "Return a list of gh-issues-issue objects from OWNER/REPO over CONNECTION."
  (let ((conn (or connection
                  (ogi--connect))))
    (oref (ogi--issues-issue-list conn
                                  owner
                                  repo
                                  (if (and org-github-issues-filter-by-assignee
                                           (or org-github-issues-assignee
                                               org-github-issues-user))
                                      (or org-github-issues-assignee org-github-issues-user)
                                    nil))
          data)))

(defun ogi--repo-header-exists-p (repository)
  "Return t if a level 1 header for REPOSITORY exists in `org-github-issues-org-file'."
  (let ((repo-headers (ogi--collect-synced-repository-names)))
    (member repository repo-headers)))

(defun ogi--insert-repo-header (repository)
  "Append a level 1 header for REPOSITORY into `org-github-issues-org-file'."
  (save-excursion
    (with-temp-buffer
      (insert (concat "\n"
                      (org-element-interpret-data
                       `(headline (:title ,repository :level 1)
                                  (property-drawer nil
                                                   ((node-property (:key "CATEGORY" :value ,repository))
                                                    (node-property (:key "GH_URL" :value ,(format "https://github.com/%s" repository)))))))))
      (append-to-file (point-min)
                      (point-max)
                      org-github-issues-org-file))))

(defun ogi--collect-synced-repository-names ()
  "Return a list of potential repositories nanes from `org-github-issues-org-file'."
  (mapcar 'ogi--repository-name (ogi--collect-synced-repositories)))

(defun ogi--collect-synced-repositories ()
  "Return a list of potential repositories from `org-github-issues-org-file'."
  (let ((repo-regex "[^\s]+/[^\s]+"))
    (save-excursion
      (with-temp-buffer
        (insert-file-contents org-github-issues-org-file)
        (org-mode)
        (org-element-map (org-element-parse-buffer 'headline) 'headline
          (lambda (hl)
            (let ((title (car (split-string (org-element-property :title hl) " ")))
                  (level (org-element-property :level hl))
                  (tags (org-element-property :tags hl)))
              (when (string-match-p repo-regex title)
                (ogi--repository-create title level tags)))))))))

(defun ogi--issue-url (owner repo number)
  "Return url to issue based on OWNER, REPO and issue NUMBER."
  (format "https://github.com/%s/%s/issues/%d"
          owner repo number))

(defun ogi--pull-url (owner repo number)
  "Return url to issue based on OWNER, REPO and issue NUMBER."
  (format "https://github.com/%s/%s/pulls/%d"
          owner repo number))

(defun ogi--labels-to-tags (issue)
  "Return a string of org tags based on labels from ISSUE."
  (mapcar (lambda (label) (ogi--replace-multi-regexp-in-string (oref label name) org-github-issues-tag-transformations)) (oref issue labels))) ;;Replace chars that are invalid for tags

(defun ogi--replace-multi-regexp-in-string(s mappings)
  "Replace multiple replace-regexp-in-string in a pipeline fashion (Feed the result of each step as input to the next)."
  (let ((result s))
    (dolist (m mappings)
      (let ((regexp (car m))
            (to-replace (car (cdr m))))
      (setq result (replace-regexp-in-string regexp to-replace result))))
    result))

(defun ogi--scheduled-property (level)
  "Return the scheduled string property."
  (if org-github-issues-auto-schedule (format "%s  SCHEDULED: <%s>\n" (make-string level 32) (org-read-date nil nil org-github-issues-auto-schedule)) nil))

(defun ogi--create-org-entry-for-issue (owner repo level issue)
  "Return a string representation of a LEVEL+1 org TODO headline based on OWNER, REPO, ISSUE."
  (let* ((title (oref issue title))
         (number (oref issue number))
         (assignee (oref (oref issue assignee) login))
         (body (replace-regexp-in-string "^[\*]+" " - " (or (oref issue body) "")))
         (tags (ogi--labels-to-tags issue))
         (link (ogi--issue-url owner repo number))
         (scheduled (ogi--scheduled-property level))
         (params (list :title (if org-github-issues-headline-prefix (format "%s: #%d: %s" repo number title) (format "#%d: %s" number title))
                       :level (+ level 1)
                       :todo-keyword "TODO")))
      (when org-github-issues-tags (setq tags (append org-github-issues-tags tags)))
      (when org-github-issues-issue-tags (setq tags (append org-github-issues-issue-tags tags)))
      (org-element-interpret-data
       `(headline ,(if tags
                       (append params (list :tags tags))
                     params)
                  ,(if scheduled scheduled)
                  (property-drawer nil ((node-property (:key "GH_URL" :value ,link))
                                        (node-property (:key "GH_OWNER" :value ,owner))
                                        (node-property (:key "GH_REPO" :value ,repo))
                                        (node-property (:key "GH_ISSUE_NO" :value ,number))
                                        (node-property (:key "GH_ASSIGNE" :value ,assignee))
                                        ))
                  ,body))))

(defun ogi--create-org-entry-for-pull (owner repo level pull)
  "Return a string representation of a LEVEL+1 org TODO headline based on OWNER, REPO, ISSUE."
  (let* ((title (oref pull title))
         (number (oref pull number))
         (assignee (oref (oref pull assignee) login))
         (body (replace-regexp-in-string "^[\*]+" " - " (or (oref pull body) "")))
         (tags (ogi--labels-to-tags pull))
         (link (ogi--pull-url owner repo number))
         (scheduled (ogi--scheduled-property level))
         (params (list :title (if org-github-issues-headline-prefix (format "%s: #%d: %s" repo number title) (format "#%d: %s" number title))
                       :level (+ level 1)
                       :todo-keyword "TODO")))
      (when org-github-issues-tags (setq tags (append org-github-issues-tags tags)))
      (when org-github-issues-pull-tags (setq tags (append org-github-issues-pull-tags tags)))
      (org-element-interpret-data
       `(headline ,(if tags
                       (append params (list :tags tags))
                     params)
                  ,(if scheduled scheduled)
                  (property-drawer nil ((node-property (:key "GH_URL" :value ,link))
                                        (node-property (:key "GH_OWNER" :value ,owner))
                                        (node-property (:key "GH_REPO" :value ,repo))
                                        (node-property (:key "GH_PULL_NO" :value ,number))
                                        (node-property (:key "GH_ASSIGNE" :value ,assignee))
                                        ))
                  ,body))))

(defun ogi--delete-org-entry ()
  "Delete org entry at point until the next headline."
  (deactivate-mark)
  (outline-mark-subtree)
  (kill-region (region-beginning) (region-end))
  (set-mark (point))
  (outline-next-heading)
  (kill-region (region-beginning) (region-end))
  (deactivate-mark)
  (setq org-map-continue-from (outline-previous-heading)))

(defun ogi--delete-existing-issues (owner repo)
  "Delete all previously created org entries matching OWNER and REPO."
  (let ((match (format "+GH_OWNER={%s}+GH_REPO={%s}" owner repo)))
    (org-map-entries
     'ogi--delete-org-entry
     match
     `(,org-github-issues-org-file))))

(defun ogi--delete-existing-pulls (owner repo)
  "Delete all previously created org entries matching OWNER and REPO."
  (let ((match (format "+GH_OWNER={%s}+GH_REPO={%s}" owner repo)))
    (org-map-entries
     'ogi--delete-org-entry
     match
     `(,org-github-issues-org-file))))

(defun ogi--get-org-file-headline-position (headline)
  "Return the marker for the given org HEADLINE."
  (org-find-exact-heading-in-directory headline
                                       (file-name-directory org-github-issues-org-file)))

(defun ogi--issue-include-p (issue)
  "Predicate that returns non-nil when ISSUE should be included."
  (let ((assignee (oref (oref issue assignee) login)))
    (or (not org-github-issues-filter-by-assignee)
        (and org-github-issues-filter-by-assignee (string= assignee (or org-github-issues-assignee
                                                                        org-github-issues-user))))))

(defun ogi--pull-include-p (pull)
  "Predicate that returns non-nil when PULL should be included."
  (let ((assignee (oref (oref pull assignee) login))
        (reviewers (mapcar (lambda (r) (oref r login)) (oref pull requested_reviewers))))
    (or (not org-github-issues-filter-by-assignee)
        (or (and assignee (string= assignee (or org-github-issues-assignee org-github-issues-user)))
            (member (or org-github-issues-assignee org-github-issues-user) reviewers)))))

(defun ogi--generate-org-entries-for-issues (owner repo level issues)
  "Create entries based on OWNER and REPO from ISSUES."
  (mapcar (-partial 'ogi--create-org-entry-for-issue owner repo level) (seq-filter 'ogi--issue-include-p issues)))

(defun ogi--generate-org-entries-for-pulls (owner repo level pulls)
  "Create entries based on OWNER and REPO from PULLS."
  (mapcar (-partial 'ogi--create-org-entry-for-pull owner repo level) (seq-filter 'ogi--pull-include-p pulls)))


(defun ogi--insert-org-entries (entries headline)
  "Insert ENTRIES under HEADLINE."
  (let ((body (if entries (string-join entries "\n") nil))
        (pos (ogi--get-org-file-headline-position headline)))
    (save-excursion
      (with-current-buffer (marker-buffer pos)
        (goto-char pos)
        (outline-next-heading)
        (when body
          (insert "\n")
          (insert body)
          (insert "\n"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rest methods
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod ogi--issues-issue-list ((api gh-issues-api) user repo &optional assignee)
  (gh-api-authenticated-request
   api (gh-object-list-reader (oref api issue-cls)) "GET"
   (if assignee
       (format "/repos/%s/%s/issues?assignee=%s" user repo assignee)
     (format "/repos/%s/%s/issues" user repo))))

(defmethod ogi--pulls-pull-list ((api gh-pulls-api) user repo &optional assignee)
  (gh-api-authenticated-request
   api (gh-object-list-reader (oref api req-cls)) "GET"
   (if assignee
       (format "/repos/%s/%s/pulls?assignee=%s" user repo assignee)
     (format "/repos/%s/%s/pulls?q=%s" user repo "is%3Apr+is%3Aopen+user-review-requested%3A%40me"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public functions
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun org-github-issues-browse-entry-at-point ()
  "Browse the issue/pull that corresponds to the org entry at point."
  (interactive)
  (let ((origin (current-buffer)))
    (when (eq major-mode 'org-agenda-mode) (org-agenda-switch-to))
    (let* ((p (point))
           (url (string-trim (org-entry-get nil "GH_URL"))))
      (when url (browse-url url))
      (when (not (equal origin (current-buffer))) (switch-to-buffer origin)))))

;;;###autoload
(defun org-github-issues-sync-issues (repository)
  "Fetch and insert all open issues from github REPOSITORY.

Example: https://github.com/foo/bar, the repository is `foo/bar'

Issues will be put under the heading matching REPOSITORY in the file
 specified by `org-github-issues-org-file'.

Executing this function will replace already downloaded issues."
  (interactive
   (list (completing-read "Github repo: "
                          (ogi--collect-synced-repository-names)
                          nil nil)))
  (let* ((repositories (ogi--collect-synced-repositories))
         (owner-and-repo (split-string repository "/"))
         (owner (car owner-and-repo))
         (repo (cadr owner-and-repo))
         (selected (ogi--repository-named repositories repository))
         (level (ogi--repository-level selected))
         (tags (ogi--repository-tags selected))
         (issues (ogi--fetch-issues owner repo)))
    (when (not (ogi--repo-header-exists-p repository))
      (progn
        (message "Creating headline for %s in %s" repository org-github-issues-org-file)
        (ogi--insert-repo-header repository)
        (sleep-for 1))) ; wait for file to be fully updated before adding issues
    (if (not issues)
        (message (format "No open issues found in repository https://github.com/%s" repository))
      (progn
        (ogi--delete-existing-issues owner repo)
        (ogi--insert-org-entries (ogi--generate-org-entries-for-issues owner repo level issues)
                                 repository)))))

(defun org-github-issues-sync-pulls (repository)
  "Fetch and insert all open pulls awaiting review or assigned to the user from github REPOSITORY.

Example: https://github.com/foo/bar, the repository is `foo/bar'

Pulls will be put under the heading matching REPOSITORY in the file
 specified by `org-github-pulls-org-file'.

Executing this function will replace already downloaded issues."
  (interactive
   (list (completing-read "Github repo: "
                          (ogi--collect-synced-repository-names)
                          nil nil)))
  (let* ((repositories (ogi--collect-synced-repositories))
         (owner-and-repo (split-string repository "/"))
         (owner (car owner-and-repo))
         (repo (cadr owner-and-repo))
         (selected (ogi--repository-named repositories repository))
         (level (ogi--repository-level selected))
         (tags (ogi--repository-tags selected))
         (pulls (ogi--fetch-pulls owner repo)))
    (when (not (ogi--repo-header-exists-p repository))
      (progn
        (message "Creating headline for %s in %s" repository org-github-issues-org-file)
        (ogi--insert-repo-header repository)
        (sleep-for 1))) ; wait for file to be fully updated before adding issues
    (if (not pulls)
        (message (format "No assigned/review pending pulls found in repository https://github.com/%s" repository))
      (progn
        (ogi--delete-existing-pulls owner repo)
        (ogi--insert-org-entries (ogi--generate-org-entries-for-pulls owner repo level pulls)
                                 repository)))))

(defun org-github-issues-fetch-all ()
  "Sync all github repository issues and pull requests."
  (interactive)
  (let ((repos (ogi--collect-synced-repository-names)))
    (mapcar (lambda (r) (org-github-issues-sync-issues r)) repos)
    (mapcar (lambda (r) (org-github-issues-sync-pulls r))  repos)))

;; gh-pulls.el does not include requested_reviewers, assignee, assignees and labels, so that needs to change.
(gh-defclass gh-pulls-request (gh-pulls-request-stub)
  ((number :initarg :number)
   (merged :initarg :merged)
   (mergeable :initarg :mergeable)
   (merged-by :initarg :merged-by)
   (comments :initarg :comments)
   (user :initarg :user :initform nil :marshal-type gh-user)
   (labels :initarg :labels :initform nil :marshal-type (list gh-issues-label))
   (assignees :initarg :assignees :initform nil :marshal-type (list gh-user))
   (assignee :initarg :assignee :initform nil :marshal-type gh-user)
   (requested_reviewers :initarg :assignees :initform nil :marshal-type (list gh-user))
   (commits :initarg :commits)
   (additions :initarg :additions)
   (deletions :initarg :deletions)
   (changed-files :initarg :changed-files))
  "Git pull requests API")

(provide 'org-github-issues)
;;; org-github-issues.el ends here
