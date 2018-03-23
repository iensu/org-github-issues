;;; org-github-issues.el --- Package for creating todos from Github issues  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Jens Östlund

;; Author: Jens Östlund <jostlund@gmail.com>
;; Keywords: tools
;; Version: 0.0.1
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

(defcustom org-github-issues-org-file "~/Dropbox/org/projects.org"
  "Path to an existing `org-mode' file in which to write issues."
  :type '(file :must-match t)
  :group 'org-github-issues)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ogi--connect ()
  "Return a Github Issues api connection."
  (gh-issues-api "API"))

(defun ogi--fetch-issues (owner repo &optional connection)
  "Return a list of gh-issues-issue objects from OWNER/REPO over CONNECTION."
  (let ((conn (or connection
                  (ogi--connect))))
    (oref (gh-issues-issue-list conn
                                owner
                                repo)
          data)))

(defun ogi--collect-synced-repository-names ()
  "Return a list of potential repository names from `org-github-issues-org-file'."
  (let ((repo-regex "[^\s]+/[^\s]+"))
    (save-excursion
      (with-temp-buffer
        (insert-file-contents org-github-issues-org-file)
        (org-mode)
        (org-element-map (org-element-parse-buffer 'headline) 'headline
          (lambda (hl)
            (let ((title (car (split-string (org-element-property :title hl) " "))))
              (when (and (eq (org-element-property :level hl) 1)
                         (string-match-p repo-regex title))
                title))))))))

(defun ogi--issue-url (owner repo number)
  "Return url to issue based on OWNER, REPO and issue NUMBER."
  (format "https://github.com/%s/%s/issues/%d"
          owner repo number))

(defun ogi--labels-to-tags (issue)
  "Return a string of org tags based on labels from ISSUE."
  (mapcar (lambda (label) (oref label name)) (oref issue labels)))

(defun ogi--create-org-entry (depth owner repo issue)
  "Return a string representation of an org entry generated from headling level DEPTH, OWNER, REPO, ISSUE."
  (let* ((title (oref issue title))
         (number (oref issue number))
         (body (oref issue body))
         (tags (ogi--labels-to-tags issue))
         (link (ogi--issue-url owner repo number))
         (params (list :title (format "#%d: %s" number title)
                       :level depth
                       :todo-keyword "TODO")))
    (org-element-interpret-data
     `(headline ,(if tags
                     (append params (list :tags tags))
                   params)
                (property-drawer nil ((node-property (:key "GH_URL" :value ,link))
                                      (node-property (:key "GH_OWNER" :value ,owner))
                                      (node-property (:key "GH_REPO" :value ,repo))
                                      (node-property (:key "GH_ISSUE_NO" :value ,number))))
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

(defun ogi--get-org-file-headline-position (headline)
  "Return the marker for the given org HEADLINE."
  (org-find-exact-heading-in-directory headline
                                       (file-name-directory org-github-issues-org-file)))

(defun ogi--get-issue-headline-level (headline)
  "Return the subtree level for issues under HEADLINE."
  (let ((pos (ogi--get-org-file-headline-position headline)))
    (save-excursion
      (with-current-buffer (marker-buffer pos)
        (goto-char pos)
        (1+ (org-current-level))))))

(defun ogi--generate-org-entries (depth owner repo issues)
  "Create entries based on DEPTH, OWNER and REPO from ISSUES."
  (mapcar (-partial 'ogi--create-org-entry depth owner repo) issues))

(defun ogi--insert-org-entries (entries headline)
  "Insert ENTRIES under HEADLINE."
  (let ((body (string-join entries "\n"))
        (pos (ogi--get-org-file-headline-position headline)))
    (save-excursion
      (with-current-buffer (marker-buffer pos)
        (goto-char pos)
        (outline-next-heading)
        (insert "\n")
        (insert body)
        (insert "\n")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public functions
;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (let* ((owner-and-repo (split-string repository "/"))
         (owner (car owner-and-repo))
         (repo (cadr owner-and-repo)))
    (ogi--delete-existing-issues owner repo)
    (let* ((issues (ogi--fetch-issues owner repo))
           (level (ogi--get-issue-headline-level repository))
           (entries (ogi--generate-org-entries level owner repo issues)))
      (ogi--insert-org-entries entries repository))))

(provide 'org-github-issues)
;;; org-github-issues.el ends here
