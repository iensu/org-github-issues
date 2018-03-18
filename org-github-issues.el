;;; org-github-issues.el --- Package for creating todos from Github issues  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Jens Östlund

;; Author: Jens Östlund <jostlund@gmail.com>
;; Keywords: outlines
;; Version: 0.0.1
;; Package-Requires: ((emacs "25") (gh "1.0.0") (dash "2.13.0"))
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

;; Comments...

;;; Code:

(require 'gh)
(require 'dash)

(defun ogi--connect ()
  "Return a Github Issues api connection."
  (gh-issues-api "API"))

(defun ogi--fetch-issues (owner repo &optional connection)
  "Return a list of gh-issues-issue objects over CONNECTION from USER/REPO."
  (let ((conn (or connection
                  (ogi--connect))))
    (oref (gh-issues-issue-list conn
                                owner
                                repo)
          data)))

(defun ogi--issue-url (owner repo number)
  (format "https://github.com/%s/%s/issues/%d"
          owner repo number))

(defun ogi--labels-to-tags (issue)
  (let ((labels (mapcar (lambda (label) (oref label name)) (oref issue labels))))
    (if labels
        (concat ":" (string-join labels ":") ":")
      "")))

(defun ogi--create-org-entry (depth owner repo issue)
  (let* ((title (oref issue title))
         (number (oref issue number))
         (body (oref issue body))
         (tags (ogi--labels-to-tags issue))
         (link (ogi--issue-url owner repo number))
         (level (apply 'concat (make-list depth "*")))
         (body (format (concat "%s TODO #%d: %s %s\n"
                               "  :PROPERTIES:\n"
                               "  :GH_URL: %s\n"
                               "  :GH_OWNER: %s\n"
                               "  :GH_REPO: %s\n"
                               "  :GH_ISSUE_NO: %d\n"
                               "  :END:\n\n"
                               "%s\n")
                       level number title tags link owner repo number body)))
    (s-replace "\r" "" body)))

(defun ogi--delete-org-entry ()
  (deactivate-mark)
  (outline-mark-subtree)
  (kill-region (region-beginning) (region-end))
  (set-mark (point))
  (outline-next-heading)
  (kill-region (region-beginning) (region-end))
  (deactivate-mark))

(defun ogi--delete-existing-issues (owner repo)
  (let ((match (format "+GH_OWNER={%s}+GH_REPO={%s}" owner repo)))
    (org-map-entries
     'ogi--delete-org-entry
     match
     'agenda)))

(defun ogi--get-org-file-headline-position (headline)
  (org-find-exact-heading-in-directory headline iensu-org-dir))

(defun ogi--get-issue-headline-level (headline)
  (let ((pos (ogi--get-org-file-headline-position headline)))
    (save-excursion
      (with-current-buffer (marker-buffer pos)
        (goto-char pos)
        (1+ (org-current-level))))))

(defun ogi--generate-org-entries (depth owner repo issues)
  (mapcar (-partial 'ogi--create-org-entry depth owner repo) issues))

(defun ogi--insert-org-entries (headline entries)
  (let ((body (string-join entries "\n"))
        (pos (ogi--get-org-file-headline-position headline)))
    (save-excursion
      (with-current-buffer (marker-buffer pos)
        (goto-char pos)
        (outline-next-heading)
        (insert "\n")
        (insert body)
        (insert "\n")))))

(defun ogi-sync-issues (repo owner)
  "Fetch and insert all open issues from github REPO by OWNER.

Example: https://github.com/<OWNER>/<REPO>

Issues will be put under heading OWNER/REPO.

This function will replace already downloaded issues."
  (interactive "sGithub repo: \nsRepo owner: ")
  (ogi--delete-existing-issues owner repo)
  (let* ((headline (concat owner "/" repo))
         (issues (ogi--fetch-issues owner repo))
         (level (ogi--get-issue-headline-level headline))
         (entries (ogi--generate-org-entries level owner repo issues)))
    (ogi--insert-org-entries headline entries)))

(provide 'org-github-issues)
;;; org-github-issues.el ends here
