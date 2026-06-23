;;; org-mem-roam-ql.el --- Org dynamic blocks over org-mem -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Artur Yaroshenko

;; Author: Artur Yaroshenko
;; Maintainer: Artur Yaroshenko
;; URL: https://github.com/Artawower/org-mem-roam-ql
;; Package-Requires: ((emacs "27.1") (org "9.5") (org-mem "0.1"))
;; Version: 0.2.0
;; Keywords: outlines, hypermedia, org

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; org-mem-roam-ql provides Org dynamic blocks and random navigation over
;; the org-mem cache using an org-roam-ql-like syntax.
;;
;; The package is not tied to org-node or org-roam. It uses org-mem as the
;; data backend and can query all indexed Org entries or only entries with
;; IDs.
;;
;; Dynamic block:
;;
;;   #+BEGIN: org-mem-roam-ql :query (tags "interview") :format list
;;   #+END:
;;
;; Unique files:
;;
;;   #+BEGIN: org-mem-roam-ql :query (tags "interview") :result files :format list
;;   #+END:
;;
;; Backward-compatible org-roam-ql block name:
;;
;;   #+BEGIN: org-roam-ql :query (tags "interview") :result files :format list
;;   #+END:
;;
;; Random note:
;;
;;   M-x org-mem-roam-ql-random
;;
;; Query examples:
;;
;;   python
;;   tag "python"
;;   tags "python"
;;   (tags "python")
;;   (and (tags "python") (not (tags "archive")))

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'org)
(require 'org-mem)

(defgroup org-mem-roam-ql nil
  "Org dynamic blocks and random navigation over the org-mem cache."
  :group 'org
  :prefix "org-mem-roam-ql-")

(defcustom org-mem-roam-ql-default-scope 'entries
  "Default query scope.

The value `entries' means all Org entries indexed by org-mem.
The value `nodes' means entries with IDs indexed by org-mem."
  :group 'org-mem-roam-ql
  :type '(choice
          (const :tag "All org-mem entries" entries)
          (const :tag "Only entries with ID" nodes)))

(defcustom org-mem-roam-ql-default-result 'entries
  "Default result kind.

The value `entries' outputs matched headings.
The value `files' outputs unique files that contain matched headings."
  :group 'org-mem-roam-ql
  :type '(choice
          (const :tag "Entries" entries)
          (const :tag "Unique files" files)))

(defcustom org-mem-roam-ql-default-format 'table
  "Default output format."
  :group 'org-mem-roam-ql
  :type '(choice
          (const :tag "Org table" table)
          (const :tag "Org list" list)))

(defcustom org-mem-roam-ql-default-sort 'title
  "Default sort column."
  :group 'org-mem-roam-ql
  :type '(choice
          (const :tag "Title" title)
          (const :tag "File" file)
          (const :tag "Tags" tags)
          (const :tag "Todo" todo)
          (const :tag "Priority" priority)
          (symbol :tag "Custom column")))

(defcustom org-mem-roam-ql-default-columns '(link file tags)
  "Default table columns."
  :group 'org-mem-roam-ql
  :type 'sexp)

(defcustom org-mem-roam-ql-random-default-query ""
  "Default query for `org-mem-roam-ql-random'."
  :group 'org-mem-roam-ql
  :type 'string)

(defcustom org-mem-roam-ql-random-default-scope 'entries
  "Default scope for `org-mem-roam-ql-random'."
  :group 'org-mem-roam-ql
  :type '(choice
          (const :tag "All org-mem entries" entries)
          (const :tag "Only entries with ID" nodes)))

(defcustom org-mem-roam-ql-random-default-result 'files
  "Default result type for `org-mem-roam-ql-random'.

The value `files' opens a random unique file that contains a match.
The value `entries' opens a random matched heading."
  :group 'org-mem-roam-ql
  :type '(choice
          (const :tag "Unique files" files)
          (const :tag "Matched entries" entries)))

(defun org-mem-roam-ql--param-value (params key)
  "Return value for KEY from dynamic block PARAMS."
  (let ((tail params)
        (result nil)
        (found nil))
    (while (and (consp tail) (not found))
      (if (eq (car tail) key)
          (progn
            (setq result (cadr tail))
            (setq found t))
        (setq tail (cdr tail))))
    result))

(defun org-mem-roam-ql--read (value fallback)
  "Read VALUE as Lisp data or return FALLBACK."
  (cond
   ((null value) fallback)
   ((not (stringp value)) value)
   ((string-empty-p value) fallback)
   (t (car (read-from-string value)))))

(defun org-mem-roam-ql--param (params key fallback)
  "Return KEY from PARAMS after reading strings, or FALLBACK."
  (org-mem-roam-ql--read
   (org-mem-roam-ql--param-value params key)
   fallback))

(defun org-mem-roam-ql--symbol-param (params key fallback)
  "Return KEY from PARAMS as a symbol, or FALLBACK."
  (let ((value (org-mem-roam-ql--param params key fallback)))
    (cond
     ((symbolp value) value)
     ((stringp value) (intern value))
     (t fallback))))

(defun org-mem-roam-ql--entries (scope)
  "Return org-mem entries for SCOPE."
  (pcase scope
    ('nodes (org-mem-all-id-nodes))
    (_ (org-mem-all-entries))))

(defun org-mem-roam-ql--tag (tag)
  "Normalize TAG."
  (replace-regexp-in-string "\\`:\\|:\\'" "" (format "%s" tag)))

(defun org-mem-roam-ql--tags (entry)
  "Return normalized inherited tags for ENTRY."
  (mapcar #'org-mem-roam-ql--tag (org-mem-entry-tags entry)))

(defun org-mem-roam-ql--tag-string (entry)
  "Return ENTRY tags formatted as an Org tag string."
  (let ((tags (org-mem-roam-ql--tags entry)))
    (if tags
        (concat ":" (string-join tags ":") ":")
      "")))

(defun org-mem-roam-ql--string (value)
  "Convert VALUE to string."
  (cond
   ((null value) "")
   ((stringp value) value)
   ((numberp value) (number-to-string value))
   (t (format "%s" value))))

(defun org-mem-roam-ql--title (entry)
  "Return display title for ENTRY."
  (or
   (org-mem-entry-title entry)
   (when-let ((file (org-mem-entry-file-truename entry)))
     (file-name-base file))
   "Untitled"))

(defun org-mem-roam-ql--property (entry name)
  "Return property NAME from ENTRY."
  (let* ((key (format "%s" name))
         (lower-key (downcase key))
         (upper-key (upcase key)))
    (pcase lower-key
      ("title" (org-mem-roam-ql--title entry))
      ("file" (org-mem-entry-file-truename entry))
      ("id" (org-mem-entry-id entry))
      ("todo" (org-mem-entry-todo-state entry))
      ("priority" (org-mem-entry-priority entry))
      ("tags" (string-join (org-mem-roam-ql--tags entry) ":"))
      (_
       (or
        (org-mem-entry-property-with-inheritance key entry)
        (org-mem-entry-property-with-inheritance upper-key entry)
        (org-mem-entry-property key entry)
        (org-mem-entry-property upper-key entry))))))

(defun org-mem-roam-ql--like (actual expected)
  "Return non-nil if ACTUAL contains EXPECTED."
  (let ((case-fold-search t))
    (string-match-p
     (regexp-quote (org-mem-roam-ql--string expected))
     (org-mem-roam-ql--string actual))))

(defun org-mem-roam-ql--match (entry query)
  "Return non-nil if ENTRY matches QUERY."
  (pcase query
    ((pred null) t)
    ('all t)
    (`(all) t)
    (`(and . ,items)
     (cl-every
      (lambda (item)
        (org-mem-roam-ql--match entry item))
      items))
    (`(or . ,items)
     (seq-some
      (lambda (item)
        (org-mem-roam-ql--match entry item))
      items))
    (`(not ,item)
     (not (org-mem-roam-ql--match entry item)))
    (`(tag ,tag)
     (member (org-mem-roam-ql--tag tag) (org-mem-roam-ql--tags entry)))
    (`(tags . ,tags)
     (cl-every
      (lambda (tag)
        (member (org-mem-roam-ql--tag tag) (org-mem-roam-ql--tags entry)))
      tags))
    (`(properties ,name ,value)
     (string=
      (org-mem-roam-ql--string (org-mem-roam-ql--property entry name))
      (org-mem-roam-ql--string value)))
    (`(property ,name ,value)
     (string=
      (org-mem-roam-ql--string (org-mem-roam-ql--property entry name))
      (org-mem-roam-ql--string value)))
    (`(properties~ ,name ,value)
     (org-mem-roam-ql--like
      (org-mem-roam-ql--property entry name)
      value))
    (`(property~ ,name ,value)
     (org-mem-roam-ql--like
      (org-mem-roam-ql--property entry name)
      value))
    (`(title ,value)
     (org-mem-roam-ql--like (org-mem-roam-ql--title entry) value))
    (`(file ,value)
     (org-mem-roam-ql--like (org-mem-entry-file-truename entry) value))
    (`(todo ,value)
     (string=
      (org-mem-roam-ql--string (org-mem-entry-todo-state entry))
      (org-mem-roam-ql--string value)))
    (`(priority ,value)
     (string=
      (org-mem-roam-ql--string (org-mem-entry-priority entry))
      (org-mem-roam-ql--string value)))
    (_ nil)))

(defun org-mem-roam-ql--query (params)
  "Return effective query from PARAMS."
  (let ((query (org-mem-roam-ql--param params :query nil))
        (include (org-mem-roam-ql--param params :include nil))
        (exclude (org-mem-roam-ql--param params :exclude nil)))
    (or query
        (cond
         ((and include exclude) `(and ,include (not ,exclude)))
         (include include)
         (exclude `(not ,exclude))
         (t 'all)))))

(defun org-mem-roam-ql--entry-link (entry)
  "Return Org link to ENTRY."
  (if-let ((id (org-mem-entry-id entry)))
      (org-link-make-string
       (concat "id:" id)
       (org-mem-roam-ql--title entry))
    (org-link-make-string
     (format "file:%s::%d"
             (org-link-escape (org-mem-entry-file-truename entry))
             (org-mem-entry-lnum entry))
     (org-mem-roam-ql--title entry))))

(defun org-mem-roam-ql--file-link (entry)
  "Return Org link to ENTRY file."
  (let ((file (org-mem-entry-file-truename entry)))
    (org-link-make-string
     (concat "file:" (org-link-escape file))
     (file-name-base file))))

(defun org-mem-roam-ql--unique-files (entries)
  "Return unique file representatives from ENTRIES."
  (let ((files nil)
        (result nil))
    (dolist (entry entries)
      (let ((file (org-mem-entry-file-truename entry)))
        (unless (member file files)
          (push file files)
          (push entry result))))
    (nreverse result)))

(defun org-mem-roam-ql--custom-column-p (column)
  "Return non-nil when COLUMN has a custom title."
  (and
   (consp column)
   (consp (car column))
   (cdr column)
   (null (cddr column))))

(defun org-mem-roam-ql--normalize-column (column)
  "Normalize COLUMN."
  (if (stringp column)
      (intern column)
    column))

(defun org-mem-roam-ql--column-expr (column)
  "Return expression part of COLUMN."
  (if (org-mem-roam-ql--custom-column-p column)
      (car column)
    (org-mem-roam-ql--normalize-column column)))

(defun org-mem-roam-ql--column-title (column)
  "Return title for COLUMN."
  (cond
   ((org-mem-roam-ql--custom-column-p column)
    (org-mem-roam-ql--string (cadr column)))
   ((and (consp column) (eq (car column) 'property))
    (org-mem-roam-ql--string (cadr column)))
   ((symbolp column)
    (capitalize (symbol-name column)))
   ((stringp column)
    (capitalize column))
   (t
    (org-mem-roam-ql--string column))))

(defun org-mem-roam-ql--entry-column-value (entry column)
  "Return ENTRY value for COLUMN."
  (pcase (org-mem-roam-ql--column-expr column)
    ('link (org-mem-roam-ql--entry-link entry))
    ('title (org-mem-roam-ql--title entry))
    ('file (abbreviate-file-name (org-mem-entry-file-truename entry)))
    ('id (or (org-mem-entry-id entry) ""))
    ('tags (string-join (org-mem-roam-ql--tags entry) ":"))
    ('todo (or (org-mem-entry-todo-state entry) ""))
    ('priority (or (org-mem-entry-priority entry) ""))
    (`(property ,name) (or (org-mem-roam-ql--property entry name) ""))
    ((pred symbolp) (or (org-mem-roam-ql--property entry column) ""))
    (_ "")))

(defun org-mem-roam-ql--file-column-value (entry column)
  "Return file result value for COLUMN."
  (pcase (org-mem-roam-ql--column-expr column)
    ('link (org-mem-roam-ql--file-link entry))
    ('title (file-name-base (org-mem-entry-file-truename entry)))
    ('file (abbreviate-file-name (org-mem-entry-file-truename entry)))
    (_ (org-mem-roam-ql--entry-column-value entry column))))

(defun org-mem-roam-ql--cell (value)
  "Escape VALUE for Org table cell."
  (replace-regexp-in-string "|" "\\vert{}" (org-mem-roam-ql--string value) t t))

(defun org-mem-roam-ql--sort-value (entry column result)
  "Return sort value for ENTRY by COLUMN and RESULT."
  (if (eq result 'files)
      (org-mem-roam-ql--file-column-value entry column)
    (org-mem-roam-ql--entry-column-value entry column)))

(defun org-mem-roam-ql--sort (entries column result)
  "Sort ENTRIES by COLUMN for RESULT."
  (if column
      (sort entries
            (lambda (left right)
              (string-lessp
               (org-mem-roam-ql--string
                (org-mem-roam-ql--sort-value left column result))
               (org-mem-roam-ql--string
                (org-mem-roam-ql--sort-value right column result)))))
    entries))

(defun org-mem-roam-ql--take (entries take)
  "Return TAKE entries from ENTRIES."
  (cond
   ((and (integerp take) (> take 0))
    (seq-take entries take))
   ((and (integerp take) (< take 0))
    (last entries (- take)))
   (t entries)))

(defun org-mem-roam-ql--list-link (entry result)
  "Return list link for ENTRY and RESULT."
  (if (eq result 'files)
      (org-mem-roam-ql--file-link entry)
    (org-mem-roam-ql--entry-link entry)))

(defun org-mem-roam-ql--list-line (entry result show-tags indent)
  "Insert list line for ENTRY."
  (insert
   (format "%s- %s%s\n"
           indent
           (org-mem-roam-ql--list-link entry result)
           (if show-tags
               (let ((tags (org-mem-roam-ql--tag-string entry)))
                 (if (string-empty-p tags)
                     ""
                   (concat " " tags)))
             ""))))

(defun org-mem-roam-ql--group-key (entry group-by result)
  "Return grouping key for ENTRY."
  (let ((value (org-mem-roam-ql--sort-value entry group-by result)))
    (if (string-empty-p (org-mem-roam-ql--string value))
        "No group"
      (org-mem-roam-ql--string value))))

(defun org-mem-roam-ql--groups (entries group-by result)
  "Return grouped ENTRIES by GROUP-BY for RESULT."
  (let ((groups nil))
    (dolist (entry entries)
      (let* ((key (org-mem-roam-ql--group-key entry group-by result))
             (group (assoc key groups)))
        (if group
            (setcdr group (cons entry (cdr group)))
          (push (cons key (list entry)) groups))))
    (mapcar
     (lambda (group)
       (cons (car group) (nreverse (cdr group))))
     (nreverse groups))))

(defun org-mem-roam-ql--insert-list (entries result show-tags group-by)
  "Insert ENTRIES as an Org list."
  (if group-by
      (dolist (group (org-mem-roam-ql--groups entries group-by result))
        (insert (format "- %s\n" (car group)))
        (dolist (entry (cdr group))
          (org-mem-roam-ql--list-line entry result show-tags "  ")))
    (dolist (entry entries)
      (org-mem-roam-ql--list-line entry result show-tags ""))))

(defun org-mem-roam-ql--insert-table (entries columns value-function)
  "Insert ENTRIES as an Org table."
  (insert "| ")
  (insert (mapconcat #'org-mem-roam-ql--column-title columns " | "))
  (insert " |\n|-\n")
  (dolist (entry entries)
    (insert "| ")
    (insert
     (mapconcat
      (lambda (column)
        (org-mem-roam-ql--cell
         (funcall value-function entry column)))
      columns
      " | "))
    (insert " |\n"))
  (org-table-align))

(defun org-mem-roam-ql-dblock (params)
  "Write an org-mem-roam-ql dynamic block using PARAMS."
  (let* ((scope (org-mem-roam-ql--symbol-param
                 params
                 :scope
                 org-mem-roam-ql-default-scope))
         (query (org-mem-roam-ql--query params))
         (columns (org-mem-roam-ql--param
                   params
                   :columns
                   org-mem-roam-ql-default-columns))
         (format-value (org-mem-roam-ql--symbol-param
                        params
                        :format
                        org-mem-roam-ql-default-format))
         (result (org-mem-roam-ql--symbol-param
                  params
                  :result
                  org-mem-roam-ql-default-result))
         (sort-column (org-mem-roam-ql--param
                       params
                       :sort
                       org-mem-roam-ql-default-sort))
         (take (org-mem-roam-ql--param params :take nil))
         (show-tags (org-mem-roam-ql--param params :show-tags nil))
         (group-by (org-mem-roam-ql--param params :group-by nil))
         (matched-entries
          (seq-filter
           (lambda (entry)
             (org-mem-roam-ql--match entry query))
           (org-mem-roam-ql--entries scope)))
         (result-entries
          (if (eq result 'files)
              (org-mem-roam-ql--unique-files matched-entries)
            matched-entries))
         (sorted-entries
          (org-mem-roam-ql--sort result-entries sort-column result))
         (final-entries
          (org-mem-roam-ql--take sorted-entries take)))
    (pcase (list result format-value)
      (`(files list)
       (org-mem-roam-ql--insert-list final-entries 'files show-tags group-by))
      (`(files ,_)
       (org-mem-roam-ql--insert-table
        final-entries
        columns
        #'org-mem-roam-ql--file-column-value))
      (`(_ list)
       (org-mem-roam-ql--insert-list final-entries 'entries show-tags group-by))
      (_
       (org-mem-roam-ql--insert-table
        final-entries
        columns
        #'org-mem-roam-ql--entry-column-value)))))

(defvar org-mem-roam-ql-random--last-query-string nil
  "Last raw query string used by `org-mem-roam-ql-random'.")

(defvar org-mem-roam-ql-random--last-query nil
  "Last parsed query used by `org-mem-roam-ql-random'.")

(defvar org-mem-roam-ql-random--last-scope nil
  "Last scope used by `org-mem-roam-ql-random'.")

(defvar org-mem-roam-ql-random--last-result nil
  "Last result kind used by `org-mem-roam-ql-random'.")

(defvar org-mem-roam-ql-random--last-entry-key nil
  "Stable key for the last entry opened by `org-mem-roam-ql-random'.")

(defun org-mem-roam-ql-random--read-query-form (value)
  "Read VALUE as an org-mem-roam-ql query form."
  (let ((input (string-trim value)))
    (cond
     ((string-empty-p input)
      'all)
     ((string-prefix-p "(" input)
      (car (read-from-string input)))
     ((string-match-p "\\`[[:alnum:]_@#%:-]+\\'" input)
      `(tags ,input))
     (t
      (car (read-from-string (concat "(" input ")")))))))

(defun org-mem-roam-ql-random--read-query-string ()
  "Read raw random query string from minibuffer."
  (read-string
   "org-mem-roam-ql random query: "
   (or org-mem-roam-ql-random--last-query-string
       org-mem-roam-ql-random-default-query)))

(defun org-mem-roam-ql-random--read-query-pair ()
  "Read random query and return (QUERY-STRING . QUERY)."
  (let ((query-string (org-mem-roam-ql-random--read-query-string)))
    (cons query-string
          (org-mem-roam-ql-random--read-query-form query-string))))

(defun org-mem-roam-ql-random--read-query ()
  "Read random query from minibuffer."
  (cdr (org-mem-roam-ql-random--read-query-pair)))

(defun org-mem-roam-ql-random--matches (query scope result)
  "Return entries matching QUERY, SCOPE and RESULT."
  (let ((entries
         (seq-filter
          (lambda (entry)
            (org-mem-roam-ql--match entry query))
          (org-mem-roam-ql--entries scope))))
    (if (eq result 'files)
        (org-mem-roam-ql--unique-files entries)
      entries)))

(defun org-mem-roam-ql-random--entry-position (entry)
  "Return ENTRY position as an integer, or nil."
  (let ((pos (org-mem-entry-pos entry)))
    (cond
     ((markerp pos) (marker-position pos))
     ((numberp pos) pos)
     (t nil))))

(defun org-mem-roam-ql-random--entry-key (entry result)
  "Return stable comparison key for ENTRY and RESULT."
  (let ((file (org-mem-entry-file-truename entry)))
    (if (eq result 'files)
        (list 'file file)
      (list
       'entry
       file
       (or
        (org-mem-entry-id entry)
        (org-mem-roam-ql-random--entry-position entry)
        (org-mem-entry-lnum entry)
        (org-mem-roam-ql--title entry))))))

(defun org-mem-roam-ql-random--current-key (result)
  "Return stable comparison key for current buffer location and RESULT."
  (when-let ((file-name buffer-file-name))
    (let ((file (file-truename file-name)))
      (if (eq result 'files)
          (list 'file file)
        (when (derived-mode-p 'org-mode)
          (condition-case nil
              (save-excursion
                (org-with-wide-buffer
                 (org-back-to-heading t)
                 (list
                  'entry
                  file
                  (or
                   (org-entry-get nil "ID")
                   (point)))))
            (error nil)))))))

(defun org-mem-roam-ql-random--excluded-keys (result)
  "Return keys that should not be picked for RESULT."
  (delq
   nil
   (list
    (org-mem-roam-ql-random--current-key result)
    org-mem-roam-ql-random--last-entry-key)))

(defun org-mem-roam-ql-random--pick (entries &optional result excluded-keys)
  "Pick a random entry from ENTRIES.

When RESULT and EXCLUDED-KEYS are non-nil, skip entries whose key is
present in EXCLUDED-KEYS."
  (let ((candidates
         (if (and result excluded-keys)
             (seq-remove
              (lambda (entry)
                (member
                 (org-mem-roam-ql-random--entry-key entry result)
                 excluded-keys))
              entries)
           entries)))
    (when candidates
      (nth (random (length candidates)) candidates))))

(defun org-mem-roam-ql-random--goto-file (entry)
  "Open file for ENTRY."
  (find-file (org-mem-entry-file-truename entry)))

(defun org-mem-roam-ql-random--goto-entry (entry)
  "Open ENTRY location."
  (find-file (org-mem-entry-file-truename entry))
  (cond
   ((number-or-marker-p (org-mem-entry-pos entry))
    (goto-char (org-mem-entry-pos entry)))
   ((numberp (org-mem-entry-lnum entry))
    (goto-char (point-min))
    (forward-line (1- (org-mem-entry-lnum entry)))))
  (org-show-context))

(defun org-mem-roam-ql-random--goto (entry result)
  "Open ENTRY according to RESULT."
  (if (eq result 'files)
      (org-mem-roam-ql-random--goto-file entry)
    (org-mem-roam-ql-random--goto-entry entry)))

(defun org-mem-roam-ql-random--message (entry matches-count result)
  "Show message for opened ENTRY."
  (message
   "Opened random %s: %s (%d match%s)"
   (if (eq result 'files) "file" "entry")
   (org-mem-roam-ql--title entry)
   matches-count
   (if (= matches-count 1) "" "es")))

(defun org-mem-roam-ql-random--query-label (query query-string)
  "Return human-readable label for QUERY and QUERY-STRING."
  (if (and (stringp query-string)
           (not (string-empty-p (string-trim query-string))))
      query-string
    (format "%S" query)))

(defun org-mem-roam-ql-random--open (query query-string scope result &optional avoid-current)
  "Open a random match for QUERY.

QUERY-STRING is the raw minibuffer input stored for
`org-mem-roam-ql-random-repeat'.

When AVOID-CURRENT is non-nil, do not pick the current file/entry."
  (let* ((matches (org-mem-roam-ql-random--matches query scope result))
         (excluded-keys
          (when avoid-current
            (org-mem-roam-ql-random--excluded-keys result)))
         (entry
          (org-mem-roam-ql-random--pick matches result excluded-keys)))
    (unless entry
      (if matches
          (user-error
           "No other matches for query: %s"
           (org-mem-roam-ql-random--query-label query query-string))
        (user-error "No matches for query: %S" query)))
    (org-mem-roam-ql-random--goto entry result)
    (setq org-mem-roam-ql-random--last-query-string query-string
          org-mem-roam-ql-random--last-query query
          org-mem-roam-ql-random--last-scope scope
          org-mem-roam-ql-random--last-result result
          org-mem-roam-ql-random--last-entry-key
          (org-mem-roam-ql-random--entry-key entry result))
    (org-mem-roam-ql-random--message entry (length matches) result)))

;;;###autoload
(defun org-mem-roam-ql-random (&optional entry-result)
  "Open a random note matching an org-mem-roam-ql query.

The command asks for a query in minibuffer.

Accepted forms include:

  python
  tag \"python\"
  tags \"python\"
  (tags \"python\")
  (and (tags \"python\") (not (tags \"archive\")))

By default the command opens a random unique file. With ENTRY-RESULT,
or interactively with universal argument, it opens a random matched
entry instead.

The current file/entry is excluded from candidates when it can be
detected."
  (interactive "P")
  (let* ((query-pair (org-mem-roam-ql-random--read-query-pair))
         (query-string (car query-pair))
         (query (cdr query-pair))
         (scope org-mem-roam-ql-random-default-scope)
         (result (if entry-result
                     'entries
                   org-mem-roam-ql-random-default-result)))
    (org-mem-roam-ql-random--open query query-string scope result t)))

;;;###autoload
(defun org-mem-roam-ql-random-repeat ()
  "Repeat the last random query and open a different match.

The command reuses the last raw query string, scope and result kind from
`org-mem-roam-ql-random'. It explicitly excludes the current file/entry
from candidates."
  (interactive)
  (unless (and org-mem-roam-ql-random--last-query-string
               org-mem-roam-ql-random--last-query
               org-mem-roam-ql-random--last-scope
               org-mem-roam-ql-random--last-result)
    (user-error "No previous random query; call org-mem-roam-ql-random first"))
  (org-mem-roam-ql-random--open
   org-mem-roam-ql-random--last-query
   org-mem-roam-ql-random--last-query-string
   org-mem-roam-ql-random--last-scope
   org-mem-roam-ql-random--last-result
   t))

;;;###autoload
(defun org-mem-roam-ql-random-entry ()
  "Open a random matched entry instead of a unique file."
  (interactive)
  (org-mem-roam-ql-random t))

;;;###autoload
(defun org-mem-roam-ql-enable-org-roam-ql-alias ()
  "Enable the `org-roam-ql' dynamic block alias.

This function is kept for compatibility with older configuration snippets.
The alias is already registered by default."
  (interactive)
  (defalias 'org-dblock-write:org-roam-ql #'org-dblock-write:org-roam-ql))

(provide 'org-mem-roam-ql)

;;; org-mem-roam-ql.el ends here
