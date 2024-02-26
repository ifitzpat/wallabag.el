;;; wallabag.el --- add urls to wallabag from emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Ian FitzPatrick

;; Author: Ian FitzPatrick ian@ianfitzpatrick.eu
;; URL: github.com/ifitzpat/wallabag.el
;; Version: 0.0.2
;; Package-Requires: ((emacs "27.1")(request))
;; Keywords: elfeed bookmarks

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Wallabag is a self-hostable bookmark manager that provides RSS feeds
;; of your bookmarks. With Wallabag.el you can quickly send urls to wallabag.

;;; Code:

(require 'request)

(defun wallabag-request-access-token ()
  (let ()
    (request (concat wallabag-url "/oauth/v2/token")
      :type "POST"
      :sync t
      :data `(("client_id" . ,wallabag-client-id)
	      ("grant_type" . "password")
	      ("username" . ,wallabag-username)
	      ("password" . ,wallabag-password)
	      ("client_secret" . ,wallabag-client-secret))
      :parser 'json-read
      :success (cl-function
		(lambda (&key data &allow-other-keys)
		  (when data
		    (message "access code received")
		    )))
      :error (cl-function
	      (lambda (&rest args &key error-thrown &allow-other-keys)
		(message "Error getting access token: %S" error-thrown)))
      )))

(defun wallabag-post-link (&optional link tags) ; assume that tags is a comma-separated list of tags
  (interactive)
  (let ((access-token (assoc-default 'access_token (request-response-data (wallabag-request-access-token))))
	(url (or link (thing-at-point 'url t))))
    (request (concat wallabag-url "/api/entries.json")
      :type "POST"
      :sync t
      :data `(,(when tags `("tags" . ,tags))
	      ("url" . ,url))
      :headers `(("Authorization" . ,(concat "Bearer " access-token)))
      :success (cl-function
		(lambda (&key data &allow-other-keys)
		  (when data
		    (message "added link to wallabag")
		    )))
      :error (cl-function
	      (lambda (&rest args &key error-thrown &allow-other-keys)
		(message "Error adding url to Wallabag: %S" error-thrown))))))


(defun wallabag-get-tags-list ()
  (interactive)
  (let ((access-token (assoc-default 'access_token (request-response-data (wallabag-request-access-token)))))
      (request (concat wallabag-url "/api/tags")
      :type "GET"
      :sync t
      :headers `(("Authorization" . ,(concat "Bearer " access-token)))
      :parser 'json-read
      :success (cl-function
		(lambda (&key data &allow-other-keys)
		  (when data
		    (let* ((taglist (append data nil)))
                    (setq wallabag-tags (mapcar (lambda (x) `(,(cdr (assoc 'label x))  . ,(cdr (assoc 'id x)))) taglist))
		      )
		    )))
      :error (cl-function
	      (lambda (&rest args &key error-thrown &allow-other-keys)
		(message "Error retrieving tags: %S" error-thrown))))))



(defun wallabag-delete-entry (num)
  (let ((access-token (assoc-default 'access_token (request-response-data (wallabag-request-access-token)))))
    (request (concat wallabag-url "/api/entries/" num)
      :type "DELETE"
      :sync t
      :data `(("entry" . ,num))
      :headers `(("Authorization" . ,(concat "Bearer " access-token)))
      :success (cl-function
		(lambda (&key data &allow-other-keys)
		  (when data
		    (message "deleting entry in wallabag")
		    )))
      :error (cl-function
	      (lambda (&rest args &key error-thrown &allow-other-keys)
		(message "Error deleting entry in Wallabag: %S" error-thrown))))))

(defun wallabag-archive-entry (num)
  (let ((access-token (assoc-default 'access_token (request-response-data (wallabag-request-access-token)))))
    (request (concat wallabag-url "/api/entries/" num)
      :type "PATCH"
      :sync t
      :data `(("archive" . ,num))
      :headers `(("Authorization" . ,(concat "Bearer " access-token)))
      :success (cl-function
		(lambda (&key data &allow-other-keys)
		  (when data
		    (message "Archiving entry in wallabag")
		    )))
      :error (cl-function
	      (lambda (&rest args &key error-thrown &allow-other-keys)
		(message "Error archiving entry in Wallabag: %S" error-thrown))))))

(defun wallabag-tag-entry (num tags)
  (let ((access-token (assoc-default 'access_token (request-response-data (wallabag-request-access-token)))))
    (request (concat wallabag-url "/api/entries/" num "/tags")
      :type "POST"
      :sync t
      :data `(("tags" . ,tags))
      :headers `(("Authorization" . ,(concat "Bearer " access-token)))
      :success (cl-function
		(lambda (&key data &allow-other-keys)
		  (when data
		    (message "Tagging entry in wallabag")
		    )))
      :error (cl-function
	      (lambda (&rest args &key error-thrown &allow-other-keys)
		(message "Error tagging entry in Wallabag: %S" error-thrown))))))


(defun wallabag-remove-tag (entrynum tag) ;tag has to be an int
  (let ((access-token (assoc-default 'access_token (request-response-data (wallabag-request-access-token)))))
    (request (concat wallabag-url "/api/entries/" entrynum "/tags/" tag)
      :type "DELETE"
      :sync t
      :headers `(("Authorization" . ,(concat "Bearer " access-token)))
      :success (cl-function
		(lambda (&key data &allow-other-keys)
		  (when data
		    (message "Untagging entry in wallabag")
		    )))
      :error (cl-function
	      (lambda (&rest args &key error-thrown &allow-other-keys)
		(message "Error untagging entry in Wallabag: %S" error-thrown))))))

(defun wallabag-remove-tags (entrynum tags) ;here tags is a list
  (mapcar (lambda (tag) (wallabag-remove-tag entrynum (cds (assoc tag wallabag-tags)))) tags))


(defun elfeed-show-wallabag-delete ()
  (interactive)
  (let* ((link (elfeed-entry-link elfeed-show-entry))
	 (entrynum (when (string-match ".*/\\([0-9]+\\)" link) (match-string 1 link))))
    (when (and entrynum (y-or-n-p "Are you sure you want to delete this from Wallabag? "))
      (message "Deleting: %s from Wallabag" entrynum)
      (wallabag-delete-entry entrynum))))

(defun elfeed-show-wallabag-archive ()
  (interactive)
  (let* ((link (elfeed-entry-link elfeed-show-entry))
	 (entrynum (when (string-match ".*/\\([0-9]+\\)" link) (match-string 1 link))))
    (when (and entrynum (y-or-n-p "Are you sure you want to archive this in Wallabag? "))
      (message "Archiving: %s from Wallabag" entrynum)
      (wallabag-archive-entry entrynum))))

(defun elfeed-show-wallabag-add ()
  (interactive)
  (let* ((link (elfeed-entry-link elfeed-show-entry)))
    (when link
      (message "Adding: %s to Wallabag" link)
      (wallabag-post-link link))))

;; Credit to Prot: https://github.com/protesilaos
(defun wallabag-eww--get-current-url ()
  "Return the current-page's URL."
  (when (eq major-mode 'eww-mode)
    (plist-get eww-data :url)))

(defun wallabag-eww-add-url ()
  (interactive)
  (wallabag-post-link (wallabag-eww--get-current-url)))

(defun elfeed-search-wallabag-add (entry)
  "Display the currently selected item in a buffer."
  (interactive (list (elfeed-search-selected :ignore-region)))
  (when (elfeed-entry-p entry)
    (unless elfeed-search-remain-on-entry (forward-line))
    (message "Adding: %s to Wallabag" (elfeed-entry-link entry))
    (wallabag-post-link (elfeed-entry-link entry))))

(defun elfeed-search-wallabag-add-and-tag (entry)
  "Display the currently selected item in a buffer."
  (interactive (list (elfeed-search-selected :ignore-region)))
  (when (elfeed-entry-p entry)
    (let* ((tags (elfeed-entry-tags entry))
	   (tagstr (mapcar #'symbol-name tags))
	   (tagstr (remove "unread" tagstr))
	   (tagcsv (string-join tagstr ",")))
      (apply #'elfeed-tag entry (list 'later))
      (message "Adding: %s to Wallabag" (elfeed-entry-link entry))
      (wallabag-post-link (elfeed-entry-link entry) tagcsv))))

(defun elfeed-wallabag-update-entry-tags (entry tags)
  (let* ((feed (elfeed-entry-feed entry))
	 (link (elfeed-entry-link entry))
	 (entrynum (when (string-match ".*/\\([0-9]+\\)" link) (match-string 1 link)))
	 (tagstr (mapcar #'symbol-name tags))
	 (tagstr (remove "unread" tagstr))
	 (tagcsv (string-join tagstr ","))
	 )
    (wallabag-tag-entry entrynum tagcsv)))

(defun elfeed-wallabag-remove-entry-tags (entry tags)
  (let* ((feed (elfeed-entry-feed entry))
	 (link (elfeed-entry-link entry))
	 (entrynum (when (string-match ".*/\\([0-9]+\\)" link) (match-string 1 link)))
	 (tagstr (mapcar #'symbol-name tags))
	 (tagstr (remove "unread" tagstr))
	 )
    (when (string-prefix-p "wallabag" (elfeed-feed-title (elfeed-entry-feed entry)) t)
    (progn
        (wallabag-get-tags-list)
        (wallabag-remove-tags entrynum tagstr)))
    ))

(defun elfeed-wallabag-remove-entries-tags (orig-fun &rest args)
  (if (listp (car args))
      (mapcar (lambda (entry) (elfeed-wallabag-remove-entry-tags entry (cdr args))) (car args))
      (elfeed-wallabag-remove-entry-tags entries (cdr args)))
  (apply orig-fun args))


(defun elfeed-wallabag-update-tags (orig-fun &rest args)
  (mapcar (lambda (entry)
	    (when (string-prefix-p "wallabag" (elfeed-feed-title (elfeed-entry-feed entry)) t)
	      (elfeed-wallabag-update-entry-tags entry (cdr args)))) (car args))
  (apply orig-fun args))

(advice-add 'elfeed-tag :around #'elfeed-wallabag-update-tags)
(advice-add 'elfeed-untag :around #'elfeed-wallabag-remove-entries-tags)

(provide 'wallabag)

;;; wallabag.el ends here
