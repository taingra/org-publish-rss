;;; rss.el --- Simple RSS Generator -*- lexical-binding: t -*-

;; Copyright (C) 2025 Thomas Ingram

;; Author: Thomas Ingram <thomas@taingram.org>
;; Version: 0.1
;; Package-Requires: ((emacs "30.1"))
;; Homepage: https://git.sr.ht/~taingram/org-publish-rss
;; Keywords:

;; This file is NOT part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a simple plist format for RSS and functions
;; for converting the plist to valid XML following the RSS 2.0 spec.

;; https://www.rssboard.org/rss-specification

;; Most elements defined in RSS 2.0 is supported.

;; See language codes defined in the RSS Spec.
;; https://www.rssboard.org/rss-language-codes

;; :title
;; :link
;; :description
;; :language
;; :copyright
;;

;; '(:title "John Test Blog"
;;   :link  "https://example.com"
;;   :description "A test feed"
;;   :language "en-us"
;;   :webmaster "admin@email.com (Johnney Admin)"
;;   :editor    "me@email.com (Mitch Edit)"
;;   :copyright "Copyright (C) 2025 ME"
;;   :image     "http://example.com/feed-image.png"
;;   :items ((:title "my article"
;; 	      :date "2025-05-02"
;; 	      :link "http://example/article-1")
;; 	     (:title "my article 2"
;; 	      :date "2025-01-02"
;; 	      :link "http://example/article-2")))


;;; Code:


(defsubst rss-time-string (&optional time zone)
  "Format TIME in RFC822 format expected by RSS.
If TIME is nil or omitted the current time is used.

The optional ZONE is omitted or nil for Emacs local time, t for
Universal Time, ‘wall’ for system wall clock time, or a string as in
the TZ environment variable.  It can also be a list (as from
‘current-time-zone’) or an integer (as from ‘decode-time’) applied
without consideration for daylight saving time."
  (format-time-string "%a, %d %b %Y %H:%M:%S %z" time zone))


;; TODO create function validates and serialize inputs
;; Excluding CDATA sections no XML tags should be used in variables.

(defun rss-to-xml (rss-plist)
  "Create an RSS feed using a RSS-PLIST.

Warning for efficiency this function does no error or input serializing.
This function should NOT be used for untrusted input data.

Expects plist in the following format:

\\='(:title \"John Test Blog\"
  :link  \"https://example.com\"
  :description \"A test feed\"
  :language \"en\"
  :webmaster \"admin@email.com (Johnney Admin) \"
  :editor    \"me@email.com (Mitch Edit)\"
  :copyright \"Copyright (C) 2025 ME\"
  :image     \"http://example.com/feed-image.png\"
  :items ((:title \"my article\"
	   :date \"2025-05-02\"
	   :link \"http://example/article-1\")
	  (:title \"my article 2\"
	   :date \"2025-01-02\"
	   :link \"http://example/article-2\")))"
  (let ((items-xml ""))
    (concat
     (format "<?xml version=\"1.0\" encoding=\"%s\" ?>\n"
	     (or (plist-get rss-plist :encoding) "utf-8"))
     "<rss version=\"2.0\">\n"
     "<channel>\n"
     (when (plist-get rss-plist :self-link)
       (format "<atom:link href=\"%s\" rel=\"self\" type=\"application/rss+xml\" />\n"
	       (plist-get rss-plist :self-link)))
     "<title>"
     (plist-get rss-plist :title)
     "</title>\n"
     "<link>"
     (plist-get rss-plist :link)
     "</link>\n"
     (format "<description><![CDATA[%s]]></description>\n"
	     (plist-get rss-plist :description))
     (when (and (plist-member rss-plist :language)
		(not (string-empty-p (plist-get rss-plist :language))))
       (concat "<language>" (plist-get rss-plist :language) "</language>\n"))
     "<lastBuildDate>"
     (format-time-string "%a, %d %b %Y %H:%M:%S %z")
     "</lastBuildDate>\n"
     "<generator>"
     (or (plist-get rss-plist :generator)
	 (format "Emacs %s; rss.el %s" emacs-version "0.1"))
     "</generator>\n"
     (when (and (plist-member rss-plist :webmaster)
		(not (string-empty-p (plist-get rss-plist :webmaster))))
       (concat "<webMaster>" (plist-get rss-plist :webmaster) "</webMaster>\n"))
     (when (and (plist-member rss-plist :editor)
		(not (string-empty-p (plist-get rss-plist :editor))))
       (concat "<managingEditor>" (plist-get rss-plist :editor) "</managingEditor>\n"))
     (when (and (plist-member rss-plist :copyright)
		(not (string-empty-p (plist-get rss-plist :copyright))))
       (concat "<copyright>" (plist-get rss-plist :copyright) "</copyright>\n"))
     (when (and (plist-member rss-plist :image)
		(not (string-empty-p (plist-get rss-plist :image))))
       (format "<image>\n<url>%s</url>\n<title>%s</title>\n<link>%s</link>\n</image>\n"
	       (plist-get rss-plist :image)
	       (plist-get rss-plist :title)
	       (plist-get rss-plist :link)))
     (dolist (item (plist-get rss-plist :items) items-xml)
       (setq items-xml
	     (concat
	      items-xml
	      "<item>\n"
	      "<title>"
	      (plist-get item :title)
	      "<title>\n"
	      "<pubDate>"
	      (plist-get item :date)
	      "</pubDate>\n"
	      "<link>"
	      (plist-get item :link)
	      "</link>\n"
	      (if (and (plist-member rss-plist :guid)
		       (not (string-empty-p (plist-get rss-plist :guid))))
		  (concat "<guid isPermaLink=\"false\">" (plist-get item :guid))
		(concat "<guid>" (plist-get item :link)))
	      "</guid>\n"
	      (when (and (plist-member rss-plist :content)
			 (not (string-empty-p (plist-get rss-plist :content))))
		"<description>\n<![CDATA[%s]]>\n</description>\n"
		(plist-get item :content))
	      "</item>\n")))
     "</channel>\n</rss>\n")))

(provide 'rss)

;;; rss.el ends here
