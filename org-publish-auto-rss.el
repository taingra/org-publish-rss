;;; org-publish-auto-rss.el --- Generate RSS for org publish -*- lexical-binding: t -*-

;; Copyright (C) 2023-2025 Thomas Ingram
;; Copyright (C) 2006-2022 Free Software Foundation, Inc.
;; Copyright (C) 2013-2015  Bastien Guerry

;; This package reuses some code from ox-publish.el and ox-rss.el.

;; Author: Thomas Ingram <thomas@taingram.org>
;; Version: 0.1
;; Homepage: https://git.sr.ht/~taingram/org-publish-auto-rss
;; Keywords: org, publishing, rss

;; This file is *not* part of GNU Emacs.

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

;; After setting up auto-rss in your org-publish project it can be
;; manually invoked with:
;;
;;   M-x org-publish-auto-rss [RET] project [RET]
;;
;; This will generate an RSS file in the base directory of your
;; project.  To publish this to your website add it to your
;; attachments list.
;;
;; This function can be added as a `':completion-function' to run
;; automatically whenever a publishing action is performed.  Note a
;; separate project will need to upload the file using
;; `org-publish-attachment'.
;;
;; Required `org-publish-project-alist' Properties:
;;
;;   `:html-link-home' (required)
;;
;;    Root website URL (e.g. https://example.com), required to
;;    determine the root URL of all posts.
;;
;; Properties added for `org-publish-auto-rss':
;;
;;   `:auto-rss' (required)
;;
;;    Enables RSS feed generation.
;;
;;   `:rss-file'
;;
;;   Generated RSS file's filename (default rss.xml).
;;
;;   `:rss-title'  (required)
;;
;;   Channel title of the RSS feed (will use `:sitemap-title' if
;;   blank).
;;
;;   `:rss-description'  (required)
;;
;;   Channel description of the RSS feed.
;;
;;   `:rss-link'  (required)
;;
;;   The URL to the webpage corresponding to the RSS channel (will use
;;   `:html-link-up' if blank).  Useful if feed is specific to a
;;   specific sub-section like https://example.com/blog/
;;
;;   `:rss-webmaster'
;;
;;   Email address for person responsible for technical issues related
;;   to the channel.
;;
;;   `:rss-editor'
;;
;;   Email address for person responsible for editorial content.
;;
;;   `:rss-image'
;;
;;   RSS image, either a complete hyper link or a link relative from
;;   the project directory.  Max size: width 144 pixels, height 400
;;   pixels.
;;
;;   `:copyright'
;;
;;   Copyright notice for content in the channel.
;;
;; Example configuration:
;;
;; (require 'ox-publish)
;; (require 'org-publish-auto-rss)
;;
;; (setq org-publish-project-alist
;;       '(("content"
;;          :base-directory "~/Documents/org/website/"
;;          :base-extension "org"
;;          :publishing-directory "~/public_html/"
;;          :publishing-function  org-html-publish-to-html
;;
;;          :html-link-home "https://example.com"
;;
;;          :auto-rss t
;;          :rss-title "Example Posts"
;;          :rss-description "My example website."
;;
;;          :completion-function org-publish-auto-rss)
;;         ("static"
;;          :base-directory "~/Documents/org/website/"
;;          :base-extension "css\\|jpg\\|png\||gif\\|xml"
;;          :publishing-directory "~/public_html/"
;;          :publishing-function org-publish-attachment)
;;         ("example.com" :components ("content" "static"))))

;;; Code:

(require 'ox-publish)

(defvar org-publish-auto-rss-version "0.1")

(defun opar--insert-generate-guid ()
  "Generate GUID tag for an `org-mode' file."
  ;; TODO optionally insert guid tag similar to ox-rss.el
  )

(defun opar--get-base-files (project)
  "Return base files for a PROJECT.

Exclude the `:auto-sitemap' and `:makeindex' files."
  (let* ((base-dir (file-name-as-directory
		    (org-publish-property :base-directory project)))
	 (base-files (org-publish-get-base-files project)))
    (when (org-publish-property :auto-sitemap project)
      (delete (expand-file-name
	       (or (org-publish-property :sitemap-filename project)
		   "sitemap.org")
	       base-dir)
	      base-files))
    (when (org-publish-property :makeindex project)
      (delete (expand-file-name "theindex.org" base-dir) base-files))
    base-files))


(defun opar--rss-builder (project)
  "Generate RSS feed for a PROJECT."
  (let ((title
	 (or (org-publish-property :rss-title     project)
	     (org-publish-property :sitemap-title project)
	     (concat (car project) " RSS feed")))
	(link
	 (or (org-publish-property :rss-link       project)
	     (org-publish-property :html-link-home project)))
	(description (org-publish-property :rss-description project))
	(language (or (org-publish-property :language project)
		      org-export-default-language))
	(webmaster (org-publish-property :rss-webmaster project))
	(editor    (org-publish-property :rss-editor project))
	(image (org-publish-property :rss-image project))
	(url (org-publish-property :html-link-up project))
	(base-files (opar--get-base-files project))
	(base-dir (file-name-as-directory
		   (org-publish-property :base-directory project))))
    (unless (and title link description)
      (error "RSS requires :rss-title, :rss-link and :rss-description"))
    (concat
     (format "<?xml version=\"1.0\" encoding=\"%s\"?>
 <rss version=\"2.0\"
  xmlns:content=\"http://purl.org/rss/1.0/modules/content/\"
  xmlns:wfw=\"http://wellformedweb.org/CommentAPI/\"
  xmlns:dc=\"http://purl.org/dc/elements/1.1/\"
  xmlns:atom=\"http://www.w3.org/2005/Atom\"
  xmlns:sy=\"http://purl.org/rss/1.0/modules/syndication/\"
  xmlns:slash=\"http://purl.org/rss/1.0/modules/slash/\"
  xmlns:georss=\"http://www.georss.org/georss\"
  xmlns:geo=\"http://www.w3.org/2003/01/geo/wgs84_pos#\"
  xmlns:media=\"http://search.yahoo.com/mrss/\">
<channel>
<title>%s</title>
<link>%s</link>
<description><![CDATA[%s]]></description>
<language>%s</language>
<lastBuildDate>%s</lastBuildDate>
<generator>Emacs %s org-publish-auto-rss.el %s</generator>\n"
	     (symbol-name org-html-coding-system)
	     title
	     link
	     description
	     language
	     (format-time-string "%a, %d %b %Y %H:%M:%S %z")
	     emacs-version
	     org-publish-auto-rss-version)
     (when webmaster
       (format "<webMaster>%s</webMaster>\n" webmaster))
     (when editor
       (format "<managingEditor>%s</managingEditor>\n") editor)
     (when image
       (format "<image>
<url>%s</url>
<title>%s</title>
<link>%s</link>
</image>\n"
	image title link))
     ;; According to the RSS spec order does not matter so we
     ;; do not need to waste effort here sorting posts.
     (apply #'concat
	          (mapcar
	           (lambda (entry)
	             (let* ((relpath
			     (string-remove-prefix (expand-file-name base-dir) entry))
		            (post-url
			     (concat url "/" (string-remove-suffix ".org" relpath) ".html")))
		       (format "<item>
<title>%s</title>
<link>%s</link>
<pubDate>%s</pubDate>
<guid>%s</guid>
</item>\n"
			 ; FIXME cache empty error
			 (org-publish-find-title entry project)
			 post-url
			 (format-time-string "%a, %d %b %Y %H:%M:%S %z"
					     (org-publish-find-date entry project))
			 post-url)))
	     base-files))
     "</channel>\n"
     "</rss>")))


(defun org-publish-auto-rss (project)
  "Create RSS feed for an org publish PROJECT.

PROJECT is either a project name, as a string, or a project
alist (see `org-publish-project-alist' variable)."
  (interactive
   (list
    (assoc (completing-read "Generate RSS for project: "
			    org-publish-project-alist nil t)
	   org-publish-project-alist)))
  (let ((project
	 (cond ((stringp project) (assoc project org-publish-project-alist))
	       ((not (stringp (car project))) (push "project" project))
	       (t project))))
    (when (and (not (null project))
	       (org-publish-property :auto-rss project))
      (let ((rss (opar--rss-builder project))
	    (filename
	     (expand-file-name
	      (or (org-publish-property :rss-file project)
		  "rss.xml")
	      (file-name-as-directory
	       (org-publish-property :base-directory project)))))
	(with-temp-buffer
	  (insert rss)
	  (write-file filename))))))


(defun org-publish-auto-rss-projects (projects)
  "Generate RSS feeds for given PROJECTS."
  (dolist (project (org-publish-expand-projects projects))
    (when (org-publish-property :auto-rss project)
      (org-publish-auto-rss project))))


(provide 'org-publish-auto-rss)

;;; org-publish-auto-rss.el ends here
