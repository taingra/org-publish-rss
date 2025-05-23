;;; org-publish-rss.el --- Generate RSS for org publish -*- lexical-binding: t -*-

;; Copyright (C) 2023-2025 Thomas Ingram
;; Copyright (C)      2025 Thibaut Meyer
;; Copyright (C) 2006-2022 Free Software Foundation, Inc.
;; Copyright (C) 2013-2015 Bastien Guerry

;; This package reuses some code from ox-publish.el and ox-rss.el.

;; Author: Thomas Ingram <thomas@taingram.org>
;; Version: 0.8
;; Homepage: https://git.sr.ht/~taingram/org-publish-rss
;; Keywords: org, publishing, rss
;; Package-Requires: ((emacs "28.1"))

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

;; After setting up `:auto-rss' in your `org-publish-project-alist'
;; RSS can be manually generated by invoking org-publish-project:
;;
;;   M-x org-publish-rss [RET] project [RET]
;;
;; This will generate an RSS file in the base directory of your
;; project.  To publish this to your website add it to your
;; attachments list.
;;
;; This function can be added as a `':completion-function' to run
;; automatically whenever a publishing action is performed.  A
;; separate project will need to upload the file using
;; `org-publish-attachment'.
;;
;; Properties added to `org-publish-project-alist' for RSS generation:
;;
;;   `:auto-rss'  (required)
;;
;;    Enables RSS feed generation.
;;
;;   `:rss-file'
;;
;;   Generated RSS file's filename (defaults to "rss.xml").
;;
;;   `:rss-root-url'  (recommended)
;;
;;   Project root URL where files will reside on your website.  If
;;   your project is published to example.com/blog/some-post.html then
;;   this would be "https://example.com/blog".  If blank falls back to
;;   `:html-link-up', `:html-link-home', then `:rss-link'.  If your
;;   RSS generates with broken links try this setting.
;;
;;   `:rss-title'  (required by RSS spec)
;;
;;   Channel title of the RSS feed (will use `:sitemap-title' if
;;   blank).
;;
;;   `:rss-description'  (required by RSS spec)
;;
;;   Channel description of the RSS feed.
;;
;;   `:rss-link'  (required by RSS spec)
;;
;;   The URL to the webpage corresponding to the RSS channel (will use
;;   `:html-link-home' if not set).
;;
;;   `:rss-image'
;;
;;   RSS image, either a complete hyper link or a link relative from
;;   the project directory.  Max size: width 144 pixels, height 400
;;   pixels.
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
;;   `:rss-copyright'
;;
;;   Copyright notice for content in the channel.
;;
;;   `:rss-guid'
;;
;;   Method used to set the global unique identifiers (GUID) tag for
;;   items in the RSS feed.  The published file's permalink (URL) is
;;   used by default.
;;
;;   - permalink (default)    - the file's published URL is used as GUID.
;;   - org-file-id-get-create - use an ID specific to the org file as GUID.
;;   - function               - a function that takes a filename and returns an
;;                              ID.
;;
;;   When permalinks are used any changes to the filename or URL will
;;   cause the item to appear as new in feed readers.  To avoid that,
;;   you can specify a custom function to provide a unique file ID.
;;   See the provided org-file-id.el for a simple solution.
;;
;;   `:rss-with-content'
;;
;;   Include file content as exported HTML in RSS feed.
;;    - nil - include no content (default).
;;    - top - export with content before first heading.
;;    - all - export all file content.
;;
;;   `:rss-filter-function'
;;
;;   Function used to filter files from the RSS feed.  It takes the
;;   absolute filename of the file being published as an argument and
;;   should return t if the file should be included in the feed.
;;

;;;; Example Configuration:

;; (require 'ox-publish)
;; (require 'org-publish-rss)
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
;;          :completion-function org-publish-rss)
;;         ("static"
;;          :base-directory "~/Documents/org/website/"
;;          :base-extension "css\\|jpg\\|png\||gif\\|xml"
;;          :publishing-directory "~/public_html/"
;;          :publishing-function org-publish-attachment)
;;         ("example.com" :components ("content" "static"))))

;; When publishing use: org-publish-current-project (C-c C-e P p)
;; which will automatically check for the meta project with :components
;; and will ensure all related project files are published.

;;; Code:

(require 'ox-publish)
(require 'ox-html)

(defconst org-publish-rss-version "0.8")

(defgroup org-publish-rss nil
  "Org publish with automatic RSS Feed."
  :tag "Org publish RSS"
  :group 'org-export-publish)

(defcustom org-publish-rss-webmaster nil
  "Default webmaster email to be included in exported RSS.
Recommended format: john@example.com (John Smith)"
  :type 'string)

(defcustom org-publish-rss-editor nil
  "Default editor email to be included in exported RSS.
Recommended format: john@example.com (John Smith)"
  :type 'string)

(defcustom org-publish-rss-copyright nil
  "Default copyright text to be included in exported RSS."
  :type 'string)

(defcustom org-publish-rss-indent-xml nil
  "Indent final exported RSS XML file."
  :type 'boolean)

(defcustom org-publish-rss-publish-immediately nil
  "When non-nil generate RSS file in `:publishing-directory'.
By default the RSS file is created in the project's `:base-directory'."
  :type 'boolean)

(defcustom org-publish-rss-with-content nil
  "Include file content in exported RSS feed."
  :type '(choice
	  (const :tag "Include all file content" all)
	  (const :tag "Include content before first heading" top)
	  (const :tag "Do not include content" nil)))

(defcustom org-publish-rss-read-more-text "Read more..."
  "Text to be displayed when file content is truncated in RSS Feed."
  :type 'string)

(defcustom org-publish-rss-guid-method 'permalink
  "Default behaviour used to generate a GUID key from am Org file.
If set to a function, it must accept a filename and return a unique ID
string."
  :type '(choice
	  (const    :tag "Use page's URL as GUID" permalink)
	  (const    :tag "Use the Org file's ID as GUID" org-file-id-get-create)
	  (function :tag "Custom GUID generation function")))

(defun org-publish-rss--get-base-files (project)
  "Return base files for a PROJECT.

Exclude the `:auto-sitemap' and `:makeindex' files."
  (let* ((base-dir (file-name-as-directory
		    (org-publish-property :base-directory project)))
	 (filter-fn
	  (or (org-publish-property :rss-filter-function project) (lambda (_fn) t)))
	 (base-files (seq-filter filter-fn (org-publish-get-base-files project))))
    (when (org-publish-property :auto-sitemap project)
      (setq base-files
	    (remove (expand-file-name
		     (or (org-publish-property :sitemap-filename project)
			 "sitemap.org")
		     base-dir)
		    base-files)))
    (when (org-publish-property :makeindex project)
      (setq base-files
	    (remove (expand-file-name "theindex.org" base-dir) base-files)))
    base-files))

(defun org-publish-rss--file-to-html (file base-url &optional top-only)
  "Generate HTML content for an Org FILE.
BASE-URL used to convert relative links into absolute links.

If TOP-ONLY is non-nil export only the top level text before the first
heading."
  (let ((default-directory (or (file-name-directory file) default-directory))
	(with-footnotes org-export-with-footnotes))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (when (re-search-forward "^#\\+HTML_LINK_HOME:" (point-max) t)
	(delete-region (line-beginning-position) (line-end-position))
	(goto-char (point-min)))
      (insert "#+HTML_LINK_HOME: " base-url "\n")
      (when top-only
	(setq org-export-with-footnotes nil)
	(org-next-visible-heading 1)
	(narrow-to-region (point-min) (point))
	(insert "\n" org-publish-rss-read-more-text "\n"))
      (org-export-as 'html nil nil t
		     `(:with-toc nil
		       :with-footnotes ,with-footnotes
		       :html-link-use-abs-url t)))))


(defun org-publish-rss--builder (project)
  "Generate RSS feed XML for a PROJECT."
  (let ((title
	 (or (org-publish-property :rss-title     project)
	     (org-publish-property :sitemap-title project)
	     (concat (car project) " RSS feed")))
	(link
	 (or (org-publish-property :rss-link       project)
	     (org-publish-property :html-link-home project)))
	(description (org-publish-property :rss-description project))
	(language
	 (or (org-publish-property :language project)
	     org-export-default-language))
	(webmaster
	 (or
	  (org-publish-property :rss-webmaster project)
	  org-publish-rss-webmaster))
	(editor
	 (or
	  (org-publish-property :rss-editor project)
	  org-publish-rss-editor))
	(copyright
	 (or
	  (org-publish-property :rss-copyright project)
	  org-publish-rss-copyright))
	(image (org-publish-property :rss-image project))
	(url
	 (string-trim-right
	  (or (org-publish-property :rss-root-url   project)
	      (org-publish-property :html-link-up   project)
	      (org-publish-property :html-link-home project)
	      (org-publish-property :rss-link       project))
	  "/"))
	(rss-file
	 (or (org-publish-property :rss-file project)
	     "rss.xml"))
	(base-files (org-publish-rss--get-base-files project))
	(base-dir (file-name-as-directory
		   (org-publish-property :base-directory project)))
	(guid-method
	 (or (org-publish-property :rss-guid project)
	     org-publish-rss-guid-method))
	(with-content
	 (or (org-publish-property :rss-with-content project)
	     org-publish-rss-with-content))
	(items-xml ""))
    (unless (and title link description)
      (error "RSS spec requires a title, link, and description"))
    (when (eq guid-method 'org-file-id-get-create)
      (require 'org-file-id))
    (concat
     (format
      "<?xml version=\"1.0\" encoding=\"%s\"?>
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
<atom:link href=\"%s\" rel=\"self\" type=\"application/rss+xml\" />
<title>%s</title>
<link>%s</link>
<description><![CDATA[%s]]></description>
<language>%s</language>
<lastBuildDate>%s</lastBuildDate>
<generator>Emacs %s org-publish-rss.el %s</generator>\n"
      (symbol-name org-html-coding-system)
      (concat url "/" rss-file)
      title
      link
      description
      language
      (format-time-string "%a, %d %b %Y %H:%M:%S %z")
      emacs-version
      org-publish-rss-version)
     (when webmaster
       (format "<webMaster>%s</webMaster>\n" webmaster))
     (when editor
       (format "<managingEditor>%s</managingEditor>\n" editor))
     (when copyright
       (format "<copyright>%s</copyright>" copyright))
     (when image
       (format
	"<image>\n<url>%s</url>\n<title>%s</title>\n<link>%s</link>\n</image>\n"
	image title link))
     ;; According to the RSS spec order does not matter so we
     ;; do not need to waste effort here sorting items.
     (dolist (file base-files items-xml)
       (let* ((file-url
	       (concat url "/" (file-name-sans-extension
				(file-relative-name file base-dir))
		       (if (string-equal "org" (file-name-extension file))
			   ".html"
			 (file-name-extension file t))))
	      (file-base-dir
	       (file-relative-name
		(file-name-directory file) base-dir))
	      (file-base-dir-url
	       (concat url "/"
		       (unless (string-equal file-base-dir "./")
			 file-base-dir)))
	      (guid
	       (pcase guid-method
		 ('permalink file-url)
		 ((pred functionp)
		  (funcall guid-method file))
		 (_ (error "Invalid GUID generation method.")))))
	 (setq items-xml
	       (concat items-xml
		       "<item>\n"
		       (format
			"<title>%s</title>\n<link>%s</link>\n<pubDate>%s</pubDate>\n<guid%s>%s</guid>\n"
			(org-publish-find-title file project)
			file-url
			(format-time-string "%a, %d %b %Y %H:%M:%S %z"
					    (org-publish-find-date file project))
			(if (eq guid-method 'permalink)
			    "" " isPermaLink=\"false\"")
			guid)
		       (when with-content
			 (format "<description>\n<![CDATA[%s]]>\n</description>"
			  (org-publish-rss--file-to-html
			   file file-base-dir-url (eq with-content 'top))))
		       "</item>\n"))))
     "</channel>\n"
     "</rss>")))

;;;###autoload
(defun org-publish-rss (project)
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
      (let ((rss (org-publish-rss--builder project))
	    (filename
	     (expand-file-name
	      (or (org-publish-property :rss-file project)
		  "rss.xml")
	      (if org-publish-rss-publish-immediately
		  (org-publish-property :publishing-directory project)
		(file-name-as-directory
		 (org-publish-property :base-directory project))))))
	(with-temp-buffer
	  (insert rss)
	  (when org-publish-rss-indent-xml
	    (xml-mode)
	    (indent-region (point-min) (point-max)))
	  (write-file filename))))))

(defun org-publish-rss-projects (projects)
  "Generate RSS feeds for given PROJECTS."
  (dolist (project (org-publish-expand-projects projects))
    (when (org-publish-property :auto-rss project)
      (org-publish-rss project))))

(provide 'org-publish-rss)

;;; org-publish-rss.el ends here
