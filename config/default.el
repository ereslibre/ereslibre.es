(require 'ox-html)
(require 'ox-publish)

(eval-after-load "ox-html"
  '(defun org-html-template (contents info)
     (concat (org-html-doctype info)
             "<html lang=\"en\">
                <head>"
                  (org-html--build-meta-info info)
                  (org-html--build-head info)
                  (org-html--build-mathjax-config info)
               "</head>
                <body>"
                  (ereslibre/pre-postamble 'preamble info)
                  (ereslibre/page-contents contents info)
                  (ereslibre/pre-postamble 'postamble info)
               "</body>
              </html>")))

(eval-after-load "ox-rss"
  '(defun org-rss-final-function (contents _backend _info)
     "Do nothing.

The original implementation used this function to indent the
XML contents, but indenting will make `<pre>' blocks inside
`CDATA' blocks be reindented."
     contents))

(defun ereslibre/pre-postamble (type info)
  (with-temp-buffer
    (insert-file-contents (plist-get info (intern (format ":html-%s" type))))
    (buffer-string)))

(defun ereslibre/has-title (info)
  (and (plist-get info :with-title)
       (plist-get info :title)))

(defun ereslibre/page-contents (contents info)
  (let ((has-title (ereslibre/has-title info)))
    (concat (if has-title
                (ereslibre/page-title-and-date info))
            contents
            (if has-title
                (ereslibre/page-title-and-date-closer info)))))

(defun ereslibre/page-title-and-date (info)
   (when (ereslibre/has-title info)
     (let ((title (plist-get info :title))
           (date (plist-get info :date))
           (subtitle (plist-get info :subtitle)))
       (when title
         (let ((contents (format "<div class=\"content container\">
                                    <div class=\"post\">
                                      <h1 class=\"post-title\">%s</h1>"
                                 (org-export-data title info))))
         (if date
           (format (concat contents
                           "<span class=\"post-date\">%s</span>")
                   (org-timestamp-format (car date) "%Y-%m-%d"))
           (concat contents
                   "<span class=\"post-date\">&nbsp;</span>")))))))

(defun ereslibre/page-title-and-date-closer (info)
  "  </div><!-- post -->
   </div><!-- content -->")

(defun ereslibre/is-entry-of-type (type entry)
  (let ((entry-link (plist-get (car entry) :entry)))
    (string-match (format "^%s/" type) entry-link)))

(defun ereslibre/all-entries (type list)
  (let ((entries (seq-filter (apply-partially #'ereslibre/is-entry-of-type type) (cdr list))))
    (if (null entries)
        "<p>No entries yet</p>"
        (concat "<div class=\"posts\">"
                (mapconcat (lambda (entry)
                             (format "%s" (plist-get (car entry) :content)))
                           entries
                           "")
                "</div>"))))

(defun ereslibre/website-project ()
  (assoc "website-content" org-publish-project-alist))

(defun ereslibre/rss-project ()
  (assoc "website-rss" org-publish-project-alist))

(defun ereslibre/rss-entry (entry)
  (let* ((entry (plist-get (car entry) :entry))
         (title (org-publish-find-title entry (ereslibre/website-project)))
         (date (org-publish-find-date entry (ereslibre/website-project)))
         (link (concat (file-name-sans-extension entry) ".html"))
         (source-file (concat (file-name-as-directory "content") entry))
         (source-file-dir (file-name-directory source-file))
         (home-url-prefix (plist-get (cdr (ereslibre/rss-project)) :html-link-home))
         (contents (with-temp-buffer
                     (org-mode)
                     (insert-file-contents source-file)
                     (beginning-of-buffer)
                     ;; demote all headlines
                     (save-excursion
                       (while (re-search-forward "^\\*" nil t)
                         (replace-match "**")))
                     ;; remove certain attributes from inserted org file
                     (save-excursion
                       (while (re-search-forward "^#\\+\\(title\\|date\\).*" nil t)
                         (replace-match "")))
                     ;; transcode embedded links to files -- e.g. expand relative paths
                     (save-excursion
                       (while (re-search-forward "\\[file:\\([^]]+\\)" nil t)
                         (let* ((match (match-string 1))
                                (element (save-match-data (org-element-at-point))))
                           (when (not (or (eq (org-element-type element) 'example-block)
                                          (eq (org-element-type element) 'src-block)))
                             (replace-match
                              (concat "[" home-url-prefix
                                      (file-name-sans-extension
                                       (file-relative-name
                                        (expand-file-name match source-file-dir)
                                        "content"))
                                      ".html"))))))
                     (buffer-string))))
    (with-temp-buffer
      (insert (format "* [[file:%s][%s]]\n" (ereslibre/path-relative-from-to-relative-to entry "content" "content/blog") title))
      (org-set-property "RSS_PERMALINK" link)
      (org-set-property "PUBDATE" (format-time-string "%Y-%m-%d %H:%M" date))
      (insert contents)
      (buffer-string))))

(defun ereslibre/path-relative-from-to-relative-to (path from to)
  (let* ((from-absolute (expand-file-name path from))
         (to-relative (file-relative-name from-absolute to)))
    to-relative))

(defun ereslibre/generate-org-rss-feed (list)
  (let ((blog-entries (seq-filter (apply-partially #'ereslibre/is-entry-of-type 'blog) (cdr list))))
    (let* ((rss-contents (mapconcat #'ereslibre/rss-entry blog-entries "\n\n"))
           (full-rss-contents (concat "#+title: ereslibre.es\n\n" rss-contents)))
      (write-region full-rss-contents nil "./content/blog/feed.org"))))

(defun ereslibre/sitemap (title list)
  (progn (ereslibre/generate-org-rss-feed list)
         (format "#+OPTIONS: title:nil\n
                  #+BEGIN_EXPORT html\n
                  <div class=\"content container front-container\">
                    <div class=\"side-by-side\">
                      <h1 class=\"post-title\">Notes</h1><hr/>
                      %s
                    </div>
                    <div class=\"side-by-side\">
                      <h1 class=\"post-title\">Blog</h1><hr/>
                      %s
                    </div>
                  </div>\n
                  #+END_EXPORT"
                 (ereslibre/all-entries 'notes list)
                 (ereslibre/all-entries 'blog list))))

(defun ereslibre/sitemap-format-entry (entry style project)
  (let ((date (ereslibre/org-publish-find-explicit-date entry project)))
    `(:content ,(format "<div class=\"post-preview\">
                            <h2 class=\"post-title\">%s</h2>
                            <span class=\"post-date\">%s</span>
                         </div>"
                        (org-export-string-as (format "[[file:%s][%s]]" entry (org-publish-find-title entry project)) 'html t)
                        (if date
                            (format-time-string "%Y-%m-%d" date)
                          "&nbsp;"))
      :entry ,entry)))

(defun ereslibre/org-publish-find-explicit-date (file project)
  "Find the date of FILE in PROJECT.
This function assumes FILE is either a directory or an Org file.
If FILE is an Org file and provides a DATE keyword use it.  In
any other case return nil.  Return
time in `current-time' format."
  (let ((file (org-publish--expand-file-name file project)))
    (if (file-directory-p file) (nth 5 (file-attributes file))
      (let ((date (org-publish-find-property file :date project)))
	;; DATE is a secondary string.  If it contains a time-stamp,
	;; convert it to internal format.
	(cond ((let ((ts (and (consp date) (assq 'timestamp date))))
		 (and ts
		      (let ((value (org-element-interpret-data ts)))
			(and (org-string-nw-p value)
			     (org-time-string-to-time value)))))))))))

(setq org-rss-use-entry-url-as-guid t)
(setq org-publish-project-alist
      `(("website"
         :components ("website-content" "website-assets" "website-rss"))
        ("website-content"
         :base-directory "./content/"
         :base-extension "org"
         :publishing-directory "./public_html/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :auto-sitemap t
         :sitemap-sort-files anti-chronologically
         :sitemap-function ereslibre/sitemap
         :sitemap-format-entry ereslibre/sitemap-format-entry
         :sitemap-style list
         :sitemap-filename "index.org"
         :html-head-include-scripts nil
         :html-head-include-default-style nil
         :export-with-tags nil
         :headline-levels 4
         :table-of-contents nil
         :section-numbers nil
         :sub-superscript nil
         :todo-keywords nil
         :author nil
         :creator-info nil
         :html-preamble ,(concat (getenv "PWD") "/templates/preamble.html")
         :html-postamble ,(concat (getenv "PWD") "/templates/postamble.html")
         :html-doctype "html5"
         :html-html5-fancy t
         :html-link-use-abs-url t
         :htmlized-source t
         :title "ereslibre.es"
         :with-toc nil
         :with-title t
         :with-date t
         :html-head nil
         :html-head-extra "
           <link rel=\"stylesheet\" type=\"text/css\" href=\"/assets/css/poole.css?v=0\" />
           <link rel=\"stylesheet\" type=\"text/css\" href=\"/assets/css/syntax.css?v=0\" />
           <link rel=\"stylesheet\" type=\"text/css\" href=\"/assets/css/hyde.css?v=0\" />
           <link rel=\"stylesheet\" type=\"text/css\" href=\"/assets/css/sidebar.css?v=0\" />
           <link rel=\"stylesheet\" type=\"text/css\" href=\"/assets/css/global.css?v=1\" />
           <link rel=\"stylesheet\" href=\"/assets/css/fontawesome.css?v=0\" />
           <link rel=\"stylesheet\" href=\"//fonts.googleapis.com/css?family=PT+Sans%3A400%2C400italic%2C700%7CAbril+Fatface\" />"
         :style nil
         :timestamp t
         :exclude "blog/feed.org"
         :exclude-tags ("noexport" "todo"))
        ("website-assets"
         :base-directory "./assets/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|otf\\|svg\\|eot\\|ttf\\|woff\\|woff2"
         :publishing-directory "./public_html/assets/"
         :recursive t
         :publishing-function org-publish-attachment)
        ("website-content-assets"
         :base-directory "./content/"
         :base-extension "png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg"
         :publishing-directory "./public_html/"
         :recursive t
         :publishing-function org-publish-attachment)
        ("website-rss"
         :base-directory "./content/"
         :base-extension "org"
         :html-link-home "https://www.ereslibre.es/"
         :html-link-use-abs-url t
         :htmlized-source t
         :rss-feed-url "https://www.ereslibre.es/blog/feed.xml"
         :rss-image-url "http://s.gravatar.com/avatar/bdc4bd9b9b18388588ed2273adaee8a6?s=128"
         :rss-extension "xml"
         :section-numbers nil
         :publishing-directory "./public_html/"
         :exclude ".*"
         :include ("blog/feed.org")
         :sitemap-sort-files 'anti-chronologically
         :sitemap-style 'list
         :author "Rafael Fernández López"
         :email "ereslibre@ereslibre.es"
         :description "Libre Software lover. Hacker."
         :publishing-function org-rss-publish-to-rss)))
