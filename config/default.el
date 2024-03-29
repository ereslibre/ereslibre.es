(require 'ox-html)
(require 'ox-publish)
(require 'ob-dot)

(setq org-confirm-babel-evaluate nil)
(add-to-list 'org-src-lang-modes (quote ("dot" . graphviz-dot)))

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

(eval-after-load "ox-html"
  '(defun org-html-example-block (example-block _contents info)
     (let ((attributes (org-export-read-attribute :attr_html example-block)))
       (if (plist-get attributes :textarea)
           (org-html--textarea-block example-block)
         (format "<div class=\"org-example-container\"><pre class=\"example\"%s>\n%s</pre></div>"
                 (let* ((name (org-element-property :name example-block))
                        (a (org-html--make-attribute-string
                            (if (or (not name) (plist-member attributes :id))
                                attributes
                              (plist-put attributes :id name)))))
                   (if (org-string-nw-p a) (concat " " a) ""))
                 (org-html-format-code example-block info))))))

(eval-after-load "ox-html"
  '(defun org-html-src-block (src-block _contents info)
  (if (org-export-read-attribute :attr_html src-block :textarea)
      (org-html--textarea-block src-block)
    (let* ((lang (org-element-property :language src-block))
	  (code (org-html-format-code src-block info))
	  (label (let ((lbl (and (org-element-property :name src-block)
				 (org-export-get-reference src-block info))))
		   (if lbl (format " id=\"%s\"" lbl) "")))
	  (klipsify  (and  (plist-get info :html-klipsify-src)
                           (member lang '("javascript" "js"
					  "ruby" "scheme" "clojure" "php" "html")))))
      (if (not lang) (format "<div class=\"org-src-container\"><pre class=\"example\"%s>\n%s</pre></div>" label code)
	(format "<div class=\"org-src-container\">\n%s%s\n</div>"
		;; Build caption.
		(let ((caption (org-export-get-caption src-block)))
		  (if (not caption) ""
		    (let ((listing-number
			   (format
			    "<span class=\"listing-number\">%s </span>"
			    (format
			     (org-html--translate "Listing %d:" info)
			     (org-export-get-ordinal
			      src-block info nil #'org-html--has-caption-p)))))
		      (format "<label class=\"org-src-name\">%s%s</label>"
			      listing-number
			      (org-trim (org-export-data caption info))))))
		;; Contents.
		(if klipsify
		    (format "<pre><code class=\"src src-%s\"%s%s>%s</code></pre>"
			    lang
			    label
			    (if (string= lang "html")
				" data-editor-type=\"html\""
			      "")
			    code)
		  (format "<pre class=\"src src-%s\"%s>%s</pre>"
                          lang label code))))))))

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
                     ;; transcode :file switch on dot src blocks
                     (save-excursion
                       (while (re-search-forward "dot :file\\s-+\\([^\\s-]+\\)" nil t)
                         (let* ((match (match-string 1))
                                (element (save-match-data (org-element-at-point))))
                             (replace-match
                              (concat "dot :file "
                                       (file-relative-name
                                        (expand-file-name match source-file-dir)
                                        "content"))))))
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
         (format "#+options: title:nil\n
                  #+begin_export html\n
                  <div class=\"content container front-container\">
                    <div class=\"side-by-side\">
                      <h1 class=\"post-title\">Blog</h1><hr/>
                      %s
                    </div>
                    <div class=\"side-by-side\">
                      <h1 class=\"post-title\">Notes</h1><hr/>
                      %s
                    </div>
                  </div>\n
                  #+end_export"
                 (ereslibre/all-entries 'blog list)
                 (ereslibre/all-entries 'notes list))))

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

(defun ereslibre/hashed-href (file)
  (let ((hash (with-temp-buffer
               (insert-file-contents file)
               (secure-hash 'sha1 (current-buffer)))))
  (concat "/" file "?h=" hash)))

(defun ereslibre/link-stylesheet (stylesheet)
  (format "<link rel=\"stylesheet\" type=\"text/css\" defer href=\"%s\" />\n" stylesheet))

(defun ereslibre/html-head-extra ()
  (concat (ereslibre/link-stylesheet (ereslibre/hashed-href "assets/css/poole.css"))
          (ereslibre/link-stylesheet (ereslibre/hashed-href "assets/css/syntax.css"))
          (ereslibre/link-stylesheet (ereslibre/hashed-href "assets/css/hyde.css"))
          (ereslibre/link-stylesheet (ereslibre/hashed-href "assets/css/sidebar.css"))
          (ereslibre/link-stylesheet (ereslibre/hashed-href "assets/css/global.css"))
          (ereslibre/link-stylesheet (ereslibre/hashed-href "assets/css/fontawesome.css"))
          (ereslibre/link-stylesheet "//fonts.googleapis.com/css?family=PT+Sans%3A400%2C400italic%2C700%7CAbril+Fatface")))

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
         :html-head-extra ,(ereslibre/html-head-extra)
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
         :base-extension "png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|pub"
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
