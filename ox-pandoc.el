;;; ox-pandoc.el --- Org exporter using Pandoc

;; Copyright 2013 Rob Tillotson

;; Author: Rob Tillotson <rob@pyrite.org>
;; Created: 2013-07-29
;; Version: 1.0
;; Package-Requires: ((org "8.0"))

;; This file is not part of GNU Emacs.

;;; License:

;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;; this software and associated documentation files (the "Software"), to deal in
;; the Software without restriction, including without limitation the rights to
;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
;; the Software, and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
;; FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
;; COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; This is a pandoc exporter for Org, built upon Markdown as an
;; intermediate format, and targeted at production of e-books in ePub format.

;;; Code:

(eval-when-compile (require 'cl))
(require 'ox-md)

;;; User Modifiable Variables:

(defgroup org-export-pandoc nil
  "Options specific to Pandoc export back-end."
  :tag "Org Pandoc"
  :group 'org-export
  :version "24.4"
  :package-version '(Org . "8.0"))

(defcustom org-pandoc-command "pandoc"
  "Command to run pandoc."
  :group 'org-export-pandoc
  :type 'string)

(defcustom org-pandoc-extra-options ""
  "Extra pandoc options to use every time.
For example, if you encounter stack overflows, put the options
to expand the stack here."
  :group 'org-export-pandoc
  :type 'string)

(defvar org-pandoc---info nil)

(org-export-define-derived-backend 'pandoc 'md
  :menu-entry
  '(?p "Export with Pandoc"
       ((?P "Markdown to buffer"
            (lambda (a s v b) (org-pandoc-export-as-pandoc a s v)))
        (?a "To asciidoc"
            (lambda (a s v b) (org-pandoc-export-to-file 'asciidoc a s v)))
        (?b "To beamer"
            (lambda (a s v b) (org-pandoc-export-to-file 'beamer a s v)))
        (?c "To context"
            (lambda (a s v b) (org-pandoc-export-to-file 'context a s v)))
        (?d "To docbook"
            (lambda (a s v b) (org-pandoc-export-to-file 'docbook a s v)))
        (?D "To docx"
            (lambda (a s v b) (org-pandoc-export-to-file 'docx a s v)))
        (?\C-d "To dzslides"
             (lambda (a s v b) (org-pandoc-export-to-file 'dzslides a s v)))
        (?e "To epub"
            (lambda (a s v b) (org-pandoc-export-to-file 'epub a s v)))
        (?E "To epub3"
            (lambda (a s v b) (org-pandoc-export-to-file 'epub3 a s v)))
        (?f "To fb2"
             (lambda (a s v b) (org-pandoc-export-to-file 'fb2 a s v)))
        (?h "To html"
            (lambda (a s v b) (org-pandoc-export-to-file 'html a s v)))
        (?5 "To html5"
            (lambda (a s v b) (org-pandoc-export-to-file 'html5 a s v)))
        (?j "To json"
            (lambda (a s v b) (org-pandoc-export-to-file 'json a s v)))
        (?l "To latex"
            (lambda (a s v b) (org-pandoc-export-to-file 'latex a s v)))
        (?m "To man"
            (lambda (a s v b) (org-pandoc-export-to-file 'man a s v)))
        (?p "To markdown"
            (lambda (a s v b) (org-pandoc-export-to-file 'markdown a s v)))
        (?n "To native"
            (lambda (a s v b) (org-pandoc-export-to-file 'native a s v)))
        (?O "To odt"
            (lambda (a s v b) (org-pandoc-export-to-file 'odt a s v)))
        (?\C-o "To opendocument"
            (lambda (a s v b) (org-pandoc-export-to-file 'opendocument a s v)))
        (?M "To opml"
             (lambda (a s v b) (org-pandoc-export-to-file 'opml a s v)))
        (?o "To org"
            (lambda (a s v b) (org-pandoc-export-to-file 'org a s v)))
        (?p "To pdf"
            (lambda (a s v b) (org-pandoc-export-to-file 'pdf* a s v)))
        (?\C-p "To plain"
            (lambda (a s v b) (org-pandoc-export-to-file 'plain a s v)))
        (?\C-r "To revealjs"
             (lambda (a s v b) (org-pandoc-export-to-file 'revealjs a s v)))
        (?r "To rst"
            (lambda (a s v b) (org-pandoc-export-to-file 'rst a s v)))
        (?R "To rtf"
            (lambda (a s v b) (org-pandoc-export-to-file 'rtf a s v)))
        (?s "To s5"
            (lambda (a s v b) (org-pandoc-export-to-file 's5 a s v)))
        (?S "To slideous"
            (lambda (a s v b) (org-pandoc-export-to-file 'slideous a s v)))
        (?\C-s "To slidy"
               (lambda (a s v b) (org-pandoc-export-to-file 'slidy a s v)))
        (?T "To texinfo"
            (lambda (a s v b) (org-pandoc-export-to-file 'texinfo a s v)))
        (?t "To textile"
            (lambda (a s v b) (org-pandoc-export-to-file 'textile a s v)))))
  :translate-alist '((template . org-pandoc-template))
  :options-alist '((:epub-rights "EPUB_RIGHTS" nil nil t)
                   (:epub-cover "EPUB_COVER" nil nil t)
                   (:epub-stylesheet "EPUB_STYLESHEET" nil nil t)
                   (:pandoc-options "PANDOC_OPTIONS" nil nil 'space)
                   ))

;; Some simple XML escaping code.  I'm surprised this isn't
;; already in emacs somewhere...

(defvar org-pandoc-xml-escapes
  '(("&"  . "&amp;")
    ("'"  . "&apos;")
    ("\"" . "&quot;")
    ("<"  . "&lt;")
    (">"  . "&gt;")))

(defun org-pandoc-escape-xml (string)
  (mapc #'(lambda (item)
            (setq string
                  (replace-regexp-in-string (car item) (cdr item) string)))
        org-pandoc-xml-escapes)
  string)

(defun org-pandoc-make-copyright-string (author email)
  (format "Copyright %s %s%s" (format-time-string "%Y") author
          (if email
              (format " <%s>" email)
            "")))

(defun org-pandoc-template (contents info)
  (let ((title (plist-get info :title))
        (author (plist-get info :author))
        (date (org-export-get-date info "%Y-%m-%d")))
    ;; Store info for later reference.
    (setq org-pandoc---info info)
    (concat (format "%% %s\n" (org-export-data title info))
            (when (and (plist-get info :with-author)
                       author)
              (format "%% %s\n" (org-export-data author info)))
            (when (plist-get info :with-date)
              (format "%% %s\n" (org-export-data date info)))
            "\n"
            contents)))

(defun org-pandoc-run-pandoc (filename outfilename output-format &optional options)
  (let* ((args (list "-t" (symbol-name output-format)
                     "-f" "markdown"
                     "-o" outfilename
                     options
                     org-pandoc-extra-options
                     filename))
         (command (concat org-pandoc-command " " (mapconcat 'identity args " "))))
    (message "Running pandoc as: %s" command)
    (message "Ran pandoc: %s" (shell-command-to-string command))))

(defun org-pandoc-make-epub-metadata-file (info)
  (let* ((description (plist-get info :description))
         (author (plist-get info :author))
         (email (plist-get info :email))
         (rights (org-pandoc-escape-xml
                  (or (plist-get info :epub-rights)
                      (org-pandoc-make-copyright-string
                       (org-export-data author info) 
                       (org-export-data email info)))))
        (keywords (plist-get info :keywords))
        (metadata-file (make-temp-file "org-pandoc" nil ".xml")))
    (with-temp-file metadata-file
      (insert (concat
               (if description
                   (format "<dc:description>%s</dc:description>\n"
                           (org-pandoc-escape-xml description)))
               "<dc:rights>"
               rights
               "</dc:rights>\n"
               (if keywords
                 (format "<dc:subject>%s</dc:subject>\n" (org-pandoc-escape-xml keywords))))))
    metadata-file))

(defun org-pandoc-process-function (&optional output-format subtreep ext-plist)
  "Return a function that will process a file in place with
pandoc. OUTPUT-FORMAT is a symbol."
  (lexical-let () ; capture output-format
    (lambda (file)
      (if (and (not (null output-format))
               (not (eq output-format 'markdown)))
          (let* ((info org-pandoc---info)
                 (epub-cover (plist-get info :epub-cover))
                 (epub-stylesheet (plist-get info :epub-stylesheet))
                 (pandoc-options (plist-get info :pandoc-options))
                 (options (concat " -s"
                                  (if (or (eq output-format 'epub)
                                          (eq output-format 'epub3))
                                      (concat 
                                       (if epub-cover
                                           (format " --epub-cover-image=%s" epub-cover))))
                                  (if epub-stylesheet
                                      (format " --epub-stylesheet=%s" epub-stylesheet))
                                  " --epub-metadata=" 
                                  (org-pandoc-make-epub-metadata-file info)
                                  (if pandoc-options
                                      (concat " " pandoc-options)))))
            (org-pandoc-run-pandoc file file output-format options)))
      file)))

(defun org-pandoc-output-file-extension (output-format)
  "Return file extension for OUTPUT-FORMAT."
  (case output-format
    (asciidoc ".asciidoc")
    (beamer ".latex")
    (context ".context")
    (docbook ".db")
    (docx ".docx")
    (dzslides ".dzslides.html")
    (epub ".epub")
    (epub3 ".epub3")
    (fb2 ".fb2")
    (html ".html")
    (html5 ".html5")
    (json ".json")
    (latex ".latex")
    (man ".man")
    (markdown ".md")
    (markdown_github ".github.md")
    (markdown_mmd ".mmd.md")
    (markdown_phpextra ".phpextra.md")
    (markdown_strict ".strict.md")
    (mediawiki ".mediawiki")
    (native ".native")
    (odt ".odt")
    (opendocument ".xml")
    (opml ".opml")
    (org ".pandoc_org")
    (plain ".txt")
    (revealjs ".revealjs.html")
    (rst ".rst")
    (rtf ".rtf")
    (s5 ".s5.html")
    (slideous ".slideous.html")
    (slidy ".slidy.html")
    (texinfo ".texi")
    (textile ".textile")
    (t (error "No format extension found for %s" (symbol-name format)))))

(defun org-pandoc-export-output-file-name (output-format &optional subtreep pub-dir)
  "Makes an output file name for pandoc."
  (org-export-output-file-name
   (org-pandoc-output-file-extension output-format)
   subtreep 
   pub-dir))

(defun org-pandoc-export-to-file
  (&optional output-format filename async subtreep visible-only body-only ext-plist)
  "Export current buffer to file.

If optional argument OUTPUT-FORMAT (a symbol) is given, will
export to that format. Otherwise will export to Pandoc compatible
markdown without post-processing."
  (interactive)
  (org-export-to-file 'pandoc
      (org-pandoc-export-output-file-name output-format subtreep)
    async subtreep visible-only body-only ext-plist
    (org-pandoc-process-function output-format subtreep ext-plist)))

(defun org-pandoc-publish-to (plist filename pub-dir)
  "Publish an org file via pandoc.

Use the :pandoc-output-format propery to specify an output
format, a symbol corresponding with a valid pandoc output format
name. If not specified, markdown will be used."
  (let* ((output-format (plist-get plist :pandoc-output-format))
         (file-ext (org-pandoc-output-file-extension output-format nil pub-dir))
         (process (org-pandoc-process-function output-format)))
    (org-publish-attachment
     plist
     (funcall process (org-publish-org-to 'pandoc filename file-ext plist pub-dir))
     pub-dir)))

(provide 'ox-pandoc)
