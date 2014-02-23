(ert-deftest org-pandoc-test-export ()
  (save-excursion
    (find-file "test.org")
    (loop
     for format in '(asciidoc beamer context docbook docx
                              dzslides epub epub3 fb2 html html5
                              json latex man markdown
                              markdown_github markdown_mmd
                              markdown_phpextra markdown_strict
                              mediawiki native odt opendocument
                              opml org plain revealjs rst rtf s5
                              slideous slidy texinfo textile)
     do
     (let ((out-file (format "test%s" (org-pandoc-output-file-extension format))))
       (should-not (string= out-file "test.org"))
       (if (not (string= out-file "test.org"))
           (progn
             (org-pandoc-export-to-file format)
             (should (file-exists-p out-file))
             (delete-file out-file)))))))

(ert-deftest org-pandoc-test-publish ()
  (org-publish 
      '("test-project"
        :base-directory "."
        :publishing-directory "/tmp/publish-test"
        :pandoc-output-format html5
        :publishing-function org-pandoc-publish-to)
      t)
  (should (file-exists-p "/tmp/publish-test/test.html5")))