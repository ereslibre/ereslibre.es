(setq make-backup-files nil)
(defun load-directory (dir)
  (let ((load-it (lambda (f)
                   (load-file (concat (file-name-as-directory dir) f)))))
    (mapc load-it (directory-files dir nil "\\.el$"))))

(load-directory (concat (getenv "PWD") "/config"))
(org-publish-all t)
