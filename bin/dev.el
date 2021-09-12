(defun :release-notes (version &optional changelog-path)
  (with-temp-buffer
    (insert-file-contents (or changelog-path "CHANGELOG.md"))
    (re-search-forward (format "## .*%s.*" version))
    (forward-line)
    (beginning-of-line)
    (let ((start (point)))
      (re-search-forward "^##")
      (buffer-substring-no-properties start (1- (match-beginning 0))))))
