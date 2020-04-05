(define-advice org-hugo--gen-front-matter (:around (f data &rest args) inject-weight)
  "Add weight to front matter in a way that preserve org's tree ordering."
  (let ((weight (line-number-at-pos nil :absolute)))
    (map-put data 'weight weight)
    (apply f data args)))
