(define-advice org-hugo--gen-front-matter (:around (f data &rest args) inject-weight)
  "Add weight to front matter in a way that preserve org's tree ordering."
  (setf (map-elt data 'weight)
        (line-number-at-pos nil :absolute))
  (apply f data args))
