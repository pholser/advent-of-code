(defn foo [map-of-lists]
  (if
    (empty? map-of-lists)
    '()
    (concat
      (map first (vals map-of-lists))
      (foo
        (into
          (sorted-map)
          (filter
            (fn [[k vs]] ((complement empty?) vs))
            (map
              (fn [[k vs]] [k (rest vs)])
              map-of-lists)))))))
