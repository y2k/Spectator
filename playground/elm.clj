

(defn component [db msg]
  db)

(defn parent-component [db msg]
  (case (:action msg)
    :left (assoc db :left (fn [db]
                            (component db (:value msg))))
    :right (assoc db :right (fn [db]
                              (component db (:value msg))))
    db))
