

(defn component [db msg]
  ...)

(defn parent-component [db msg]
  (cond
    (:id msg)
    (:left (assoc db :left (fn [db]
                             (component db (:value msg)))))
    (:right (assoc db :right (fn [db]
                               (component db (:value msg)))))))
