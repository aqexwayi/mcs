(ns mcs.dp
  (:import [javax.swing.table DefaultTableModel AbstractTableModel])
  )

(def data-point-tables {:AI (atom [])
                        :AO (atom [])
                        :DI (atom [])
                        :DO (atom [])})

(def column-names ["变量名称" "功能块号"])

(defn add-data-point! [table-id data-point block-id]
  (let [table (table-id data-point-tables)]
    (if (some #(= data-point (first %)) @table)
      nil
      (swap! table conj [data-point block-id]))))

(defn delete-data-point! [table-id data-point]
  (let [table (table-id data-point-tables)]
    (reset! table
            (vec (filter #(not= data-point (first %)) @table)))))

(defn delete-data-points! [table-id dps]
  (doseq [dp dps] (delete-data-point! table-id dp)))

(defn build-data-point-table-model [table]
  (proxy [AbstractTableModel] []
    (getRowCount []
      (count @table))
    (getColumnCount []
      (count column-names))
    (getColumnName [c]
      (nth column-names c))
    (getColumnClass [c]
      (class java.lang.String))
    (getValueAt [r c]
      (let [len (count @table)]
        (if (< r len)
          (nth (nth @table r) c)
          "")))))

(def AI-table-model
  (build-data-point-table-model (:AI data-point-tables)))

(def AO-table-model
  (build-data-point-table-model (:AO data-point-tables)))

(def DI-table-model
  (build-data-point-table-model (:DI data-point-tables)))

(def DO-table-model
  (build-data-point-table-model (:DO data-point-tables)))

(defn table2map [table]
  (into {} (for [item table] [(first item) (second item)])))
