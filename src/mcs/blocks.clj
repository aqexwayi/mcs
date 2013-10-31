(ns mcs.blocks
  (:require [mcs.blockclass :as bc])
  (:require [mcs.util :as util])
  (:import [javax.swing.table DefaultTableModel AbstractTableModel])
  (:import [org.jgrapht.graph SimpleDirectedGraph DefaultEdge])
  (:import [org.jgrapht.traverse TopologicalOrderIterator])
  )

;; block structure :
;; {:block-id block-id,
;;  :block-type block-type,
;;  :block-desc block-desc,
;;  :inputs para }

(def blocks (atom []))

(def block-table-column-names ["功能块号" "功能块类型" "功能块描述"])

(def block-table-model
  (proxy [AbstractTableModel] []
    (getRowCount []
      (count @blocks))
    (getColumnCount []
      (count block-table-column-names))
    (getColumnName [c]
      (nth block-table-column-names c))
    (getColumnClass [c]
      (class java.lang.String))
    (getValueAt [r c]
      (let [len (count @blocks)]
        (if (< r len)
          (case c
            0 (:block-id (nth @blocks r))
            1 (:block-type (nth @blocks r))
            2 (:block-desc (nth @blocks r))
            "")
          "")))))

(defn parameter-using-block-id? [para id]
  (and (:link para) (= (:link-block-id para) id)))

(defn block-using-block-id? [block id]
  (let [ps (:inputs block)]
    (not (empty? (filter #(parameter-using-block-id? % id) ps)))))

(defn find-blocks-using [id bs]
  (filter #(block-using-block-id? % id) bs))

(defn blocks-before [b bs]
  (filter #(block-using-block-id? b (:block-id %)) bs))

(defn sort-by-id [bs]
  (sort-by #(Integer/parseInt (:block-id %)) bs))

(defn sort-by-topology [bs]
  (let [g (SimpleDirectedGraph. (Class/forName "org.jgrapht.graph.DefaultEdge"))]
    (do
      (doseq [b bs]
        (.addVertex g (:block-id b)))
      (doseq [b bs
              bb (blocks-before b bs)]
        (.addEdge g (:block-id bb) (:block-id b)))
      (let [iter (TopologicalOrderIterator. g)]
        (loop [result []]
          (if (.hasNext iter)
            (recur (conj result (.next iter)))
            (map #(bc/find-block-by-id % bs) result))))
      )))

(defn build-graph [bs]
  (into {} 
        (for [b bs] 
          [(:block-id b) (mapv :block-id (blocks-before b bs))])))

(defn get-available-block-id [bs]
  (if (empty? bs)
    "1000"
    (let [b (apply max-key #(Integer/parseInt (:block-id %)) bs)
          bc (bc/block-class-from-type-name (:block-type b))
          max-id (Integer/parseInt (:block-id b))
          new-id (+ max-id (count (:outputs bc)))]
      (str (* 10 (inc (quot new-id 10)))))))

(defn set-value-by-default-value [parameter]
  (let [dv (:default parameter)
        col (get parameter :change-online true)]
    (assoc parameter :value dv :change-online col)))

(defn use-value-instead-of-link [parameter]
  (if (nil? (:link parameter))
    (assoc parameter :link false :link-block-id "0")
    parameter))

(defn clear-array-parameter [paras]
  (filter #(nil? (:array-size %)) paras))

(defn copy-array-parameter [paras block-class]
  (concat paras (filter #(:array-size %) (:inputs block-class))))

(defn expand-array-parameter-for [ctx parameter]
  (if (= :array (:type parameter))
    (let [array-size-var-name (:array-size parameter)
          np (first (filter #(= (:array-size parameter) (:name %)) ctx))
          len (:value np)
          ety (:element-type parameter)
          p (assoc parameter
              :type ety
              :value (:default parameter)
              :change-online true
              :link false
              :link-block-id "0")
          f (fn [i] (update-in p [:name] #(str % (inc i))))]
      (map f (range len)))
    [parameter]))

(defn expand-array-parameter [paras]
  (mapcat #(expand-array-parameter-for paras %) paras))

(defn new-block [block-id block-type block-desc]
  (let [nb {:block-id block-id
            :block-type block-type
            :block-desc block-desc}
        bc (bc/block-class-from-type-name block-type)
        ps  (:inputs bc)
        ps3 (map set-value-by-default-value ps)
        ps4 (map use-value-instead-of-link ps3)
        ps5 (expand-array-parameter ps4) ]
    (assoc nb :inputs ps5)))

(defn try-to-add-block [block-id block-type block-desc]
  (if (bc/find-block-by-id block-id @blocks)
    false
    (let [b (new-block block-id block-type block-desc)
          bs (sort-by-id (conj @blocks b))]
      (reset! blocks bs)
      true)))

(defn delete-blocks [block-ids]
  (let [bs (filter #(not (some #{(:block-id %)} block-ids)) @blocks)]
    (reset! blocks (into [] bs))))

(def current-block (atom nil))

(defn select-current-block [id]
  (if-let [b (bc/find-block-by-id id @blocks)]
    (reset! current-block b)))

(defn get-block-row [id]
  (util/first-index-of #(= (:block-id %) id) @blocks))

(def block-parameter-table-column-names
  ["参数名称" "参数描述" "参数类型" "参数设定值" "使用外部连线" "外部块号" "在线修改"])

(def block-parameter-table-model
  (proxy [AbstractTableModel] []
    (getRowCount []
      (count (:inputs @current-block)))
    (getColumnCount []
      (count block-parameter-table-column-names))
    (getColumnName [c]
      (nth block-parameter-table-column-names c))
    (getColumnClass [c]
      (class java.lang.String))
    (getValueAt [r c]
      (if-let [b @current-block]
        (let [bi (nth (:inputs @current-block) r)]
          (case c
            0 (str (:name bi))
            1 (str (:desc bi))
            2 (str (:type bi))
            3 (str (:value bi))
            4 (str (:link bi))
            5 (str (:link-block-id bi))
            6 (str (:change-online bi))
            nil))
        ""))))

(defn replace-block [nb bs]
  (mapv (fn [b] (if (= (:block-id b) (:block-id nb)) nb b)) bs))

(defn replace-parameter [np ps]
  (mapv (fn [p] (if (= (:name p) (:name np)) np p)) ps))

(defn handle-parameter-used-as-array-size [nps bc]
  (let [nps2 (clear-array-parameter nps)
        nps3 (copy-array-parameter nps2 bc)
        nps4 (expand-array-parameter nps3)]
    nps4))

;;
;; { name
;;     [condition-f(para) 
;;              [vaule-check-f(para pvalue) 
;;               link-check-f(para plinkid)]]
;;  ... }
;;
(def parameter-check-passes 
  [;; check-boolean-value
   [#(= :bool (:type %))
    [(fn [p v] 
       (let [uv (clojure.string/upper-case v)]
         (or (= uv "TRUE") (= uv "FALSE"))))
     (constantly true)
     ]]
   ;; check-vector-value
   [#(= :vector (:type %))
    [(fn [p v]
       (util/double-vector? (read-string v)))
     (constantly true)]]
   ;; check-array-size-value
   [:used-as-array-size    
    [(fn [p v] 
       (<= 2 (Integer/parseInt v) 10))
     (constantly false)]]
   ;; check-real-value-range
   ;; if real value is not in valid format , throw exception.
   [#(= :real (:type %))
    [(fn [p v]
       (let [min-v (get p :min-value -1e9)
             max-v (get p :max-value 1e9)]
         (<= min-v (Double/parseDouble v) max-v)))
     (constantly true)]]
   ])

(defn valid-parameter-value? [para pvalue plink plinkid]
  (try
    (every? identity (map (fn [[fp [fv fl]]]
                            (if (fp para)
                              (if plink 
                                (fl para plinkid)
                                (fv para pvalue))
                              true)) 
                          parameter-check-passes))
    (catch Exception e 
      false)))

;; pvalue maybe "1.0" "false"
(defn change-parameter-of-current-block! [para pvalue plink plinkid]
  (let [b @current-block
        bc (bc/block-class-from-type-name (:block-type b))
        np (assoc para 
             :value (read-string pvalue)
             :link plink
             :link-block-id plinkid)
        nps (replace-parameter np (:inputs b))
        nps2 (if (:used-as-array-size np)
               (handle-parameter-used-as-array-size nps bc)
               nps)
        nb (assoc b :inputs nps2)
        nbs (replace-block nb @blocks) ]
    (reset! current-block nb)
    (reset! blocks nbs)
    ))

(defn change-link-id-for-parameter [para old-id new-id]
  (if (= (:link-block-id para) old-id) 
    (assoc para :link-block-id new-id)
    para))

(defn change-link-id-for-block [b old-id new-id]
  (let [ps (:inputs b)
        new-ps (mapv #(change-link-id-for-parameter % old-id new-id) ps)]
    (assoc b :inputs new-ps)))

(defn change-link-id-for-blocks [bs old-id new-id]
  (mapv #(change-link-id-for-block % old-id new-id) bs))

(defn change-link-id-for-block-ex [bs n old-id new-id]
  (let [oid (str (+ n (Integer/parseInt old-id)))
        nid (str (+ n (Integer/parseInt new-id)))]
    (change-link-id-for-blocks bs oid nid)))

(defn change-link-ids-for-blocks [bs n old-id new-id]
  (reduce #(change-link-id-for-block-ex %1 %2 old-id new-id) 
          bs 
          (range n)))

(def layout-x [70 170 220 370 420 520])

(def block-x (nth layout-x 2))
(def block-y 60)
(def block-w (- (nth layout-x 3) (nth layout-x 2)))
(def block-h (atom 200))

(def input-w (- (nth layout-x 1) (nth layout-x 0)))
(def input-h 32)

(def output-w (- (nth layout-x 5) (nth layout-x 4)))
(def output-h 32)

(def input-y (atom nil))
(def output-y (atom nil))

(defn layout-current-block []
  (if-let [b @current-block]
    (let [bc (bc/block-class-from-type-name (:block-type b))
          ic (count (:inputs b)) 
          oc (count (:outputs bc))
          y0 block-y
          h (+ 200 (* ic 20))
          y1 (- (+ y0 h) 5)
          input-dy (int (/ h (inc ic)))
          output-dy (int (/ h (inc oc)))
          ]
      (reset! input-y (rest (range y0 y1 input-dy)))
      (reset! output-y (rest (range y0 y1 output-dy)))
      (reset! block-h h)
      )
    (do 
      (reset! input-y nil)
      (reset! output-y nil))))

(defn change-current-block-id-and-desc! [block-id block-desc]
  (let [b @current-block
        bs @blocks
        nb (assoc b :block-id block-id :block-desc block-desc)]
    (if-let [b2 (bc/find-block-by-id block-id bs)]
      (if (= block-id (:block-id b))
        (let [bs @blocks
              bs2 (filter #(not= (:block-id b) (:block-id %)) bs)
              bs3 (vec (sort-by-id (conj bs2 nb)))]
          (do (reset! current-block nb)
              (reset! blocks bs3)
              true))
        false)
      (let [bs @blocks
            bs2 (filter #(not= (:block-id b) (:block-id %)) bs)
            bs3 (vec (sort-by-id (conj bs2 nb)))
            bc (bc/block-class-from-type-name (:block-type b))
            n (count (:outputs bc))
            bs4 (change-link-ids-for-blocks bs3 n (:block-id b) block-id)]
        (do (reset! current-block nb)
            (reset! blocks bs4)
            true)))))

(defn get-block-ids [block]
  (let [bc (bc/block-class-from-type-name (:block-type block))
        bid0 (Integer/parseInt (:block-id block))
        n (count (:outputs bc))]
    (map str (range bid0 (+ bid0 n)))))

(defn get-block-outputs-type [block]
  (let [bc (bc/block-class-from-type-name (:block-type block))
        os (:outputs bc)]
    (zipmap (get-block-ids block) os)))


