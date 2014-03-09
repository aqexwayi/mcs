(ns mcs.check 
  (:require [mcs.dp :as dp])
  (:require [mcs.blocks :as bs])
  (:require [mcs.blockclass :as bc]))

(defn check-dangling-link [blocks]
  (let [block-ids (mapcat bs/get-block-ids blocks)
        ai (map second @(:AI dp/data-point-tables))
        di (map second @(:DI dp/data-point-tables))
        ids (set (concat block-ids ai di))
        error-para? (fn [para]
                      (if (:link para) 
                        (nil? (ids (:link-block-id para)))
                        false))
        error-block? (fn [block]
                       (not (empty? (filter error-para?
                                            (:inputs block)))))
        ]
    (filter error-block? blocks)))

(defn check-link-type [blocks]
  (let [ty-map (apply merge (map bs/get-block-outputs-type blocks))
        ai-map (into {} (for [[n i] @(:AI dp/data-point-tables)]
                          [i :real]))
        di-map (into {} (for [[n i] @(:DI dp/data-point-tables)]
                          [i :bool]))
        m (merge ty-map ai-map di-map)
        error-para? (fn [para]
                      (if (:link para)
                        (not= (:type para) 
                              (get m (:link-block-id para) false))
                        false))
        error-block? (fn [block]
                       (not (empty? (filter error-para?
                                            (:inputs block)))))
        ]
    (filter error-block? blocks)))

(def check-passes 
  {"输入连接悬空" check-dangling-link
   "输入连接类型错误" check-link-type
   })

(defn check-blocks [blocks]
  (let [rs (for [[name f] check-passes]
                        (let [error-blocks (f blocks)]
                          (if (empty? error-blocks)
                            nil
                            [name (map :block-id error-blocks)])))
        r (first (filter coll? rs))]
    (if (nil? r)
      "" ;; OK 
      (str (first r) ":" (second r)))))

(defn check-ao-link-type [blocks]
  (let [ty-map (apply merge (map bs/get-block-outputs-type blocks))
        ai-map (into {} (for [[n i] @(:AI dp/data-point-tables)]
                          [i :real]))
        aos (for [[n i] @(:AO dp/data-point-tables)]
              [i n])
        m (merge ty-map ai-map)
        error-ao? (fn [[ao-idx ao-name]]
                    (not= :real (get m ao-idx)))
        ]
    (filter error-ao? aos)))

(defn check-do-link-type [blocks]
  (let [ty-map (apply merge (map bs/get-block-outputs-type blocks))
        di-map (into {} (for [[n i] @(:DI dp/data-point-tables)]
                          [i :bool]))
        dos (for [[n i] @(:DO dp/data-point-tables)]
              [i n])
        m (merge ty-map di-map)
        error-do? (fn [[do-idx do-name]]
                    (not= :bool (get m do-idx)))
        ]
    (filter error-do? dos)))

(defn check-dp [blocks]
  (let [r (check-ao-link-type blocks)]
    (if (empty? r)
      (let [r (check-do-link-type blocks)]
        (if (empty? r)
          "" ;; OK
          (str "开关量数据输出类型错误:" (first r))))
      (str "模拟量数据输出类型错误:" (first r)))))
