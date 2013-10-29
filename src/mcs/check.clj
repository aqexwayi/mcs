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

(defn check [blocks]
  (filter coll?
          (for [[name f] check-passes]
            (let [error-blocks (f blocks)]
              (if (empty? error-blocks)
                nil
                [name (map :block-id error-blocks)])))))
