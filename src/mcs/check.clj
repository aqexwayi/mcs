(ns mcs.check 
  (:require [mcs.dp :as dp])
  (:require [mcs.blocks :as bs])
  (:require [mcs.blockclass :as bc]))

(defn check-dangling-link [blocks]
  (let [block-ids (mapcat bs/get-block-ids blocks)
        ai (map second @(:AI dp/data-point-tables))
        di (map second @(:DI dp/data-point-tables))
        ids (set (concat block-ids ai di))
        dangling-link-para? (fn [para]
                              (and (:link para) (nil? (ids (:link-block-id para)))))
        dangling-link-block? (fn [block]
                               (not (empty? (filter dangling-link-para? (:inputs block)))))
        ]
    (filter dangling-link-block? blocks)))

(def check-passes 
  {"输入连接悬空" check-dangling-link
   })

(defn check [blocks]
  (filter coll?
          (for [[name f] check-passes]
            (let [error-blocks (f blocks)]
              (if (empty? error-blocks)
                nil
                [name (map :block-id error-blocks)])))))
