(ns mcs.ui
  (:require [mcs.blockclass :as bc])
  (:require [mcs.blocks :as bs])
  (:require [mcs.db :as db])
  (:require [mcs.dp :as dp])
  (:require [mcs.check :as check])
  (:require [mcs.sim :as sim])
  (:require [mcs.util :as util])
  (:require [mcs.mac :as mac])
  (:use [seesaw core])
  (:use [rhizome dot])
  (:require [seesaw action font table border forms mouse])
  (:require [clojure.java.io])
  (:require [clojure.java.shell])
  (:import [java.io File])
  (:import [java.awt Color Dimension])
  (:import [javax.swing JComponent UIManager JFileChooser])
  (:import [javax.swing.filechooser FileFilter FileNameExtensionFilter])
  (:import [java.util Date])
  (:import [java.util.concurrent Executors TimeUnit])
  (:import [java.net InetAddress NetworkInterface])
  (:gen-class))

(def scada-config (atom {:host "localhost"
                         :port 27017
                         :db-name "scada"}))

(native!) ;; must call it very early 

(def resource-root-directory
  "file:/D:/clojure-projects/be/resources/")

(defn resource* [r]
  (or (clojure.java.io/resource r)
      (str resource-root-directory r)))

(def main-panel)
(def main-frame)
(def main-frame-title "华电万通组态软件")

(defn center-component! 
  "This centers any component.
   If only c (the component) is supplied, it centers c on the monitor.
   If parent component is provided as well, it should center c over parent component"
  ([c]
     (.setLocationRelativeTo c nil)
     c)
  ([c parent]
     (.setLocationRelativeTo c parent)
     c))

(defn center-dialog! [d]
  (.setLocationRelativeTo d main-frame)
  d)

(defn add-block-dlg []
  (let [items ["功能块号" (text :id :block-id 
                                :text (bs/get-available-block-id @bs/blocks))
               (seesaw.forms/next-line)
               "功能块类型" (combobox :id :block-type 
                                      :model (map :type-name bc/block-classes))
               (seesaw.forms/next-line)
               "功能块描述" (text :id :block-desc
                                  :text "用户功能块描述信息")]]
    (->
     (dialog :id :add-block-dlg
             :title "添加功能块"
             :success-fn (fn [p]
                           [(text (select (to-frame p) [:#block-id]))
                            (selection (select (to-frame p) [:#block-type]))
                            (text (select (to-frame p) [:#block-desc]))])
             :cancel-fn (fn [p] nil)
             :option-type :ok-cancel
             :content (seesaw.forms/forms-panel "right:pref,10dlu,120dlu,10dlu,pref"
                                                :items items))
     pack!
     center-dialog!
     show!)))

(defn action-add-block [e]
  (if (sim/simulation-running?)
    (alert main-frame "仿真运行中不能增加功能块")
    (let [[block-id block-type block-desc] (add-block-dlg)]
      (if (nil? block-id)
        (alert main-frame "取消添加功能块。")
        (if (bs/try-to-add-block block-id block-type block-desc)
          (let [table (select main-panel [:#block-table])]
            (bs/select-current-block block-id)
            (selection! table (bs/get-block-row block-id))
            (.invalidate table)
            (repaint! main-panel))
          (alert main-frame "由于参数不正确无法添加功能块！"))))
    ))

(defn action-delete-block [e]
  (if (sim/simulation-running?)
    (alert main-frame "仿真运行中不能删除功能块")
    (if (-> (dialog :option-type :ok-cancel
                    :content "你确定要删除选中的功能块吗？")
            pack!
            center-dialog!
            show!)
      (let [table (select main-panel [:#block-table])
            rows (selection table {:multi? true})]
        (if (nil? rows)
          (alert main-frame "功能块无法删除！")
          (let [ids (doall (map #(.getValueAt table % 0) rows))]
            (do
              (println "delete blocks:" ids)
              (bs/delete-blocks ids)
              (selection! table nil)
              (reset! bs/current-block nil)
              (repaint! main-panel))))))))

(defn change-block-dlg []
  (let [b @bs/current-block 
        items ["功能块号" (text :id :block-id :text (:block-id b))
               (seesaw.forms/next-line)
               "功能块描述" (text :id :block-desc :text (:block-desc b))]]
    (->
     (dialog :id :change-block-dlg
             :title "修改功能块号"
             :success-fn (fn [p]
                           [(text (select (to-frame p) [:#block-id]))
                            (text (select (to-frame p) [:#block-desc]))])
             :cancel-fn (fn [p] nil)
             :option-type :ok-cancel
             :content (seesaw.forms/forms-panel "right:pref,10dlu,120dlu,10dlu,pref"
                                                :items items))
     pack!
     center-dialog!
     show!)))

(defn action-change-block [e]
  (if (sim/simulation-running?)
    (alert main-frame "仿真运行中不能修改功能块信息")
    (let [table (select main-panel [:#block-table])
          rows (selection table {:multi? true})]
      (if (nil? rows)
        (alert main-frame "未选中功能块！")
        (let [[block-id block-desc] (change-block-dlg)]
          (if (nil? block-id)
            (alert main-frame "取消修改！")
            (if (bs/change-current-block-id-and-desc! block-id block-desc)
              (repaint! main-panel)
              (alert main-frame "块号不正确，无法修改！"))))))))

(defn on-select-block [e]
  (let [table (select main-panel [:#block-table])
        s (selection table)]
    (if s 
      (do
        (bs/select-current-block (.getValueAt table s 0))
        (repaint! main-panel)))))

(defn input-parameter-dlg [para]
  (let [cv (str (:value para))
        items ["参数名称" (:name para) (seesaw.forms/next-line)
               "参数类型" (:type para) (seesaw.forms/next-line)
               "参数值" (if (= (:type para) :bool)
                          (-> (combobox :id :value :model ["true" "false"])
                              (selection! cv))
                          (text :id :value :text cv))
               (seesaw.forms/next-line)
               (checkbox :id :link :selected? (:link para)) "使用外部连线"
               (seesaw.forms/next-line)
               "外部连线编号" (text :id :link-block-id :text (:link-block-id para))
               ]]
    (->
     (dialog :id :input-parameter-dlg
             :title "修改功能块参数"
             :success-fn (fn [p]
                           [(text (select (to-frame p) [:#value]))
                            (selection (select (to-frame p) [:#link]))
                            (text (select (to-frame p) [:#link-block-id]))])
             :cancel-fn (fn [p] nil)
             :option-type :ok-cancel
             :content (seesaw.forms/forms-panel 
                       "right:pref,10dlu,120dlu,10dlu,pref"
                       :items items))
     pack!
     center-dialog!
     show!)))

(defn edit-block-parameter [e]
  (let [table (select main-panel [:#block-parameter-table])
        row (selection table) ]
    (if row
      (let [para-name (.getValueAt table row 0)
            para (first (filter #(= para-name (:name %))
                                (:inputs @bs/current-block)))
            ret (input-parameter-dlg para)
            [pv pl plid] (mapv util/trim+ ret)
            ]
        (if (nil? pv)
          (alert main-frame "取消参数修改！")
          (if (bs/valid-parameter-value? para pv pl plid)
            (try
              (if (and (sim/simulation-running?) 
                       (not (get para :change-online false)))
                (alert main-frame "不能在线修改参数！")
                (do
                  (bs/change-parameter-of-current-block! para pv pl plid)
                  (repaint! main-panel)))
              (catch Exception e 
                (alert main-frame "不正确的参数！")))
            (alert main-frame "不正确的参数！")))))))

(def link-blocks (atom []))

(defn clear-link-blocks []
  (reset! link-blocks []))

(defn add-link-block [x y w h block-id]
  (swap! link-blocks conj [x y w h block-id]))

(defn point-in-rect? [pt rect]
  (let [[x0 y0] pt
        [x y w h] rect]
    (and (< x x0 (+ x w)) (< y y0 (+ y h)))))

(defn find-link-block-id [pt2d]
  (let [x0 (.getX pt2d)
        y0 (.getY pt2d)
        lbs @link-blocks]
    (last (first (filter #(point-in-rect? [x0 y0] %) lbs)))))

(defn draw-link-block [g x y idx]
  (add-link-block x (- y 12) 100 24 idx)
  (doto g
    (.setColor Color/black)
    (.drawRoundRect x (- y 12) 100 24 6 6)
    (.setColor Color/blue)
    (.setFont (.deriveFont (.getFont g) (float 12)))
    (.drawString (str idx) (+ x 20) (+ y 6))))

(defn draw-block [g b]
  (doto g
    (.setColor Color/black)
    (.drawRoundRect bs/block-x bs/block-y bs/block-w @bs/block-h 10 10)
    (.setFont (.deriveFont (.getFont g) (float 20)))
    (.drawString (:block-type b) bs/block-x 30)
    (.drawString (:block-id b) bs/block-x 50)))

(defn draw-input [g p y]
  (let [x (nth bs/layout-x 0)]
    (do 
      (doto g
        (.setColor Color/black)
        (.drawLine (nth bs/layout-x 1) y (nth bs/layout-x 2) y))
      (if (:link p)
        (draw-link-block g x y (:link-block-id p))
        (doto g
          (.setColor Color/blue)
          (.setFont (.deriveFont (.getFont g) (float 12)))
          (.drawString (str (:value p)) (+ x 20) y)
          )))))

(defn draw-inputs [g ps]
  (doseq [[idx para] (map list (range 32) ps)]
    (do 
      (doto g
        (.setColor Color/blue)
        (.setFont (.deriveFont (.getFont g) (float 12)))
        (.drawString (:name para) (+ bs/block-x 16) (nth @bs/input-y idx))
        )
      (draw-input g para (nth @bs/input-y idx)))
    ))

(defn draw-outputs [g b]
  (doseq [[idx y] (map list (range 32) @bs/output-y)]
    (let [rid (+ idx (Integer/parseInt (:block-id b)))]
      (do
        (let [obs (bs/find-blocks-using (str rid) @bs/blocks)]
          (doseq [[ob-idx ob] (map list (range 32) obs)]
            (draw-link-block g 
                             (nth bs/layout-x 4) (+ y (* ob-idx 24)) 
                             (:block-id ob))))
        (doto g
          (.setColor Color/black)
          (.drawLine (nth bs/layout-x 3) y (nth bs/layout-x 4) y)
          )
        (if (sim/simulation-running?)
          (let [v (sim/get-current-block-value (str rid))]
            (doto g
              (.drawString (util/format-value v) 
                           (+ 16 bs/output-w (nth bs/layout-x 4)) y))))
        ))))

(def visual-block-component
  (canvas
   :paint (fn [c g]
            (if-let [b @bs/current-block]
              (do 
                (bs/layout-current-block)
                (clear-link-blocks)
                (draw-block g b)
                (draw-inputs g (:inputs b))
                (draw-outputs g b))))))

(listen visual-block-component
        :mouse-clicked
        (fn [e]
          (if (= 2 (.getClickCount e))
            (if-let [lbid (find-link-block-id (.getPoint e))]
              (let [lbid-str (str lbid) 
                    table (select main-panel [:#block-table])
                    r (util/first-index-of #(= lbid-str (:block-id %))
                                           @bs/blocks)]
                (bs/select-current-block lbid-str)
                (selection! table r)
                (repaint! main-panel))))))

(def block-table-panel
  (let [tbl (table :id :block-table
                   :show-grid? true 
                   :model bs/block-table-model
                   :listen [:selection on-select-block])
        _ (.setReorderingAllowed (.getTableHeader tbl) false)]
    (border-panel
     :center (scrollable tbl)
     :south (flow-panel
             :items [(button :text "添加"
                             :listen [:action action-add-block])
                     (button :text "删除"
                             :listen [:action action-delete-block])
                     (button :text "修改"
                             :listen [:action action-change-block])
                     ]
             :align :right)
     :vgap 5 :hgap 5 :border 5)))

(defn add-data-point-dlg []
  (let [items ["数据点名称" (text :id :data-point-name :text "数据点名称")
               (seesaw.forms/next-line)
               "功能块块号" (text :id :block-id :text "功能块块号")]]
    (->
     (dialog :id :add-data-point-dlg
             :title "添加数据点"
             :success-fn (fn [p]
                           [(text (select (to-frame p) [:#data-point-name]))
                            (text (select (to-frame p) [:#block-id]))])
             :cancel-fn (fn [p] nil)
             :option-type :ok-cancel
             :content (seesaw.forms/forms-panel "right:pref,10dlu,pref,10dlu,pref"
                                                :items items))
     pack!
     center-dialog!
     show!)))

(defn action-add-data-point [table-id]
  (let [[data-point block-id] (add-data-point-dlg)]
    (if (nil? data-point)
      (alert main-frame "取消添加数据点")
      (if (dp/add-data-point! table-id data-point block-id)
        (repaint! main-panel)
        (alert main-frame "由于参数不正确无法添加数据点")))))

(defn action-delete-data-point [table-id table-name]
  (if (-> (dialog :option-type :ok-cancel
                  :content "你确定要删除选中的数据点吗？")
          pack!
          center-dialog!
          show!)
    (let [table (select main-panel [table-name])
          rows (selection table {:multi? true})]
      (if (nil? rows)
        (alert main-frame "数据点无法删除！")
        (let [dps (doall (map #(.getValueAt table % 0) rows))]
          (do
            (println "delete-data-point:" dps)
            (dp/delete-data-points! table-id dps)
            (selection! table nil)
            (repaint! main-panel)))))))

(defn action-add-AI [e]
  (action-add-data-point :AI))

(defn action-delete-AI [e]
  (action-delete-data-point :AI :#AI-table))

(defn action-add-AO [e]
  (action-add-data-point :AO))

(defn action-delete-AO [e]
  (action-delete-data-point :AO :#AO-table))

(defn action-add-DI [e]
  (action-add-data-point :DI))

(defn action-delete-DI [e]
  (action-delete-data-point :DI :#DI-table))

(defn action-add-DO [e]
  (action-add-data-point :DO))

(defn action-delete-DO [e]
  (action-delete-data-point :DO :#DO-table))

(defn make-data-point-table-panel [model table-id add-f delete-f]
  (let [tbl (table :id table-id
                   :show-grid? true 
                   :model model)
        _ (.setReorderingAllowed (.getTableHeader tbl) false)]
    (border-panel
     :center (scrollable tbl)
     :south (flow-panel
             :items [(button :text "添加"
                             :listen [:action add-f])
                     (button :text "删除"
                             :listen [:action delete-f])]
             :align :right)
     :vgap 5 :hgap 5 :border 5)))

(def AI-table-panel
  (make-data-point-table-panel dp/AI-table-model
                               :AI-table
                               action-add-AI action-delete-AI))

(def AO-table-panel
  (make-data-point-table-panel dp/AO-table-model
                               :AO-table
                               action-add-AO action-delete-AO))

(def DI-table-panel
  (make-data-point-table-panel dp/DI-table-model
                               :DI-table
                               action-add-DI action-delete-DI))

(def DO-table-panel
  (make-data-point-table-panel dp/DO-table-model
                               :DO-table
                               action-add-DO action-delete-DO))

(def data-bus-panel
  (tabbed-panel :placement :top
                :tabs [{:title "模拟量输入" :content AI-table-panel}
                       {:title "模拟量输出" :content AO-table-panel}
                       {:title "开关量输入" :content DI-table-panel}
                       {:title "开关量输出" :content DO-table-panel}]))

(def block-parameter-table-panel
  (let [tbl (table :id :block-parameter-table
                   :show-grid? true
                   :model bs/block-parameter-table-model)
        _ (.setReorderingAllowed (.getTableHeader tbl) false)]
    (border-panel
     :center (scrollable tbl)
     :south (flow-panel
             :items [(button :text "编辑"
                             :listen [:action edit-block-parameter])]
             :align :right))))

(def main-panel
  (left-right-split
   (tabbed-panel :placement :top
                 :tabs [ {:title "功能块表" :content block-table-panel}
                         {:title "数据总线" :content data-bus-panel}
                         ])
   (top-bottom-split block-parameter-table-panel
                     visual-block-component 
                     :divider-location 1/3)
   :divider-location 1/3))

(def mcs-file-chooser
  (let [fc (JFileChooser.)
        _ (.setMultiSelectionEnabled fc false)
        ff (FileNameExtensionFilter. "MCS工程文件" (into-array ["mcs"]))
        _ (.setFileFilter fc ff)]
    fc))

(defn mcs-choose-file [act]
  (let [rv (case act
             :save (.showSaveDialog mcs-file-chooser main-panel)
             :open (.showOpenDialog mcs-file-chooser main-panel)
             nil)]
    (if (= rv JFileChooser/APPROVE_OPTION)
      (.getSelectedFile mcs-file-chooser))))

(defn new-project! [e]
  (if (-> (dialog :option-type :ok-cancel
                  :content "你确定要放弃当前工程，建立新工程吗？")
          pack!
          center-dialog!
          show!)
    (let [table (select main-panel [:#block-table])]
      (selection! table nil)
      (reset! bs/current-block nil)
      (reset! bs/blocks [])
      (reset! bs/current-block nil)
      (reset! (:AI dp/data-point-tables) [])
      (reset! (:AO dp/data-point-tables) [])
      (reset! (:DI dp/data-point-tables) [])
      (reset! (:DO dp/data-point-tables) [])
      (repaint! main-panel))))

(defn load-project! [e]
  (if (sim/simulation-running?)
    (alert main-frame "组态系统已经在运行！")
    (if-let [f (mcs-choose-file :open)]
      (let [s (slurp f :encoding "utf-8")
            [cfg blks ait aot dit dot] (read-string s)] 
        (let [table (select main-panel [:#block-table])]
          (selection! table nil)
          (reset! bs/current-block nil)
          (reset! bs/blocks blks)
          (reset! scada-config cfg)
          (reset! (:AI dp/data-point-tables) ait)
          (reset! (:AO dp/data-point-tables) aot)
          (reset! (:DI dp/data-point-tables) dit)
          (reset! (:DO dp/data-point-tables) dot)
          (.invalidate table)
          (repaint! main-panel)
          )))))

(defn save-project! [e]
  (if-let [f (mcs-choose-file :save)]
    (let [fn (.getPath f)
          s (str [@scada-config 
                  @bs/blocks 
                  @(:AI dp/data-point-tables)
                  @(:AO dp/data-point-tables)
                  @(:DI dp/data-point-tables)
                  @(:DO dp/data-point-tables)])]
      (if (.endsWith fn ".mcs")
        (spit f s :encoding "utf-8")
        (spit (File. (str fn ".mcs")) s :encoding "utf-8"))
      )))

(def menu-item-scada-start)
(def menu-item-scada-stop)

(defn scada-config-dlg []
  (let [items ["主机地址" (text :id :scada-config-host
                            :text (:host @scada-config))
               (seesaw.forms/next-line)
               "通讯端口" (text :id :scada-config-port
                            :text (str (:port @scada-config)))
               (seesaw.forms/next-line)
               "数据库名" (text :id :scada-config-name
                            :text (:db-name @scada-config))
               (seesaw.forms/next-line)
               "仿真周期" (combobox :id :simulation-interval
                                    :model [1000 500 250]) "ms"]]
    (->
     (dialog :id :scada-config-dlg
             :title "SCADA数据总线配置"
             :success-fn (fn [p]
                           {:host (text (select (to-frame p) [:#scada-config-host]))
                            :port (Integer/parseInt (text (select (to-frame p) 
                                                                  [:#scada-config-port])))
                            :db-name (text (select (to-frame p) [:#scada-config-name]))
                            :interval (let [s (selection (select (to-frame p)
                                                                 [:#simulation-interval]))] 
                                        (/ s 1000.0))
                            })
             :cancel-fn (fn [p] nil)
             :option-type :ok-cancel
             :content (seesaw.forms/forms-panel "right:pref,10dlu,pref,10dlu,pref"
                                                :items items))
     pack!
     center-dialog!
     show!)))

(defn scada-start! [e]
  (if (sim/simulation-running?)
    (alert main-frame "组态系统已经在运行！")
    (if (empty? @bs/blocks)
      (alert main-frame "组态工程为空！")
      (let [r (check/check @bs/blocks)]
        (if (empty? r)
          (if-let [nc (scada-config-dlg)]
            (do
              (reset! scada-config nc)
              (sim/simulation-turn-on! nc)))
          (alert main-frame (str (ffirst r) (nfirst r))))))))

(defn scada-stop! [e]
  (if (sim/simulation-running?)
    (do
      (sim/simulation-turn-off!)
      (alert main-frame "系统仿真停止")
      )))

(defn sort-blocks-by-id! [e]
  (swap! bs/blocks #(into [] (bs/sort-by-id %)))
  (repaint! main-panel))

(defn sort-blocks-by-topology! [e]
  (swap! bs/blocks #(into [] (bs/sort-by-topology %)))
  (repaint! main-panel))

(def menu-item-scada-start
  (seesaw.action/action :name "组态系统运行"
                        :handler scada-start!
                        :enabled? true))

(def menu-item-scada-stop
  (seesaw.action/action :name "组态系统停止"
                        :handler scada-stop!
                        :enabled? false))

(defn show-about-dialog [e]
  (->
   (dialog :content (vertical-panel 
                     :items ["组态仿真软件"
                             "@北京华电万通科技有限公司"
                             "版本：0.7"
                             "2013 HDWT"]
                     )
           :type :info)
   pack!
   center-dialog!
   show!))

(defn- output-svg [g fname]
  (let [f (fn [bid] 
            {:label (str bid "\n" (:block-desc (bc/find-block-by-id bid @bs/blocks))) 
             :shape :box})
        dot (graph->dot (keys g) g :node->descriptor f)
        ]
    (do
      (spit "tmp.dot" dot)
      (clojure.java.shell/sh "dot" "-Tsvg:cairo" (str "-o" fname) "tmp.dot"))))

(defn export-svg! [e]
  (if-let [f (mcs-choose-file :save)]
    (let [fname (.getPath f)
          g (bs/build-graph @bs/blocks)]
      (if (.endsWith fname ".svg")
        (output-svg g fname)
        (output-svg g (str fname ".svg"))))))

(def menu-items
  [(menu
    :text "文件  "
    :items [(seesaw.action/action :name "新建工程"
                                  :handler new-project!)
            (seesaw.action/action :name "加载工程"
                                  :handler load-project!)
            (seesaw.action/action :name "保存工程"
                                  :handler save-project!)
            :separator
            (seesaw.action/action :name "导出SVG"
                                  :handler export-svg!)
            :separator
            (seesaw.action/action :name "退出软件"
                                  :handler (fn [e]
                                             (do
                                               (scada-stop! nil)
                                               ;; (.shutdown ui-scheduler)
                                               (dispose! main-panel))))
            ])
   (menu
    :text "运行  "
    :items [menu-item-scada-start
            menu-item-scada-stop ])
   (menu
    :text "查看  "
    :items [(seesaw.action/action :name "拓扑排序"
                                  :handler sort-blocks-by-topology!)
            (seesaw.action/action :name "块号排序"
                                  :handler sort-blocks-by-id!)])
   (menu
    :text "帮助  "
    :items [(seesaw.action/action :name "内容")
            (seesaw.action/action :name "浏览")
            :separator
            (seesaw.action/action :name "关于"
                                  :handler show-about-dialog)
            ])
   ])

(def main-menu (menubar :items menu-items))

(def main-frame
  (frame
   :title main-frame-title
   :minimum-size [1280 :by 800]
   :on-close :exit
   :menubar main-menu
   :content main-panel ))

(defn ui-thread [e]
  (if (sim/simulation-running?)
    (let [c (sim/get-simulation-interval)
          title (str main-frame-title " [ 运行 : 仿真周期=" c "s ]")]
      (config! main-frame :title title)
      (.setEnabled menu-item-scada-start false)
      (.setEnabled menu-item-scada-stop true))
    (let [title (str main-frame-title " [ 停止 ] ")]
      (config! main-frame :title title)
      (.setEnabled menu-item-scada-start true)
      (.setEnabled menu-item-scada-stop false)
      ))
  (repaint! main-panel))

(defn -main [& args]
  (if-not (mac/valid-mac-address?)
    (println "无法运行未经授权的版本")
    (invoke-later 
     (do 
       (-> main-frame pack! show!)
       (timer ui-thread :delay 500)
       (future (sim/simulate scada-stop!))))))




