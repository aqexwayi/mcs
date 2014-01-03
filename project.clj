(defproject mcs "0.7.9"
  :description "micro-block control system"
  :url "http://micro-block-control-system.com/"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.nrepl "0.2.2"]
                 [seesaw "1.4.4"]
                 [net.sf.jgrapht/jgrapht "0.8.3"]
                 [com.novemberain/monger "1.4.2"]
                 [clatrix "0.3.0"]
                 [rhizome "0.2.0"]
                 ]
  :omit-source true
  :aot [mcs.ui]
  :main mcs.ui
  )


