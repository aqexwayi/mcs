(ns mcs.mac
  (:require [mcs.util :as util])
  (:import [java.net InetAddress NetworkInterface]))

(def valid-mac-address-list
  (list "68-A8-6D-37-FB-3A"
        "FC-4D-D4-32-7B-49"
        "00-1E-65-42-0F-7E"
        "00-1E-65-3A-10-06"
        "00-27-10-36-09-08"
        "90-2B-34-A7-9B-DC"
        "60-EB-69-54-C9-79"
        "C0-CB-38-36-CC-D9"
        "00-24-E8-98-9D-7E"
        "00-22-5F-8B-67-30"
        "3C-97-0E-76-5F-5C"
        "60-6C-66-2D-75-30"
        "00-1B-B9-D9-E3-F0"
        "00-1B-B9-D8-90-47"
        "00-26-5E-2C-20-E2" ;; alp home 
        "8C-89-A5-20-A9-42"
        "84-3A-4B-7E-84-94"
        "3C-97-0E-75-6D-54"
        "00-0B-AB-74-FA-90" ;; Q8400
        "FC-4D-D4-32-75-6D" 
        "9C-4E-36-18-D3-28"
        "00-2C-CC-CA-FC-BD"
        "00-0B-AB-74-FA-90"
        "00-05-5D-0F-40-E0" ;; DFE530Tx
        ))

(defn -valid-mac-address? []
  (let [ia (InetAddress/getLocalHost)
        ni (NetworkInterface/getByInetAddress ia)
        mac (.getHardwareAddress ni)
        machex (map #(format "%02X" %) mac)
        macstr (clojure.string/join "-" machex)
        ]
    (some #{macstr} valid-mac-address-list)))

(defn valid-mac-address? []
  (if (util/is-os-windows?)
    (-valid-mac-address?)
    true
    ))
