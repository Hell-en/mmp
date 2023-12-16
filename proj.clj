(ns nocomm
  (:require [clojure.string :as str]
            [clojure.java.io :as io])
  )

(defn first_id [n])
(defn second_id [n])
(defn third_id [n])
(defn walk [lst])
(defn conveyor [filtr_lst])
(defn apply_func [filtr])

(def photoid (atom nil))
(def requestid (atom nil))

(defn read_input []
  (def x (read))
  (let [requestid x] requestid)
  (if (empty? (str/replace x #"[0-9]" ""))
    (first_id x)
    (println "Wrong request ID"))
  (def y (read))
  (let [photoid y] photoid)
  (if (empty? (str/replace (str/replace y #"[0-9]" "") #" " ""))
    (second_id y)
    (println "Wrong photos ID"))
  (def z (read))
  (third_id z)
  )

; id запроса
(defn first_id [n]
  ; (int n)
  (println "Request ID")
  (println "contains only numbers")
  (println "type is " (type n))
  (println "number is " n)
  )

; id фотографий
(defn second_id [n]
  (println "Photos ID")
  (println "contains only numbers")
  (println "type is " (type n))
  (println "number is " n)
  (def photos_ID_amount (+ 1 (count (re-seq #" " n))))
  (println "amount of photos_ID " photos_ID_amount)
  )

; строка фильтров
(defn third_id [n]
  (println "Finding ( ) [ ]")
  (println n)
  (def round_open (- (count n) (count (str/replace n "(" ""))))
  (println "round_open " round_open)
  (def round_close (- (count n) (count (str/replace n ")" ""))))
  (println "round_close " round_close)
  (def square_open (- (count n) (count (str/replace n "[" ""))))
  (println "square_open " square_open)
  (def square_close (- (count n) (count (str/replace n "]" ""))))
  (println "square_close " square_close)
  (if (and (= round_open round_close) (= square_open square_close) (<= round_close 1) (<= square_close 1))
    (conveyor (str/split n #" "))
    (println "Wrong filter request"))
  ; если передает в конце если резка то ".. rezka m" где м - число
  )


(defn noise_reduction_1 [photoid]                           ; default
  (def f_name (slurp (str requestid ".txt")))
  (with-open [wrtr (io/writer f_name :append true)]
    (.write wrtr (str "processed noise_reduction_1 " photoid)))
  photoid
  )
(defn noise_reduction_2 [photoid]
  (def f_name (slurp (str requestid ".txt")))
  (with-open [wrtr (io/writer f_name :append true)]
    (.write wrtr (str "processed noise_reduction_2 " (inc photoid))))
  (inc photoid)
  )
(defn noise_reduction_3 [photoid]
  (def f_name (slurp (str requestid ".txt")))
  (with-open [wrtr (io/writer f_name :append true)]
    (.write wrtr (str "processed noise_reduction_3 " (dec photoid))))
  (dec photoid)
  )
(defn blur [reqid]
  (def f_name (slurp (str reqid ".txt")))
  (with-open [wrtr (io/writer f_name :append true)]
    (.write wrtr "processed blur"))
  )
(defn panorama_stitching [photoid_lst]
  (with-open [wrtr (io/writer (str requestid ".txt"))]               ; write to a new file
    (.write wrtr "panorama_stitching")
    (.write wrtr (for [x photoid_lst] x)))                  ; will work; resulted like '()
  )
(defn HDR_conversion [photoid_lst]
  (use 'clojure.java.io)
  (with-open [wrtr (io/writer (str requestid ".txt"))]               ; write to a new file
    (.write wrtr "HDR_conversion")
    (.write wrtr (for [x photoid_lst] x)))                  ; will work; resulted like '()
  )
(defn choosing_best [photoid]
  (->> photoid
       (noise_reduction_1)
       (noise_reduction_2)
       (noise_reduction_3)
       )
  (cond)
  ;read 3 files, take last elems. compare them
  )

(defn cutting_areas [photoid]
  ; (println "print the number of partition")
  ; (def m (read))
  (for [x (range 3)]
    ((with-open [wrtr (io/writer (str requestid "_" x ".txt"))] ; write to a new file
       (.write wrtr "cutting") (.write wrtr x))
     ))                                                     ; write smth

  (pmap blur (list (str requestid "_1") (str requestid "_2") (str requestid "_3")))
  )


(defn conveyor [filtr_lst]               ; Разбивает строку запроса на отдельные фильтры

  (println "filtr_lst is " filtr_lst)
  (println "type is " (type filtr_lst))
  (def panorama_amount (filterv (fn [x] (= x "panorama_stitching")) filtr_lst))
  (def HDR_amount (filterv (fn [x] (= x "HDR_conversion")) filtr_lst))
  (def cutting_areas_amount (filterv (fn [x] (= x "cutting_areas")) filtr_lst))
  ; Все остальные фильтры : choosing_best, noise_reduction_1, noise_reduction_2, noise_reduction_3, blur
  (def rest_amount (filterv (fn [x] (or (= x "choosing_best") (= x "noise_reduction_1") (= x "noise_reduction_2") (= x "noise_reduction_3") (= x "blur"))) filtr_lst))
  (println panorama_amount HDR_amount cutting_areas_amount rest_amount)
  (println (count filtr_lst) (count panorama_amount) (count HDR_amount) (count cutting_areas_amount) (count rest_amount))
  (def all_filtrs (+ (count panorama_amount) (count HDR_amount) (count cutting_areas_amount) (count rest_amount)))
  (println all_filtrs)


  (if (not= all_filtrs (count filtr_lst))
    (println "There are wrong filter's names")
    (println "All filters exist"))

  (cond
    (and (empty? panorama_amount) (empty? HDR_amount)) (println "There is no panorama_stitching or HDR_conversion")
    (> (+ (count panorama_amount) (count HDR_amount)) 1) (println "Too many filters from n to 1")
    (< photos_ID_amount 2) (println "Wrong amount of photos_ID")
    (or (= (first filtr_lst) "panorama_stitching") (= (first filtr_lst) "HDR_conversion")) (println "Filter from n to 1 passed all tests")
    :else (println "First filter is incorrect")
    )

  (cond
    (empty? cutting_areas_amount) (println "There is no cutting_area")
    (> (count cutting_areas_amount) 1) (println "Too many cutting_areas filters")
    (= (nth filtr_lst (- (count filtr_lst) 1)) "cutting_areas") (println "Cutting_areas passed all tests")
    :else (println "Last filter is incorrect")
    )

  (println "list of filtrs = ", filtr_lst)
  (def rest_request (atom (vec filtr_lst)))
  (while (not (empty? @rest_request))
    (apply_func (first @rest_request))                      ;apply filters
    (swap! rest_request rest))
  )

(defn apply_func [filtr]                                    ; без выбора лучшего. шумы по умолчанию
  (println "filtr = ", filtr)
  (cond                                                     ;; нет выбора лучшего!!!
    (= filtr "panorama_stitching") (panorama_stitching [photoid]) ; должно быть по дефолту то что считали с консоли
    (= filtr "HDR_conversion") (HDR_conversion [photoid])
    (= filtr "noise_reduction_1") (noise_reduction_1 [photoid])
    (= filtr "noise_reduction_2") (noise_reduction_2 [photoid])
    (= filtr "noise_reduction_3") (noise_reduction_3 [photoid])
    (= filtr "choosing_best") (choosing_best [photoid])
    (= filtr "blur") (blur [photoid])
    (= filtr "cutting_areas") (cutting_areas [photoid])
    :else (println "Error filter name"))
  )

(read_input)
