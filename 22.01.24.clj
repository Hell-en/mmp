(ns nocomm
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.test :refer :all])
  )

(defn first_id [n])
(defn second_id [n])
(defn conveyor [filtr_lst])
(defn apply_func [filtr])
(defn blur [])
(defn noise_reduction_1 [])
(defn noise_reduction_2 [])
(defn noise_reduction_3 [])
(defn panorama_stitching [n])
(defn HDR_conversion [n])
(defn choosing_best [])
(defn cutting_areas [])
(defn cutting[f_name])


(def photoid (atom nil))
(def requestid (atom nil))


(defn read_input "Чтение водных данных. Входные данные должны быть предствленны в виде 'id запроса' 'id фотографий' 'список фильтров'." []
  (def x (read))
  (def requestid x)
  (if (empty? (str/replace x #"[0-9]" ""))
    (first_id x)
    (throw (IllegalArgumentException. "Wrong request ID"))
  )
  (def y (read))
  (def photoid y)
  (if (empty? (str/replace (str/replace y #"[0-9]" "") #" " ""))
    (second_id y)
    (throw (IllegalArgumentException. "Wrong photos ID"))
  )
  (def z (read))
  (conveyor z)
  )

(defn first_id "Проверка, что 'id запроса' содержит только числа, прошла успешно. [n] - 'id запроса'." [n]
  (println "Request ID contains only numbers")
  (println "type is " (type n))
  )
(defn second_id  "Проверка, что 'id фотографий' содержит только числа, прошла успешно. [n] - 'id фотографий'." [n]
  (println "Photos ID contains only numbers")
  (println "number is " n)
  (def photos_ID_amount (+ 1 (count (re-seq #" " n))))
  (println "amount of photos_ID " photos_ID_amount)
  )


(defn conveyor "Разбивает строку запроса на отдельные фильтры. [filtr_lst] - строка фильтров." [filtr_lst]
  ; filtr_lst is java.lang.String!
  (def filtr_array (str/split filtr_lst #" "))
  (println filtr_array)
  (def panorama_amount (filterv (fn [x] (= x "panorama_stitching")) filtr_array))
  (def HDR_amount (filterv (fn [x] (= x "HDR_conversion")) filtr_array))
  (def cutting_areas_amount (filterv (fn [x] (= x "cutting_areas")) filtr_array))
  ; Все остальные фильтры : choosing_best, noise_reduction_1, noise_reduction_2, noise_reduction_3, blur
  (def rest_amount (filterv (fn [x] (or (= x "choosing_best") (= x "noise_reduction_1") (= x "noise_reduction_2") (= x "noise_reduction_3") (= x "blur"))) filtr_array))
  (def all_filtrs (+ (count panorama_amount) (count HDR_amount) (count cutting_areas_amount) (count rest_amount)))
  (cond
    (and (empty? panorama_amount) (empty? HDR_amount)) (println "There is no panorama_stitching or HDR_conversion")
    (> (+ (count panorama_amount) (count HDR_amount)) 1) (throw (IllegalArgumentException. "Too many filters from n to 1"))
    (< photos_ID_amount 2) (throw (IllegalArgumentException. "Wrong amount of photos_ID"))
    (or (= (first filtr_array) "panorama_stitching") (= (first filtr_array) "HDR_conversion")) (println "Filter from n to 1 passed all tests")
    :else (throw (IllegalArgumentException. "First filter is incorrect"))
    )
  (cond
    (empty? cutting_areas_amount) (println "There is no cutting_area")
    (> (count cutting_areas_amount) 1) (throw (IllegalArgumentException. "Too many cutting_areas filters"))
    (= (nth filtr_array (- (count filtr_array) 1)) "cutting_areas") (println "Cutting_areas passed all tests")
    :else (throw (IllegalArgumentException. "Last filter is incorrect"))
    )
  (println "list of filtrs = ", filtr_array)
  (println (type filtr_array))
  (println (type filtr_lst))
  (println "photo_id = ", photoid)

  ; нет фильтров(слов) не из нашего списка фильтров
  (if (not= all_filtrs (count filtr_array))
    (throw (IllegalArgumentException. "There are some wrong filter's names"))
    (println "All filters exist"))

  (apply_func (for [x filtr_array]
                x))
  )

(defn apply_func "Применяет текущий фильтр в конвеере. Если название фильтра введено некорректно,
                  выводит сообщение об ошибке. [filtr] - текущий фильтров." [filtr]
  (println "current filtr = ", filtr)
  ; filtr`s type is LazeSeq
  ; Чтобы удобно работать дальше приведем его к типу строка
  (def str_filtr (first filtr))
  (cond
    (= str_filtr "panorama_stitching") (panorama_stitching photoid)
    (= str_filtr "HDR_conversion") (HDR_conversion photoid)
    (= str_filtr "noise_reduction_1") (noise_reduction_1)
    (= str_filtr "noise_reduction_2") (noise_reduction_2)
    (= str_filtr "noise_reduction_3") (noise_reduction_3)
    (= str_filtr "choosing_best") (choosing_best)
    (= str_filtr "blur") (blur)
    (= str_filtr "cutting_areas") (cutting_areas)
    )
  (println "APPLY CYCLE IS DONE. NEXT!")
  )

(defn blur "Применяет фильтр blur, производит запись в файл." []
  (def f_name (str requestid ".txt"))
  (with-open [wrtr (io/writer f_name :append true)]
    (.write wrtr "processed blur "))
  )

(defn noise_reduction_1 "Применяет фильтр устарнение_шумов_1, производит запись в файл." []
  (def f_name (str requestid ".txt"))
  (with-open [wrtr (io/writer f_name :append true)]
    (.write wrtr (str "processed noise_reduction_1 " photoid " ")))
  (Integer/parseInt photoid)
  )
(defn noise_reduction_2 "Применяет фильтр устарнение_шумов_2, производит запись в файл." []
  (def f_name (str requestid ".txt"))
  (with-open [wrtr (io/writer f_name :append true)]
    (.write wrtr (str "processed noise_reduction_2 " (inc (Integer/parseInt photoid)) " ")))
  (inc (Integer/parseInt photoid))
  )
(defn noise_reduction_3 "Применяет фильтр устарнение_шумов_3, производит запись в файл." []
  (def f_name (str requestid ".txt"))
  (with-open [wrtr (io/writer f_name :append true)]
    (.write wrtr (str "processed noise_reduction_3 " (dec (Integer/parseInt photoid)) " ")))
  (dec (Integer/parseInt photoid))
  )

(defn panorama_stitching "Применяет фильтр сшивка_панорамы, производит запись в файл. [photoid_lst] - id фотографий для работы." [photoid_lst]
  (def photo_id_array (str/split photoid_lst #" "))
  (def f_name (str requestid ".txt"))
  (def id_process (for [x photo_id_array] (str "processed panorama_stitching " x " ")))
  (with-open [wrtr (io/writer f_name :append true)]
    (.write wrtr (apply str id_process))
    )
  )
(defn HDR_conversion "Применяет фильтр HDR_преобразование, производит запись в файл. [photoid_lst] - id фотографий для работы." [photoid_lst]
  (def photo_id_array (str/split photoid_lst #" "))
  (def f_name (str requestid ".txt"))
  (def id_process (for [x photo_id_array] (str "processed HDR_conversion " x " ")))
  (with-open [wrtr (io/writer (str requestid ".txt") :append true)]
    (.write wrtr (apply str id_process))
    )
  )
(defn choosing_best "Применяет фильтр выбор_лучшего, производит запись в файл." []
  (println "choosing_beeeeeest")
  (dosync (pcalls (noise_reduction_3) (noise_reduction_1) (noise_reduction_2)))
  (def best_score (rand-int 3))
  (println "best_score" best_score)
  (def f_name (str requestid ".txt"))
  (with-open [wrtr (io/writer f_name :append true)]
    (.write wrtr (str "processed choosing_best, best noise_reduction filter is " best_score " ")))
  )

(defn cutting_areas "Применяет фильтр нарезка_на_области, производит запись в файл." []
  (cutting (str requestid ".txt"))
  (cutting (str requestid "_1.txt"))
  (cutting (str requestid "_2.txt"))
  (cutting (str requestid "_3.txt"))
  )

(defn cutting[f_name]
  (with-open [wrtr (io/writer f_name :append true)]
    (.write wrtr "processed cutting "))
  )


(read_input)


(deftest check_id_request_type
  (is (= java.lang.Long (type requestid))))
(deftest check_id_request_value
  (is (< 0 requestid)))
(deftest check_id_photo_type
  (is (= java.lang.String (type photoid))))
(deftest check_id_photo_value
  (is (< 0 (Integer/parseInt photoid))))
(deftest check_panorama_amount
  (is (thrown? Exception (conveyor "panorama_stitching panorama_stitching"))))
(deftest check_HDR_amount
  (is (thrown? Exception (conveyor "HDR_conversion HDR_conversion"))))
(deftest check_from_n_to_1_amount
  (is (thrown? Exception (conveyor "panorama_stitching HDR_conversion"))))
(deftest check_first_filter_panorama
  (is (thrown? Exception (conveyor "blur panorama_stitching"))))
(deftest check_first_filter_HDR
  (is (thrown? Exception (conveyor "blur HDR_conversion"))))
(deftest check_cutting_amount
  (is (thrown? Exception (conveyor "cutting_areas cutting_areas"))))
(deftest check_last_filter_cutting
  (is (thrown? Exception (conveyor "cutting_areas blur"))))
(deftest check_wrong_filters_name
  (is (thrown? Exception (conveyor "cutting_areas blur badfb"))))

(run-tests 'nocomm)
