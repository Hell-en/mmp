(ns main (:require [clojure.string :as str]
         [clojure.java.io :as io])
  )

; Фильтры:
; n в 1
; сшивка панорам       -- panorama_stitching
; hdr преобразование   -- HDR_conversion
; Выбор лучшего        -- choosing_best()
; 1 в 1
; устранение шумов(1)  -- noise_reduction_1
; устранение шумов(2)  -- noise_reduction_2
; устранение шумов(3)  -- noise_reduction_3
; blur                 -- blur
; 1 в n
; нарезка на области   -- cutting_areas()


(defn first_id [n])
(defn second_id [n])
(defn third_id [n])
(defn walk [lst])
(defn conveyor [filtr_lst])
(defn apply_func [filtr])


; Если в read_input сразу есть "Wrong request ID" или "Wrong photos ID" => в конвеер даже не заходим. На этом конец.
; При обработке скобок вышло "Wrong filter request" => то же самое. Дальше в конвеер не заходим.
; При обработке строки фильтров в конвеере сначала проверить коррекность всех записей.
; Если что-то не так -> конвеер не запускаем.
; И только если все условия соблюдены идем в конвеер!!!



(defn read_input []
  (def x (read))
  ; проверка на Int: если без цифр пусто, значит были только цифры
  (if (empty? (str/replace x #"[0-9]" ""))
    (first_id x)
    (println "Wrong request ID"))
  (def y (read))
  ; проверка на Int: если без цифр и пробелов пусто, значит были только цифры
  (if (empty? (str/replace (str/replace y #"[0-9]" "") #" " ""))
    (second_id y)
    (println "Wrong photos ID"))
  (def z (read))
  (third_id z)
  )
;(def x (read false :eof)))) не можем проверить есть ли еще данные.

; id запроса
(defn first_id [n]
  ; (int n)
  (println "Request ID")
  (println "contains only numbers")
  (println "type is " (type n))
  (println "number is " n))
; id фотографий
(defn second_id [n]
  (println "Photos ID")
  (println "contains only numbers")
  (println "type is " (type n))
  (println "number is " n)
  (def photos_ID_amount (+ 1 (count (re-seq #" " n))))
  (println "amount of photos_ID " photos_ID_amount))

; строка фильтров
(defn third_id [n]
  (println "Finding ( ) [ ]")
  (println n)
  (def round_open ( - (count n) (count (str/replace n "(" ""))))
  (println "round_open " round_open)
  (def round_close ( - (count n) (count (str/replace n ")" ""))))
  (println "round_close " round_close)

  (def square_open ( - (count n) (count (str/replace n "[" ""))))
  (println "square_open " square_open)
  (def square_close ( - (count n) (count (str/replace n "]" ""))))
  (println "square_close " square_close)

  (if (and (= round_open round_close) (= square_open square_close) (<= round_close 1 ) (<= square_close 1))
    (conveyor (str/split n #" "))
    (println "Wrong filter request"))
  )


(defn noise_reduction_1 [photoid] ; default
  (def f_name (slurp (str photoid ".txt"))) ; возьмем имя файла
  (with-open [wrtr (io/writer f_name :append true)] ; пишем в уже существ файл
    (.write wrtr (str "processed noise_reduction_1 " photoid)))
  )
(defn noise_reduction_2 [photoid]
  (def f_name (slurp (str photoid ".txt"))) ; возьмем имя файла
  (with-open [wrtr (io/writer f_name :append true)] ; пишем в уже существ файл
    (.write wrtr (str "processed noise_reduction_2 " (inc photoid))))
  )
(defn noise_reduction_3 [photoid]
  (def f_name (slurp (str photoid ".txt"))) ; возьмем имя файла
  (with-open [wrtr (io/writer f_name :append true)] ; пишем в уже существ файл
    (.write wrtr (str "processed noise_reduction_3 " (dec photoid))))
  )
(defn blur [photoid]
  (def f_name (slurp (str photoid ".txt"))) ; возьмем имя файла
  (with-open [wrtr (io/writer f_name :append true)] ; пишем в уже существ файл
    (.write wrtr "processed blur"))
  )

(defn panorama_stitching [photoid_lst]
  (with-open [wrtr (io/writer "p-s_new.txt")] ; write to a new file
    (.write wrtr "panorama_stitching" )
    (.write wrtr (for [x photoid_lst] x))) ; will work; resulted like ()
  )
(defn HDR_conversion [photoid_lst]
  (use 'clojure.java.io)
  (with-open [wrtr (io/writer "HDR_new.txt")] ; write to a new file
    (.write wrtr "HDR_conversion" )
    (.write wrtr (for [x photoid_lst] x))) ; will work (?); resulted like ()
  )
(defn choosing_best [photoid, fl1, fl2, fl3]
  (fl1 photoid)
  (fl2 photoid)
  (fl3 photoid)
  ;read 3 files, take last elems. compare them
  )
; для choosing_best() в скобках указаны фильтры noise_reduction(1) noise_reduction(2) noise_reduction(3).
; иначе => ошибка ввода.

(defn cutting_areas [photoid, m]
  ; (for [x (range 3)] )
  )
; для cutting_areas() в скобках указано целое число - колво областей для нарезки.
; иначе => ошибка ввода.


(defn conveyor [filtr_lst] ; голова конвеера. Разбивает строку запроса на отдельные фильтры
  ; проверки из документа dontforget.md
  ; обработка строки на наличие фильтров, скобок и тд
  ; тк дальше будет работа с 1 фильтром, а не со всей строкой

  ; Обработка строки на корректность ввода:
  ; если любое условие не выполнияется => ошибка ввода.


  (println "filtr_lst is " filtr_lst)
  (println "type is " (type filtr_lst))
  ; фильтры, которых должно быть не больше одного
  (def panorama_amount (filterv (fn [x] (= x "panorama_stitching")) filtr_lst))
  (def HDR_amount (filterv (fn [x] (= x "HDR_conversion")) filtr_lst))
  (def cutting_areas_amount (filterv (fn [x] (= x "cutting_areas")) filtr_lst))
  ; Все остальные фильтры : choosing_best, noise_reduction_1, noise_reduction_2, noise_reduction_3, blur
  (def rest_amount (filterv (fn [x] (or (= x "choosing_best") (= x "noise_reduction_1") (= x "noise_reduction_2") (= x "noise_reduction_3") (= x "blur"))) filtr_lst))
  (println panorama_amount HDR_amount cutting_areas_amount rest_amount)
  (println (count filtr_lst) (count panorama_amount) (count HDR_amount) (count cutting_areas_amount) (count rest_amount) )
  (def all_filtrs (+ (count panorama_amount) (count HDR_amount) (count cutting_areas_amount) (count rest_amount)))
  (println all_filtrs)


  ; нет фильтров(слов) не из нашего списка фильтров (тогда убрать строку else в apply_func)
  (if (not= all_filtrs (count filtr_lst))
    (println "There are wrong filter's names")
    (println "All filters exist"))



  ; колво вхождений panorama_stitching, HDR_conversion <=1.
  ; если есть panorama_stitching, то нет HDR_conversion. И наоборот. (то есть есть кто-то один)
  ; если есть panorama_stitching или HDR_conversion: кол-во ID фотографий > 1. если их нет, то колво ID фотографий = 1!
  ; если есть panorama_stitching или HDR_conversion - первый фильтр в строке.
  (cond
    (and (empty? panorama_amount) (empty? HDR_amount)) (println "There is no panorama_stitching or HDR_conversion")
    (> (+ (count panorama_amount) (count HDR_amount)) 1) (println "Too many filters from n to 1")
    (< photos_ID_amount 2) (println "Wrong amount of photos_ID")
    (or (= (first filtr_lst) "panorama_stitching") (= (first filtr_lst) "HDR_conversion")) (println "Filter from n to 1 passed all tests")
    :else (println "First filter is incorrect"))


  ; если есть cutting_areas() - он последний фильтр в строке.
  ; колво вхождений cutting_areas() <=1.
  (cond
    (empty? cutting_areas_amount) (println "There is no cutting_area")
    (> (count cutting_areas_amount) 1) (println "Too many cutting_areas filters")
    (= (nth filtr_lst (- (count filtr_lst) 1)) "cutting_areas") (println "Cutting_areas passed all tests")
    :else (println "Last filter is incorrect"))


  (println "list of filtrs = ", filtr_lst)
  (def rest_request (atom (vec filtr_lst)))
  (while (not(empty? @rest_request))
    ;(println (first @rest_request))
    (apply_func (first @rest_request))
    (swap! rest_request rest))
  )

(defn apply_func [filtr] ; без выбора лучшего. шумы по умолчанию
  (println "filtr = ", filtr)


  (cond
    (= filtr "panorama_stitching") (panorama_stitching [])
    (= filtr "HDR_conversion") (HDR_conversion [])
    (= filtr "noise_reduction_1") (noise_reduction_1 [])
    (= filtr "noise_reduction_2") (noise_reduction_2 [])
    (= filtr "noise_reduction_3") (noise_reduction_3 [])
    (= filtr "blur") (blur [])
    (= filtr "cutting_areas") (cutting_areas [])
    :else (println "Error filter name"))
  )

(read_input)
