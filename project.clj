(require '[clojure.string :as str])
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
    (if (empty? (clojure.string/replace x #"[0-9]" ""))
        (first_id x)
        (println "Wrong request ID"))
    (def y (read))
    ; проверка на Int: если без цифр и пробелов пусто, значит были только цифры
    (if (empty? ( clojure.string/replace (clojure.string/replace y #"[0-9]" "") #" " ""))
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
    ; Добавить обработку на несколько ID
    (println "Photos ID")
    (println "contains only numbers")
    (println "type is " (type n))
    (println "number is " n))
; строка фильтров
(defn third_id [n]
    (println "Finding ( ) [ ]")
    (println n)
    (def round_open ( - (count n) (count (clojure.string/replace n "(" ""))))
    (println "round_open " round_open)
    (def round_close ( - (count n) (count (clojure.string/replace n ")" ""))))
    (println "round_close " round_close)

    (def square_open ( - (count n) (count (clojure.string/replace n "[" ""))))
    (println "square_open " square_open)
    (def square_close ( - (count n) (count (clojure.string/replace n "]" ""))))
    (println "square_close " square_close)

    (if (and (= round_open round_close) (= square_open square_close) (<= round_close 1 ) (<= square_close 1))
        (conveyor (str/split n #" "))
        (println "Wrong filter request"))
    )


(defn noise_reduction_1 [lst]) ; default
(defn noise_reduction_2 [lst])
(defn noise_reduction_3 [lst])
(defn blur [lst])

(defn panorama_stitching [lst])
(defn HDR_conversion [lst])
(defn choosing_best [lst])
; для choosing_best() в скобках указаны фильтры noise_reduction(1) noise_reduction(2) noise_reduction(3).
; иначе => ошибка ввода.

(defn cutting_areas [lst])
; для cutting_areas() в скобках указано целое число - колво областей для нарезки.
; иначе => ошибка ввода.


(defn conveyor [filtr_lst] ; голова конвеера. Разбивает строку запроса на отдельные фильтры
    ; проверки из документа dontforget.md
    ; обработка строки на наличие фильтров, скобок и тд
    ; тк дальше будет работа с 1 фильтром, а не со всей строкой

    ; Обработка строки на корректность ввода:
    ; колво вхождений panorama_stitching, HDR_conversion, cutting_areas() <=1.
    ; если есть panorama_stitching, то нет HDR_conversion. И наоборот. (то есть есть ктото один)
    ; если есть panorama_stitching или HDR_conversion - первый фильтр в строке.
    ; если есть panorama_stitching или HDR_conversion: кол-во ID фотографий > 1. если их нет, то колво ID фотографий = 1!
    ; если есть cutting_areas() - он последний фильтр в строке.
    ; нет фильтров(слов) не из нашего списка фильтров (тогда убрать строку else в apply_func)
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
    
    (if (not= all_filtrs (count filtr_lst))
        (println "There are wrong filter's names")
        (println "next check"))

    (if (> (+ (count panorama_amount) (count HDR_amount)) 1)
        (println "Too many filters from n to 1")
        (println "next check"))
    
    
    (def n_to_one (.indexOf (apply str filtr_lst) "panorama_stitching"))
    (def n_to_one_2 (.indexOf (apply str filtr_lst) "HDR_conversion"))
    (if (and (not (= n_to_one -1)) (not (= n_to_one_2 -1))) 
        (println "Error panorama_stitching and HDR_conversion at the same time")
    )
    
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
