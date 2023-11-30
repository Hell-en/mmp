(require '[clojure.string :as str])
; Фильтры:
; n в 1
; сшивка панорам       -- panorama_stitching 
; hdr преобразование   -- HDR_conversion 
; Выбор лучшего        -- choosing_best()
; 1 в 1
; устранение шумов(1)  -- noise_reduction(1)
; устранение шумов(2)  -- noise_reduction(2)
; устранение шумов(3)  -- noise_reduction(3)
; blur                 -- blur  
; 1 в n
; нарезка на области   -- cutting_areas()


(defn first_id [n])
(defn second_id [n])
(defn third_id [n])
(defn walk [lst])
(defn conveyor [filtr_lst])
(defn apply_func [filtr])

(defn read_input []    (def x (read))
    (first_id x)    (def x (read))
    (second_id x)    (def x (read))
    (third_id x))
    ;(def x (read false :eof)))) не можем проверить есть ли еще данные.
    
; id запроса
(defn first_id [n]
    (if (int? n)
    (println n)
    (println "Wrong request ID")))
; id фотографий
(defn second_id [n]
   ; (map int? n)
    (println n))
; строка фильтров
(defn third_id [n]
    (def lst (str/split n #" "))
    (walk lst))

(defn walk [filtr_lst]
    (def ind_open (.indexOf (apply str filtr_lst) "("))
    (def ind_close (.indexOf (apply str filtr_lst) ")"))
    (println ind_open, ind_close)
    (if (and (= -1 ind_open) (= -1 ind_close)) (conveyor filtr_lst))
    (cond 
        (and (= -1 ind_open) (< -1 ind_close)) (println "Error (")
        (and (= -1 ind_close) (< -1 ind_open)) (println "Error )")
        (> ind_open ind_close) (println "Error )(")
    )
    :else ; ()! ; когда 2 скобки
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

    ; по хорошему функция walk должна идти после этой или внутри нее, но не раньше

    
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
