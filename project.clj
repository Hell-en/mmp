(require '[clojure.string :as str])
; Фильтры:
; 1в 1
; устранение шумов(1)  -- noise_reduction(1)
; устранение шумов(2)  -- noise_reduction(2)
; устранение шумов(3)  -- noise_reduction(3)
; blur                 -- blur  
; n в 1
; сшивка панорам       -- panorama_stitching 
; hdr преобразование   -- HDR_conversion 
; Выбор лучшего        -- choosing_best()
; 1 в n
; нарезка на области   -- cutting_areas()


(defn first_id [n])
(defn second_id [n])
(defn third_id [n])
(defn walk [lst])
(defn apply_func [filtr_lst])

(defn read_input []    (def x (read))
    (first_id x)    (def x (read))
    (second_id x)    (def x (read))
    (third_id x))
    ;(def x (read false :eof)))) не можем проверить есть ли еще данные.
    
; id запроса
(defn first_id [n]
    (if (int? n)
    (println n)))
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
    (if (and (= -1 ind_open) (= -1 ind_close)) (apply_func filtr_lst))
    (cond 
        (and (= -1 ind_open) (< -1 ind_close)) (println "Error (")
        (and (= -1 ind_close) (< -1 ind_open)) (println "Error )")
        (> ind_open ind_close) (println "Error )(")
    )
    :else ; ()! ; когда 2 скобки
)

(defn noise_reduction [lst]) ; default
(defn noise_reduction_2 [lst])
(defn noise_reduction_3 [lst])
(defn blur [lst])

(defn panorama_stitching [lst])
(defn HDR_conversion [lst])
(defn fn_to_one [expr]
    (if (= expr "panorama_stitching")
        (panorama_stitching [])
    )
    (if (= expr "HDR_conversion")
        (HDR_conversion [])
    )
)

(defn cutting_areas [lst])


(defn apply_func [filtr_lst] ; без выбора лучшего. шумы по умолчанию
    (println "list of filtrs = ", filtr_lst)
    ; проверки:
    ; такие проверки берут первый. если фильтр указан дважды то плевать, делаем 1 раз

    ; 0) из н в 1 стоит или на первом месте или его нет
    (def n_to_one (.indexOf (apply str filtr_lst) "panorama_stitching"))
    (def n_to_one_2 (.indexOf (apply str filtr_lst) "HDR_conversion"))
    ; !! не важно в каком порядке ввод был. мы в любом случае смотрим, чтобы
    ; если фильтр был только 1 любой, то его и сделаем. если есть 2 или ни 1, то
    ; ничего делать не будет. решение!
    (if (and (= n_to_one -1) (not (= n_to_one_2 -1))) 
        (HDR_conversion [])
    )
    (if (and (not (= n_to_one -1)) (= n_to_one_2 -1))
        (panorama_stitching [])
    )

    ; 1) все слова соответсвуют названиям фильтров
    (if (some (fn [elem] (= elem "noise_reduction")) filtr_lst)
        (noise_reduction [])
    )
    (if (some (fn [elem] (= elem "blur")) filtr_lst)
        (blur [])
    )
    
    ; 3) из 1 в н стоит или на последнем месте или его нет - просто в последний зайдем
    ; нет двух одновременно фильтров н в 1 ! - и так нет, тк нет выбора в подфункции
    (if (some (fn [elem] (= elem "cutting_areas")) filtr_lst)
        (cutting_areas [])
    )
)

(read_input)
