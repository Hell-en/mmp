# mmp
Modern methods of programming 2023 NSU

Тема проекта:
Конвейерная обработка

Студенты: 23225.2
- Обершт Елена
- Потапова Анастасия

Модельная задача:

Продемонстрируйте работу разработанной вычислительной модели на примере обработки
фотографий. Достаточно симулировать работу фильтров/преобразований без их реального
использования. Вместо изображений использованы файлы с метаинформацией о том,
какие преобразования были выполнены.
Типы фильтров:
- преобразование 1-в-1 (т.е. без изменения типа объекта): устранение шумов (2 типа), blur
- преобразование n-в-1 (объекты поступают из одного канала): сшивка панорам, HDR-
преобразование
- преобразование 1-в-n: нарезка на области
- выбор одного из n (объекты поступают из разных каналов): выбираем лучший из 2х фильтров по устранению шумов

Дополнительные требования:
- многопоточные узлы (для модельной задачи это означает, что можно обрабатывать несколько фотографий одновременно), не нарушающие порядок
- конвейеры с циклами
