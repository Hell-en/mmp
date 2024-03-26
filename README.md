# mmp
## Modern methods of programming, 2023 NSU

Тема проекта:
Конвейерная обработка

Студенты: 23225.2
- Обершт Елена
- Потапова Анастасия


Конвейер состоит из связанных между собой узлов. Узел характеризуется набором входных и
выходных каналов, по которым могут передаваться объекты. Узел ожидает появления
определенного набора объектов на своих входных каналах, после чего проводит вычисления
и порождает объект(-ы) на своих выходных каналах (не обязательно всех). Например, узел
"сумматор" имеет два входных канала и один выходной. Получая по одному объекту с
каждого входного канала, он их суммирует и результат отправляет в выходной канал. При
этом, если входные каналы не будут синхронизованы, то в одном из них могут
"скапливаться" данные, ожидающие обработки. Определите проблемно-специфичный язык
(DSL) для работы с конвейерными схемами обработки данных, поддерживающий следующие
элементы:

• узлы с набором входных и выходных каналов;

• статическую типизацию каналов;

• формирование конвейера в форме связанных узлов в соответствии с типизацией;

• запуск конвейера и обработку данных;

• параллельное исполнение отдельных узлов конвейера;

• использование одинаковых узлов для формирования разных конвейеров.


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

Обершт:

- реализация фильтров
- реализация дополнительных требований

Потапова:

- обработка ошибок
- разработка циклического прохождения конвейера



Путь решения задачи:

На вход пользователь вводит строку запроса по шаблону: ID-запроса ID-фотографий "Названия фильтров"

Количество фотографий и фильтров может быть как 1 так и несколько.

Фильтры применяются к фотографии в порядке, заданном пользователем. Таким образом для различных запросов формируются разлчные конвейеры обработки фотографий.
