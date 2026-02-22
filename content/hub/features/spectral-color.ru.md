---
title: "Спектральное смешение цветов"
type: docs
---
Система палитр Lumi использует спектральную цветовую модель для имитации смешивания реальных пигментов. Цель состоит в том, чтобы процесс создания и выбора цветов из цифровой палитры напоминал смешивание физических красок. После применения цвета к холсту он становится стандартным RGB.

## Что означает спектральное смешивание

Традиционное смешивание RGB является аддитивным: смешивание двух значений RGB усредняет их к средней точке. Смешение пигментов является субтрактивным: каждый пигмент поглощает волны определенной длины, и их совокупный эффект становится темнее и часто меняет оттенок.

Люми моделирует это, используя 10-полосное представление спектрального отражения для цветов палитры, а не RGB.

Это дает результат, похожий на краску: смешивание синего и желтого дает зеленый, а не серый цвет. Смешивание двух насыщенных цветов дает цвет, который смещается в сторону нейтрального, как это происходит с физическими пигментами.

Спектральные вычисления выполняются во время построения палитры — при создании записей вторичной и третичной палитры, а также когда микшер палитр смешивает два родительских цвета. Результирующий цвет преобразуется в линейный RGB для отображения и рисования.

## Профили пигментов

Записи палитры могут быть основаны на реальных данных о пигментах с использованием **кодов индекса цвета (CI)**. Каждое семейство пигментов CI имеет характерное спектральное смещение, которое влияет на то, как оно смешивается.

| Роль пигмента | Смешивание поведения | Пример |
| :--- | :--- | :--- |
| **Основной** | Высокая цветность, чистые вторичные материалы | PY3 (лимонно-желтый), PR122 (пурпурный) |
| **Тело** | Матовый, сильный массовый тон, переходит в оливковый в зеленых миксах | PY35 (желтый кадмий), PR108 (красный кадмий) |
| **Нейтрализатор** | Быстро обесцвечивает и приглушает звук | PBk11 (Марс Черный), PBr7 (Сиена) |
| **Якорь цветности** | Высокая красящая способность, доминирует над смесями | PB29 (ультрамариновый синий), PG7 (фталозеленый) |

Добавление основных цветов с кодами CI в палитру дает механизму смешивания точный спектральный сдвиг для этих цветов, поэтому создаваемые вторичные и третичные смеси отражают реальное поведение смешивания.

## Люми Пигменты

Палитра Master поставляется со следующими пигментами. Образцы демонстрируют типичный массовый оттенок каждого пигмента (насыщенный, неразбавленный).

### Апельсины и желтые| Образец | Имя | Код CI | Семья |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(245,135,20);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> | Пирроловый оранжевый | ПО73 | Красный (Алый) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(243,114,64);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> | Кадмий оранжевый | РО20 | Желтый (Тело) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(240,180,80);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> | Кадмий желтый | ПГ35 | Желтый (Тело) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(245,210,25);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> | Кадмий Желтый Бледный | PY35:Бледный | Желтый (бледный кадмий) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(250,230,5);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> | Лимонно-желтый | ПГ3 | Желтый (Лимонный) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(225,155,10);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> | Никель азо желтый | ПГ150 | Желтый (Средний) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(180,175,45);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> | Зеленое золото | PY129 | Желто-зеленый (золотой) |

### Цвета Земли

| Образец | Имя | Код CI | Семья |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(200,100,70);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> | Жженая Сиенна | PBr7:Сгорел | Земля (красно-коричневый) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(117,66,0);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> | Жженая умбра | PBr7:Умбра | Земля (Нейтральная) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(205,68,35);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> | Сырая Сиенна | PBr7:Сырой | Земля (Желто-Коричневый) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(187,124,25);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> | Желтая охра | PY42 | Земля (Желтый) |

### Зелень

| Образец | Имя | Код CI | Семья |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,166,81);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> | Фтало-зеленый (YS) | PG36 | Зеленый (желтый оттенок фтало) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(64,130,109);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> | Виридиан | PG18 | Зеленый (Виридиан) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(128,138,112);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> | Терре Верте | PG23 | Зеленый (Земля Прохладный) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,110,100);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> | Уинзор Грин (Б.С.) | PG7 | Зеленый (Фтало-синий оттенок) |### Блюз и цианы

| Образец | Имя | Код CI | Семья |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,177,176);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> | Кобальт Бирюзовый Светлый | PG50 | Голубой (минерал) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,148,214);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> | лазурный синий | ПБ35 | Голубой (минерал) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,100,110);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> | Фтало Бирюзовый | ПБ16 | Синий (Фтало) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,123,194);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> | Кобальтовый синий | ПБ28 | Синий (фиолетово-худой) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,75,115);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> | Винзор Блю | ПБ15 | Синий (Фтало) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(27,63,148);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> | Ультрамарин | ПБ29 | Синий (фиолетово-худой) |

### Фиолетовые, пурпурные и красные

| Образец | Имя | Код CI | Семья |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(124,65,153);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> | блестящий фиолетовый | ПВ23 | Фиолетовый (Диоксазин) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(230,90,180);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> | Перманентная роза | PV19:Роза | Пурпурный (Хинакридон) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(190,40,120);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> | Хинакридон пурпурный | PV19:Пурпурный | Пурпурный (Хинакридон) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(160,30,65);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> | Постоянный Ализарин Кримсон | PV19:Малиновый | Пурпурный (Хинакридон) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(120,35,65);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> | Перилен Фиолетовый | ПВ29 | Пурпурный (Хинакридон) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(135,10,45);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> | Перилен Бордовый | ПР179 | Красный (Малиновый) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(215,30,60);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> | Пирроловый красный | ПР254 | Красный (Алый) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(225,55,65);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> | Пиррол Красный Свет | ПР255 | Красный (пирроловый свет) |

### Чёрные и белые| Образец | Имя | Код CI | Семья |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(22,15,10);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> | Марс Черный (Теплый) | ПБк11 | Черный (Марс) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(18,28,12);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> | Перилен Зеленый | ПБк31 | Черный (периленовый зеленый) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(10,18,19);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> | Черный цвет слоновой кости (прохладный) | ПБк9 | Черный (слоновая кость) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(18,18,18);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> | Лампа черная (нейтральная) | ПБк7 | Черный (Лампа) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(255,249,235);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> | Титановый Белый (Теплый) | PW6:Теплый | Белый (Титан Теплый) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(255,255,255);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> | Титановый белый (нейтральный) | PW6 | Белый (титановый нейтральный) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(245,250,255);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> | Цинковый белый (холодный) | ПВ4 | Белый (Цинковый Холодный) |

### Контроль серого

Контрольные серые — это стандартизированные нейтрализаторы, используемые для предсказуемого обесцвечивания смесей.

| Образец | Имя | Код CI |
| :---: | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(135,128,120);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> | Теплый серый | Н_ТЕПЛЫЙ |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(128,128,128);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> | Нейтральный серый | Н_НЕЙТРАЛЬ |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(120,128,135);vertical-align:middle;border:1px Solid rgba(0,0,0,0.25)"></span> | Прохладный серый | Н_КУЛ |

## Карта палитры

Карта палитры визуализирует активную палитру в виде колеса оттенков: 36 секторов оттенков (с шагом 10°) × 15 ячеек яркости. При добавлении первичных объектов система генерирует вторичные и третичные миксы и размещает их в соответствующих позициях на карте.

Щелчок по ячейке выбирает цвет в качестве переднего плана. Щелчок при нажатой клавише Shift назначает его родительской конечной точкой в ​​микшере палитр.

## Миксер палитр

Смеситель палитр получает новые цвета из двух родительских записей, используя фиксированный трехэтапный конвейер:

1. **Смешение**: спектральная WGM между родительским сигналом A (CCW) и родительским сигналом B (CW).
2. **Цветность**: растушуйте в сторону нейтрального спектра палитры, уменьшая насыщенность.
3. **Тон**: растушуйте в сторону смешивания белого или черного, регулируя яркость.

Тон применяется последним, поэтому настройки яркости не разбавляются изменениями цветности. Элементы управления Value Lock и Band Clamp ограничивают результаты определенным уровнем яркости или диапазоном значений.

Смешанные цвета можно сохранить в палитре как записи **Пользовательские**, сохраняя полный рецепт (родительские UID, коэффициент смешивания, тон, значения цветности) для последующего восстановления.

## Пиксели холста — это RGBСпектральная система полностью работает в рамках построения палитры и выбора цвета. Когда применяется мазок кисти, цвет переднего плана, уже преобразованный в линейный RGB, окрашивается. Холст хранит стандартные данные пикселей RGB.

Спектральное смешивание улучшает процесс создания палитры и выбора цветов в соответствии с физическим поведением пигментов, не меняя при этом способ хранения или компоновки данных изображения.