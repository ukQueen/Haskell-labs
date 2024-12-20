# Курсовая работа - часть 1

## Задание: 

Написать синтаксический анализатор (парсер), разбирающий строки, прочитанные из текстового файла .txt. Файл должен содержать значения и бинарные операции.
> **Значения:** строки битов
> 
> **Бинарные операции:** сравнение >,<, =

Пример строки в файле: `010101 › 101010`. Вычислить проанализированное выражение, вывести его и результат вычисления на экран. Пользователь вводит название файла.

---

## Файлы реализации
В файле `src\Lib.hs` написаны все чистые функции. Доступ предоставлен только к нескольким, которые используются в `app\Main.hs`.  

В файле `app\Main.hs` написаны все функции работающие с файлами и взаимодействующие с пользователем через терминал.  

В папке `files` находится файл с бинарными операциями.

---

# Запуск программы 

## Запуск через `stack`
Пишем команду для сборки проекта:
```
stack build
```
после успешной сборки запускаем программу:
```
stack exec run
```

## Другой запуск через `stack`
Пишем команду для сборки проекта:
```
stack build
```
после успешной сборки запускаем программу:
```
stack exec 1Course-exe
```


## Запуск через `ghci` 
Заходим в  `ghci` :
```
stack ghci
```
после загружаем необходимый файл, из которого хотим запустить функцию. В данном случае это функция `main` в `app\Main.hs`:
```
:l app\Main.hs
```
запускаем необходимую нам функцию:
```
main
```


