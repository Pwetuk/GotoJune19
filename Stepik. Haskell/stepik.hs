--функция lenVect вычисляет длину трехмерного вектора
lenVec3 x y z =  sqrt(x*x + y*y + z*z)

--в дальнейшем коментарии будут относится к функциям под ними
--определяет знак числа
sign x
    | x > 0 = 1
    | x < 0 = -1
    |otherwise = 0
--модуль разности 2 чисел
x |-| y = abs(x - y)
--дистанция между 2 точками
dist :: (Double, Double) -> (Double, Double) -> Double
dist (x1, y1) (x2, y2) = sqrt((abs(x1 -x2) ^ 2) + (abs(y1 - y2)) ^ 2)
--факториал, учитывающий числа только той четности какой является аргумент
doubleFact :: Integer -> Integer
doubleFact n |n > 1 = n * (doubleFact (n - 2))
    |otherwise = 1
--числа Фибоначчи для целых чисел
fibonacci :: Integer -> Integer
fibonacci n | n>1 =fibonacci(n-2)+fibonacci(n-1) | n==1 =1 |n==0 =0 | n==(-1) =1 | otherwise =fibonacci(n+2)-fibonacci(n+1)
--более оптимизированный алгоритм вычисления чисел Фибоначчи
fibonacci' :: Integer -> Integer
helper a b i n =
    if (i==n) 
        then a + b
        else  if n > 0 
            then helper b (a + b) (i + 1) n
            else helper (b - a) a (i - 1) n

fibonacci' n = if n > 0 
    then helper 0 1 0 (n - 2)
    else if mod n 2 == 0 
        then -helper 0 1 0 ((-n) - 2)
        else helper 0 1 0 ((-n) - 2)  