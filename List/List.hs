import Data.List
x = [5, 2, 3, 4]
y = x++[12]
isZero x = if x == 0 then True else False
z = y++[0, 0, 0]
c = filter (isZero) z
f x = x * x
v = map f z
b = 1:b
n = take 10 b
m = [x * x | x <- [1..]]
k = [(x, y, z)| x<-[1..1000], y<-[1..1000], z<-[1..1000], x^2 + y^2 == z^2]
