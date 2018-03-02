module Lab2 where

-- zad 2
data Figure = Circle Double Double Double | Rectangle Double Double Double Double | Line Double Double Double Double  

dimension :: Figure -> Int 
dimension (Circle radius x y) = 2

dimension (Rectangle x1 y1 x2 y2) = 2

dimenstion (Line x1 y1 x2 y2) = 1

area :: Figure -> Double
area (Circle radius x y) = 
    2 * pi * radius

area (Rectangle x1 y1 x2 y2) =
    (abs (x1 - x2)) * (abs (y1 - y2))

area (Line _ _ _ _) = 0

scale :: Figure -> Double -> Figure
scale (Circle radius x y) s = 
    Circle (radius * s) x y

scale (Rectangle x1 y1 x2 y2) s = 
    Rectangle x1 y1 x y
    where 
        x = x1 + (x2 - x1) * s
        y = y1 + (y2 - y1) * s

scale (Line x1 y1 x2 y2) s = 
    Line x1 y1 x y
    where 
        x = x1 + (x2 - x1) * s
        y = y1 + (y2 - y1) * s

-- zad 8

data Expression = 
    Sum Expression Expression |
     Value Double Expression Expression  

