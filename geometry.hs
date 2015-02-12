--Kahli Holmes  EID: kh27624

data Geometry = Point Int Int   -- x, y
              | Circle Int Int Int      -- x, y, radius
              | Rectangle Int Int Int Int -- x1, y1, x2, y2
              | Triangle Int Int Int Int Int Int  
              | Group [Geometry]

--area is 62.831856, bounding box is (-4.0,-4.0,4.0,4.0)
concentric = Group [Circle 0 0 2, Circle 0 0 4]

--area is 150.26549, bounding box is (-4.0,-4.0,10.0,10.0)
circleAndRect = Group [Circle 0 0 4, Rectangle 0 0 10 10]

--area is 4.0, bounding box is (-2.0,0.0,2.0,2.0)
triangleTest = Triangle (-2) 0 2 0 0 2

--MY TEST CASES--   add abs for geometric values
--area is   67.774
--circle    28.274
--rect      6
--point     0
--triangle  35.5
--allShapes = Group [Circle 0 0 3, Rectangle 1 2 3 4, Point 17 17, Triangle 1 1 4 8 15 10]

---------------------------------------------------------

--calculates the area of Geometric Objects defined above
area :: Geometry -> Float
area (Point x y)                  = 0
area (Circle x y r)               = pi * fromIntegral(abs r) ^ 2
area (Rectangle t l r b)          = fromIntegral(abs r - t) * fromIntegral(abs b - l)
area (Triangle x1 x2 y1 y2 z1 z2) = abs (fromIntegral(x1*(y2-z2)) + fromIntegral(y1*(z2-x2)) + fromIntegral(z1*(x2-y2)))/2
area (Group cs)                   = sum [ area c | c <- cs ]


--computes the max and min of two boundingBoxes
--to create a boundingBox that encapsulates multiple 
--Geometric Objects
extremas :: (Float, Float, Float, Float) -> (Float, Float, Float, Float) -> (Float, Float, Float, Float)
extremas (t0,t1,t2,t3) (x0,x1,x2,x3) = (min t0 x0, min t1 x1 , max t2 x2 , max t3 x3)


--handles a list of Geometric Objects to aide
--in computing its boundingBox
merge :: [Geometry] -> (Float, Float, Float, Float)
merge [] = (0.0, 0.0, 0.0, 0.0)
merge (e:l)  = extremas (boundingBox e) (merge l)


--the smallest rectangle than encloses ALL shapes in the group
--of Geometric Objects
boundingBox :: Geometry -> (Float, Float, Float, Float)
boundingBox (Point x y)                  = (fromIntegral x, fromIntegral x, fromIntegral y, fromIntegral y)
boundingBox (Circle x y r)               = (fromIntegral(x - r), fromIntegral(y-r), fromIntegral(x+r), fromIntegral(y+r))
boundingBox (Rectangle t l r b)          = (fromIntegral(t), fromIntegral(l), fromIntegral(r), fromIntegral(b))
boundingBox (Triangle x1 x2 y1 y2 z1 z2) = (fromIntegral(min x1 (min y1 z1)), fromIntegral(min x2 (min y2 z2)), fromIntegral(max x1 (max y1 z1)), fromIntegral(max x2 (max y2 z2)))
boundingBox (Group cs)                   =  merge(cs)

---------------------------------------------------------

main = do
  putStrLn "concentric circles:"
  print (area concentric)
  print (boundingBox concentric)

  putStrLn "\ncircle and rectangle:"
  print (area circleAndRect)
  print (boundingBox circleAndRect)
  
  putStrLn "\ntriangle:"
  print (area triangleTest)
  print (boundingBox triangleTest)

--  TEST CASE--
--  putStrLn "\nall Shapes:"
--  print (area allShapes)
--  print (boundingBox allShapes)

