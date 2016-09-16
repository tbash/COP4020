module PolygonArea where

computeArea :: [(Double,Double)] -> Double
computeArea [] = error "KURWA"
computeArea [a] = error "MACH"
computeArea (a:xs) = (0.5) * abs(det (last xs) a + det a (head xs) + 
	computeArea' xs)

det :: (Double,Double) -> (Double,Double) -> Double
det (x1,y1) (x2,y2) = (x1*y2) - (x2*y1)

computeArea' :: [(Double,Double)]->Double
computeArea' [a,b] = det a b
computeArea' (a:xs) = det a (head xs) + computeArea' xs
