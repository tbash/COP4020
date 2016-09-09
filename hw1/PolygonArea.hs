module PolygonArea where

computeArea :: [(Double,Double)] -> Double
computeArea [(a,b):xs] = det(a,b) 

det :: (Double,Double)-> (Double,Double) -> Double
det (x1,y1) (x2,y2) = (x1*y2) - (x2*y1)

