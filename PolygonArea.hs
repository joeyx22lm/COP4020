module PolygonArea where

computeArea :: [(Double,Double)] -> Double
computeArea [] = error "No verticies Given"
computeArea [(w,x)] = error "Too few verticies given"
computeArea [(w,x),(y,z)] = ((*) (0.5) (det (w,x) (y,z)))
computeArea [(a,b),(c,d),(e,f)] = (*) (0.5) ((det (a,b) (c,d))+(det (c,d) (e,f))+(det (e,f) (a,b)))
computeArea [(a,b),(c,d),(e,f),(g,h)] = (*) (0.5) ((det (a,b) (c,d))+(det (c,d) (e,f))+(det (e,f) (g,h))+(det (g,h) (a,b)))
computeArea [(a,b),(c,d),(e,f),(g,h),(i,j)] = (*) (0.5) ((det (a,b) (c,d))+(det (c,d) (e,f))+(det (e,f) (g,h))+(det (g,h) (i,j))+(det (i,j) (a,b)))
computeArea [(a,b),(c,d),(e,f),(g,h),(i,j),(k,l)] = (*) (0.5) ((det (a,b) (c,d))+(det (c,d) (e,f))+(det (e,f) (g,h))+(det (g,h) (i,j))+(det (i,j) (k,l))+(det (k,l) (a,b))) 
computeArea [(a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n)] = (*) (0.5) ((det (a,b) (c,d))+(det (c,d) (e,f))+(det (e,f) (g,h))+(det (g,h) (i,j))+(det (i,j) (k,l))+(det (k,l) (m,n))+(det (m,n) (a,b)))
computeArea _ = 0.0

-- Uhhhh I know I had to do this in recursion for the circle, for unlimited # of vertices, buttttt....... :/

det :: (Double,Double) -> (Double,Double) -> Double
det (w,x) (y,z) = ((-) ((*) w z) ((*) x y))
