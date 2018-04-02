import Data.List
import System.IO

data Vector = Vector { vxx :: Double, vyy :: Double} deriving (Eq, Show)
data Body = Body{ px :: Double, py :: Double, velocity::Vector,mass :: Double} deriving (Eq, Show)
data Quad = Quad{ xmid :: Double, ymid :: Double, length :: Double}| Nothg deriving (Eq, Show)
data DisQuads = AllPQuads{nwq :: Quad, neq :: Quad, swq :: Quad, seq :: Quad}| Noquad deriving (Eq, Show)
data BHTree = LeafNode {body :: Body,region :: Quad}| InternalNode { 
	quad :: Quad, nw :: BHTree, ne :: BHTree, sw :: BHTree, se :: BHTree,
	totalMass :: Double, comx :: Double, comy :: Double }| Empty{quad :: Quad} deriving (Eq, Show)

newBody :: [Char] -> Body
newBody = makeBody.convert.words
	where 
		makeBody (x:y:z:e:f:g) = Body x y (Vector z e) f
		convert [] = []
		convert (x:xs) = (read x :: Double) : (convert xs)

contains :: Body -> Quad -> Bool
contains (Body x y _ _ ) (Quad xq yq l)
	| x <= xq + (l/2.0) && x>xq-(l/2.0) && y <= yq+(l/2.0) && y>yq-(l/2.0) = True | otherwise = False

containss :: Body -> BHTree -> Bool
containss bdy (Empty ql) | bdy `contains` ql = True | otherwise = False
containss bdy (LeafNode bl ql) | bdy `contains` ql = True | otherwise = False
containss bdy (InternalNode qin _ _ _ _ _ _  _)| bdy `contains` qin = True | otherwise = False

equals :: Body -> Body -> Bool
equals (Body a1 b c e) (Body a2 b1 c1 e1) | a1==a1 && b==b1 && (c == c1)  && e==e1 = True | otherwise = False

disQuad :: Quad -> [Quad]
disQuad (Quad xmid ymid length) = [q1, q2, q3, q4]
	where
		q1 = Quad (xmid-(length/4.0)) (ymid+(length/4.0)) (length/2.0)
		q2 = Quad (xmid+(length/4.0)) (ymid+(length/4.0)) (length/2.0)
		q3 = Quad (xmid-(length/4.0)) (ymid-(length/4.0)) (length/2.0)
		q4 = Quad (xmid+(length/4.0)) (ymid-(length/4.0)) (length/2.0)		

comnmass :: BHTree -> Double
comnmass (LeafNode (Body _ _ _  m ) _ ) = m
comnmass (Empty q) = 0
comnmass (InternalNode _ a b c d _ _ _) = (comnmass a) + (comnmass b) + (comnmass c) + (comnmass d)

comassx :: BHTree -> Double
comassx (LeafNode (Body x _ _ m ) _ ) = m*x
comassx (Empty q) = 0
comassx (InternalNode _ a b c d f _ _) = ((comassx a) + (comassx b) + (comassx c) + (comassx d))

comassy :: BHTree -> Double
comassy (LeafNode (Body _ y _  m ) _ ) = m*y
comassy (Empty q) = 0
comassy (InternalNode _ a b c d f _ _) = ((comassy a) + (comassy b) + (comassy c) + (comassy d))

newTree :: BHTree -> BHTree
newTree (Empty q) = Empty q
newTree (LeafNode bdy q ) = LeafNode bdy q  
newTree (InternalNode a b c d e f g h) = InternalNode a (newTree b) (newTree c) (newTree d) (newTree e) mcal g h
	where
		mcal = comnmass (b) + comnmass (c) + comnmass (d) + comnmass (e)

nTree :: BHTree -> BHTree
nTree (Empty q) = Empty q
nTree (LeafNode bdy q ) = LeafNode bdy q 
nTree (InternalNode a b c d e f g h) = InternalNode a (nTree b) (nTree c) (nTree d) (nTree e) f mcal h
	where
		mcal = ((comassx b) + (comassx c) + (comassx d) + (comassx e))/f

nnTree :: BHTree -> BHTree
nnTree (Empty q) = Empty q
nnTree (LeafNode bdy q ) = LeafNode bdy q 
nnTree (InternalNode a b c d e f g h) = InternalNode a (nnTree b) (nnTree c) (nnTree d) (nnTree e) f g mcal
	where
		mcal = ((comassy b) + (comassy c) + (comassy d) + (comassy e))/f

inserts :: Body -> BHTree -> BHTree
inserts bdy (Empty q) = LeafNode bdy q

inserts b@(Body x y xv m) l@(LeafNode bd q) = (inserts bd (InternalNode q inw ine isw ise 0 0 0))
	where
		k = disQuad q
		inw | b `contains` (k !! 0) = LeafNode b (k !! 0) | otherwise = Empty (k !! 0)
		ine | b `contains` (k !! 1) = LeafNode b (k !! 1) | otherwise = Empty (k !! 1)
		isw | b `contains` (k !! 2) = LeafNode b (k !! 2) | otherwise = Empty (k !! 2)
		ise | b `contains` (k !! 3) = LeafNode b (k !! 3) | otherwise = Empty (k !! 3)

inserts b@(Body x y xv m) l@(InternalNode q anw ans asw ase _ _ _ )  = (nnTree(nTree(newTree (InternalNode q inw ine isw ise 0 0 0)))) 
	where
		inw | b `containss` anw = inserts b anw | otherwise = anw
		ine | b `containss` ans = inserts b ans | otherwise = ans
		isw | b `containss` asw = inserts b asw | otherwise = asw
		ise | b `containss` ase = inserts b ase | otherwise = ase	

plus :: Vector -> Vector -> Vector
plus (Vector a b) (Vector c d) = Vector (a+c) (b+d) 
theeta :: Double -> Double -> Double -> Double -> Double
theeta a b c d = atan((d-b)/(c-a))

forces :: Body -> BHTree -> Vector
forces bdy@(Body a b c e) (Empty f) = Vector 0 0
forces bdy@(Body a b c e) (LeafNode f@(Body qw ew rw yw) g) 
	| bdy `equals` f = Vector 0 0 | otherwise = Vector ((((-6.67408/(10^(11)))*e*yw)/((bdy `distance` f)^2)+3E4*3E4) * (cos (theeta a b qw ew)))
						 							   ((((-6.67408/(10^(11)))*e*yw)/((bdy `distance` f)^2)+3E4*3E4) * (sin (theeta a b qw ew)))

forces bdy@(Body a b c e) (InternalNode (Quad p k l) w r t y u i o)
	| (l/(sqrt ((a-i)^2 + (b-o)^2))) < 0.5 = Vector (((-6.67408/(10^(11)))*e*u)/((sqrt ((a-i)^2 + (b-o)^2))^2 +3E4*3E4) * cos (theeta a b i o))
											 		(((-6.67408/(10^(11)))*e*u)/((sqrt ((a-i)^2 + (b-o)^2))^2 +3E4*3E4) * sin (theeta a b i o))
	| otherwise = (forces bdy w) `plus` (forces bdy r) `plus` (forces bdy t) `plus` (forces bdy y)

dt = 2500

updateVel :: (Vector, Body) -> Body
updateVel (f@(Vector l m), bdy@(Body a b c e)) = Body a b newvel e
	where
		getvel = Vector ((l/e)*dt) ((m/e)*dt)
		newvel =  c `plus` getvel	

updatePos :: Body -> Body
updatePos (Body x y v@(Vector vx vy) m) = Body ( x+ (dt * vx) ) ( y+ (dt * vy) ) v m

distance :: Body -> Body -> Double
distance (Body a b c e) (Body a1 b1 c1 e1) = sqrt ((a-a1)^2 + (b-b1)^2)

b1 = newBody "1.4960e+11 0.0000e+00 0.0000e+00 2.9800e+04 5.9740e+24"
b2 = newBody "2.2790e+11 0.0000e+00 0.0000e+00 2.4100e+04 6.4190e+23" 
b3 = newBody "5.7900e+10 0.0000e+00 0.0000e+00 4.7900e+04 3.3020e+23"
b4 = newBody "0.0000e+00 0.0000e+00 0.0000e+00 0.0000e+00 1.9890e+30"
b5 = newBody "1.0820e+11 0.0000e+00 0.0000e+00 3.5000e+04 4.8690e+24"

qa = Quad (0) (0) (2.50e+11 *2)
arr = [b1,b2,b3,b4,b5]

makeTree :: [Body] -> BHTree
makeTree [x] = inserts x (Empty qa) 
makeTree (x:xs) = inserts x (makeTree xs) 

doSteps ::Int -> [Body] -> [Body]
doSteps 0 bs = bs
doSteps s bs = doSteps (s - 1) new_bs
	where
		tr = makeTree bs
		maped =  map (\c -> forces c tr) bs
		zipped = zip maped bs
		new_bs = map (updatePos.updateVel) zipped
        
t = inserts b1 (Empty qa) 
t1 = inserts b2 t
t12 = inserts b3 t1
t13 = inserts b4 t12
tt = inserts b5 t13

force = map (\c -> forces c tt) arr 
tuples = zip force arr
new_bs = map (updatePos.updateVel) tuples

main = do 
        handle <- openFile "test.txt" ReadMode
        contents <- hGetLine handle
        let bodies = words contents
            nb = f bodies
        contents <- hGetLine handle
        let singlewords = words contents
            s = func singlewords
        let qa = Quad 0 0 (s *2)

        contents <- hGetContents handle
        let list = (map(newBody) (lines contents))

        let domain = doSteps 1262 list
        print domain
        hClose handle
f :: [String] -> Int
f (x:xs) = (read x::Int)
func :: [String] -> Double
func (x:xs) = (read x::Double)