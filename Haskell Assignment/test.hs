import Data.List
import System.IO

data Body = Body {
	x::Double,
	y::Double,
	vx :: Double,
	vy :: Double,
	m :: Double
}

data Accel = Accel{
	ax :: Double,
	ay :: Double
}


timeStep = 12

doSteps ::Int -> [Body] -> [Body]
doSteps 0 bs = bs
doSteps s bs = doSteps (s - 1) new_bs
	where
		new_bs = map (updatePos . updateVel) bs
		updatePos ( Body x y vx vy m) = Body ( x+ timeStep * vx ) ( y+ timeStep * vy ) vx vy m
 		updateVel b = foldl deductChange b (map ( accel b ) bs )
 		deductChange ( Body x y vx vy m) ( Accel ax ay ) = Body x y ( vx-ax ) ( vy-ay ) m
 		accel ( Body ix iy ivx ivy imass) ( Body jx jy jvx jvy jmass ) = Accel ( dx * jmass * mag ) ( dy * jmass * mag )
																	 		where
																				mag = timeStep / ( dSquared * distance )
																				distance = sqrt ( dSquared )
																				dSquared = dx*dx + dy*dy
																				dx = ix - jx
																				dy = iy - jy