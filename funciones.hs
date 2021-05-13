memoriaRam m = (m*500)

procesador proce  
		| (proce == "i3") = 4000
		| (proce == "i5") = 6000
		| (proce == "i7") = 7000

tarjetaM tarjt
		| (tarjt == "graf") = 3000
		| (tarjt == "ngraf") = 2000

discoduro hd = (hd/500) * 1000

gabinete gabi
		| (gabi == "simple") = 500
		| (gabi == "doble") = 800

precioComp mem cpu moth hdd gab = (memoriaRam mem) + (procesador cpu) + (tarjetaM moth) + (discoduro hdd) + (gabinete gab)