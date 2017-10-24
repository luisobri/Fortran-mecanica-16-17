program cap1_2p
	real::dist_pul,dist_cent
    real, parameter::ccpc=2.54
    write(*,*)'introduce la longitud en pulgadas que desea convertir a centimetros'
    read(*,*) dist_pul
    dist_cent=dist_pul*ccpc
    write(*,*)'la longitud',dist_pul,'"  ','en centimetros es',dist_cent,'cm'
end