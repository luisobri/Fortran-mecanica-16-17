program cap1_2p
	IMPLICIT NONE
	real::diametro,lon_cir,are_cir,radio
    real, parameter::pi=3.141592
    write(*,*)'introduce el diametro de la circunferencia'
    read(*,*) diametro
    write(*,*)'usted a definido el diametro como',diametro,'m'
    radio=diametro/2
    lon_cir=2*pi*radio
    are_cir=pi*(radio**2)
    write(*,*)'radio de la circunferencia	',radio,'m'
    write(*,*)'longitud de la circunferencia	',lon_cir,'m'
    write(*,*)'area de la circunferencia	',are_cir,'m'
end
