program cap1_1.2p
    implicit none
    real::lon_bas,are_bas,radio,h,vol_con
    real, parameter::pi=3.141592
    write(*,*)'introduce la altura del cono'
    read(*,*) h
    write(*,*)'usted a definido la altura como',h,'m'
    write(*,*)'introduce el radio de la base'
    read(*,*) radio
    write(*,*)'usted a definido el radio de la base como',radio,'m'
    lon_bas=2*pi*radio
    are_bas=pi*(radio**2)
    vol_con=(are_bas*h)/3
    write(*,*)' '
    write(*,*)'longitud de la base  ',lon_bas,'m'
    write(*,*)'area de la base      ',are_bas,'m'
    write(*,*)'volumen del cono     ',vol_con,'m'

end program cap1_1.2p
