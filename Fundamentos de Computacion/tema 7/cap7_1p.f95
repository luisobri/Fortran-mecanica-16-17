program cap7_1p
	implicit none
    integer::i,a


    open(UNIT = 20, FILE='cap7_1p_out.TXT', STATUS='NEW', ACTION='WRITE',iostat=a)	!SIN EL .TXT FORTRAN ES INCAPAZ DE TRABAJAR CON EL ARCHIVO
    print*,a
    

	do i=1,100
    	write(20,*) i
    end do

    close(UNIT=20)
    







end program  