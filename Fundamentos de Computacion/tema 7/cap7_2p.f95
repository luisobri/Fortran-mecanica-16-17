!Programa que abra el archivo del ejercicio anterior, calcule el
!cuadrado de cada número y lo escriba en un archivo con nombre
!cap7_2p_out.txt. ¿Cómo se escribiría todo en el mismo archivo?
!Para comodidad del usuario, copiar cap7_1p_out.txt en
!cap7_2p_inout.txt antes de ejecutar el programa.

Program cap7_2p
	implicit none
    real:: val_now
    integer::a,b,c,a1,c1

    open(UNIT= 3, FILE='cap7_1p_out.TXT', STATUS='OLD', ACTION='READ',iostat=a, POSITION= 'REWIND')
    open(UNIT= 4, FILE='cap7_2p_out.TXT', STATUS='NEW', ACTION='write',iostat=b)
    open(UNIT= 5, FILE='cap7_2p_inout.TXT', STATUS='OLD', ACTION='READWRITE',iostat=c, POSITION= 'REWIND')
    print*,a
    print*,b
    print*,c


    
    do
    	read(3,*, iostat = a1) val_now
        if (a1<0) exit
        val_now =  val_now**2
        write(4,*) val_now
        

    end do

    do
      	read(5,*, iostat = c1) val_now
        if (c1<0) exit
        val_now =  val_now**2
        backspace 5
        30 format(T30,F10.5)
        write(5,30) val_now
        
    end do


        close(UNIT=3)
        close(UNIT=4)
        close(UNIT=5)

	call test_time_and_date



end program



subroutine test_time_and_date
              character(8)  :: date
              character(10) :: time
              character(5)  :: zone
              integer,dimension(8) :: values
              ! using keyword arguments
              call date_and_time(date,time,zone,values)
              call date_and_time(DATE=date,ZONE=zone)
              call date_and_time(TIME=time)
              call date_and_time(VALUES=values)
              print '(a,2x,a,2x,a)', date, time, zone
              print '(8i5)', values
          end subroutine test_time_and_date