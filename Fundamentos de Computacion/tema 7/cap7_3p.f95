!Programa que escriba las temperaturas de los días de una semana en
!un archivo con nombre cap7_3p_out.txt.

program cap7_3p
	implicit none

    !call file_openner_for_write
    !call temp_writer
    !close(3)

	call file_openner_for_read
    call general_info_writer
    close(4)
    
end program



subroutine file_openner_for_write
	implicit none
	integer::a
	!apertura del archivo de almacenado de las temperaturas
    open(UNIT= 3, FILE='cap7_3p_out.TXT', STATUS='old', ACTION='WRITE', position = 'append',iostat=a)
    if(a/=0) stop 'error al crear archivo cap7_3p_out.TXT'
end subroutine

subroutine file_openner_for_read
	implicit none
	integer::a
	!apertura del archivo de almacenado de las temperaturas
    open(UNIT= 4, FILE='cap7_3p_out.TXT', STATUS='OLD', ACTION='READWRITE', position = 'REWIND',iostat=a)
    if(a/=0) stop 'error al crear archivo cap7_3p_out.TXT'
end subroutine


!escirtura de variable leida por teclado en fichero abierto
subroutine temp_writer
	implicit none
	integer,dimension(8):: values
    integer:: killer,a
    real:: temp_actual

    
	do
    	!lectura de la fecha del apunte y de la temperatura por teclado
    	temp_actual=0
      	call test_time_and_date(values)
		Print*, 'introduzca valor de la temperatura del día', values(1:3)
        Read*, temp_actual
		
		!escritura de los datos en el archivo
		!2 format (2X, I4, 3X, F6.3)
        write(3,*,iostat = a) values(1:3), temp_actual
        if (a /= 0) print*, 'error de escritura'

        !condición de bucle
        print*, 'continuar? 1 - no'
        read*, killer
        if (killer == 1) exit

    end do
end subroutine


    
!subrutina para obtener fecha y hora del computador
subroutine test_time_and_date(values)
	implicit none
	character(8)  :: date
    character(10) :: time
    character(5)  :: zone
    integer, intent(out), dimension(8):: values
              ! using keyword arguments
              call date_and_time(date,time,zone,values)
              call date_and_time(DATE=date,ZONE=zone)
              call date_and_time(TIME=time)
              call date_and_time(VALUES=values)

     !print '(8i5)', values
end subroutine test_time_and_date


subroutine general_info_writer
	!Programa que lea las temperaturas de los días de la semana del
	!ejercicio anterior y escriba en otro archivo con nombre cap7_4p_out.txt:
	!-	 Temperatura media = valor con dos cifras decimales.
	!-	 Temperatura mínima = valor y nombre del día de la semana correspondiente.
	!-	 Temperatura máxima = valor y nombre del día de la semana correspondiente.
	!¿Cómo se añadiría la información anterior en el archivo de entrada?
	implicit none
    integer:: rows, cols, i, j
    real, dimension(:,:),allocatable:: text_read_array
    real, dimension(1:4)::max, min
    double precision:: suma, media

	!creación de la matriz de almacenaje
    call file_reader (rows,cols)
    allocate(text_read_array(cols,rows))
    text_read_array = 0
    


    !escritura en la matriz de almacenaje
    rewind 4
    do i = 1, rows
		read(4,*) text_read_array(1:4,i)
    end do

    
	min = 0
    max = 0
    suma = 0

    
    do j = 1, rows
      	!acumulador para la media de temperaturas
		suma = suma + text_read_array(4,j)

        !conprobación para saber si es maximo historico
        if (j==1) max(1:4) = text_read_array(1:4,j)
        if (text_read_array(4,j) > max(4)) max(1:4) = text_read_array(1:4,j)

        !conprobación para saber si es minimo historico
        if (j==1) min(1:4) = text_read_array(1:4,j)
        if (text_read_array(4,j) < min(4)) min(1:4) = text_read_array(1:4,j)
    end do

    write(4,*) 'Temperatura mínima =', min(1),min(2),min(3),min(4)
    write(4,*) 'Temperatura maxima =', max(1), max(2), max(3), max(4)
    media = real(suma)/rows
    write(4,*) 'Temperatura media =', media

    write(*,*) 'Temperatura mínima =', min(1),min(2),min(3),min(4)
    write(*,*) 'Temperatura maxima =', max(1), max(2), max(3), max(4)
    write(*,*) 'Temperatura media =', media

    CALL temp_organizer


end subroutine



!subrutina para la lectura integra de archivo del archivo de texto 
subroutine file_reader (rows,cols)
	implicit none
    integer, intent(out):: cols, rows
    integer::a
    integer:: float_information

    cols=4
    rows=0


	!bucle principal para obtener el numero de filas
    Do
    	Read(4,*,iostat = a) float_information
        if(a<0) exit
        if(a>0) exit
        rows = rows + 1
    end do
    print*, rows, cols

end subroutine


!subrutina para organizar las temperaturas por orden
subroutine temp_organizer
	implicit none
    

    
    
    