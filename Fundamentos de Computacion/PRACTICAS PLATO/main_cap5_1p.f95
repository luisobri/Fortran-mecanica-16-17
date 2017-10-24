program main_cap5_1p
	!LSF declaración de variables
    implicit none
    integer:: killer
    double precision:: a, b
    logical:: cuadron_perfecto_evaluation

    
	!LSF main loop 
    do
      print*, 'introduce el primer valor'
      read*, a
      print*, 'introduce el segundo valor a evaluar'
      read*, b


      !LSF evaluación de los numeros
      !LSF devoluciónd de resultdos
      if (cuadron_perfecto_evaluation (a,b)) then
		print*, 'son cuadrones'
      else
        print*, 'no son cuadrones'
      end if

      !LSF end main loop
      print*,'Salir? 1 - afirmativo'
      read*, killer
      if (killer==1) exit
    end do
end program