program cap5_7p
	implicit none
    integer:: COLS,j
    real,allocatable,dimension(:)::vect,vect_sol

    do
      
		
		!Lectura de la dimensión y de la propia matriz
        print*, 'introduce el numero de columnas'
        read*, COLS
        allocate(vect(COLS),vect_sol(COLS))
            do j=1,COLS
                print*,'introduce el elemento: (',j,')'
                read*, vect(j)
            end do


        !subrutinas
		call move_to_left(COLS,vect,vect_sol)
		call print_matrix_point(COLS,vect_sol)
        call killer_loop()
    end do
end program

subroutine killer_loop()
    integer::killer
    killer=0

    print*, 'Salir? 1 - afirmativo'
    read*,killer
    if(killer==1) stop 'fin del programa'

end subroutine

subroutine move_to_left(COLS,vect,vect_sol)
	implicit none
    integer, intent(in)::COLS
    real, intent(in),dimension(COLS):: vect
    real, dimension(COLS),intent(out):: vect_sol
	integer::i,j


	!bucle de rotación
    do i=1,(COLS-1)
      if (i==1) then
        vect_sol(COLS)=vect(1)
      else
      j=i+1
	  vect_sol(i)= vect(j)
      end if
    end do
end subroutine

subroutine print_matrix_point(COLS,vect_sol) 
    integer, intent(in):: COLS
    real, intent(inout),dimension(COLS):: vect_sol
    integer::j
	
	do j=1,COLS
    	print*,'elemento: (',j,')'
        print*, vect_sol(j)
    end do
end subroutine
    
