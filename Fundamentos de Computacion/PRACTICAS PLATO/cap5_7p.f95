program cap5_7p
	implicit none
    integer:: COLS,j
    real,allocatable,dimension::vect(:)

    do
      
		
		!Lectura de la dimensión y de la propia matriz
        print*, 'introduce el numero de columnas'
        read*, COLS
        allocate(VECT(COLS))
            do j=1,COLS
                print*,'introduce el elemento: (',j,')'
                read*, mat(j)
            end do


        !subrutinas
		call move_to_left(vect,COLS)
		
        call killer_loop()
    end do
end program

subroutine move_to_left(vect,COLS)
	implicit none
    INTEGER,intent(in)::COLS
    real,dimension(COLS),intent(inout)::vect
    real::aux_1,aux_2

    do j=1,COLS
    	if (j==COLS) then
        	
			vect(j+1)=aux
        	vect(j+1)=vect(j)
        
    end do
        
    
    