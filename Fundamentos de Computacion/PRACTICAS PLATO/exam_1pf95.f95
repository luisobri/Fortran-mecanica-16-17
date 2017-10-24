program cap5_7p
	implicit none
    integer:: COLS_A, COLS_B,j,k
    real,allocatable,dimension(:)::vect_A
    real,allocatable,dimension(:)::vect_B
    real,allocatable,dimension(:)::vect_sol
 

    do
      
		!Lectura de la dimensión y del vector A
        print*, 'introduce el numero de columnas'
        read*, COLS_A
        allocate(vect_A(COLS_A))
            do j=1,COLS_A
                print*,'introduce el elemento: (',j,')'
                read*, vect_A(j)
            end do
            
		!Lectura de la dimensión y del vector B
        print*, 'introduce el numero de columnas'
        read*, COLS_B
        allocate(vect_B(COLS_B))
            do k=1,COLS_B
                print*,'introduce el elemento: (',k,')'
                read*, vect_B(k)
            end do

        !creación del espacio de memoria para el vector solución
        allocate(vect_sol(COLS_A+COLS_B))
		
		!subrutinas
        call vector_generator(COLS_A,COLS_B,vect_A,vect_B,vect_sol)
        print*, vect_sol
        
        !terminador del bucle
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

subroutine vector_generator(COLS_A,COLS_B,vect_A,vect_B,vect_sol)
	implicit none
    integer, intent(in)::COLS_A,COLS_B
    real, intent(in),dimension(COLS_A)::vect_A
	real, intent(in),dimension(COLS_B)::vect_B
    real, intent(inout),dimension(COLS_A+COLS_B):: vect_sol
    integer:: LIMIT, INDEX, i, INDEX_BREAK, j
	
	!designación del limite del DO
	if (COLS_A<COLS_B) LIMIT=COLS_B
    if (COLS_A>COLS_B) LIMIT=COLS_A

    
	!Asignación de los valores de la subroutine
    main_do: do i=0, LIMIT 
      	INDEX=(2*i+1)
        if (COLS_A<i) then
            INDEX_BREAK=INDEX
        	do j=i,COLS_B
            	vect_sol(INDEX_BREAK) = vect_B(j+1)
                INDEX_BREAK=INDEX_BREAK+1
            end do
            exit main_do
        end if
        if (COLS_B<i) then
            INDEX_BREAK=INDEX
        	do j=i,COLS_A
            	vect_sol(INDEX_BREAK) = vect_A(j+1)
                INDEX_BREAK=INDEX_BREAK+1
            end do
            exit main_do
        end if
		if (vect_A(i+1)>vect_B(i+1)) then
        	vect_sol(INDEX) = vect_B(i+1)
            vect_sol(INDEX+1) = vect_A(i+1)
        else
          	vect_sol(INDEX) = vect_A(i+1)
            vect_sol(INDEX+1) = vect_B(i+1)
        end if
    end do main_do
    
end subroutine 
		
        
