!Elaborar un programa en FORTRAN que cargue dos vectores ordenados
!descendentemente y genere un único vector ordenado descendentemente mezclando los
!dos vectores anteriores, mostrándolo por monitor. No usar ningún algoritmo de
!ordenación para obtenerlo
module mod_1
implicit none
contains
subroutine killer_loop()
    integer::killer
    killer=0

    print*, 'Salir? 1 - afirmativo'
    read*,killer
    if(killer==1) stop 'fin del programa'

end subroutine

subroutine vector_generator(COLS_A,COLS_B,vect_A,vect_B,vect_sol,DIMe)
	implicit none
    integer, intent(in)::COLS_A,COLS_B,DIMe
    real, intent(in),dimension(COLS_A)::vect_A
	real, intent(in),dimension(COLS_B)::vect_B
    real, intent(IN OUT),dimension(DIMe):: vect_sol
    integer :: LIMIT, INDEX, i, INDEX_BREAK, j,Inx
	
	!designación del limite del DO
	if (COLS_A<COLS_B) LIMIT=COLS_B
    if (COLS_A>COLS_B) LIMIT=COLS_A

    
	!Creación de la matriz solución
	main_do: do i=0, LIMIT, 1
      	
		!indices de las matrices
		INDEX=(2*i+1)
        Inx=i+1

		!Cuando no ha mas valores de la matriz A que añadir...
        if (COLS_A<Inx) then
            INDEX_BREAK=INDEX
        	do j=i,COLS_B+COLS_A
            	Inx=j+1
            	vect_sol(INDEX_BREAK) = vect_B(Inx)
                INDEX_BREAK=INDEX_BREAK+1
            end do
            exit main_do
        end if
        
		!Cuando no ha mas valores de la matriz B que añadir...
        if (COLS_B<Inx) then
            INDEX_BREAK=INDEX
        	do j=i,COLS_B+COLS_A
            	Inx=j+1
            	vect_sol(INDEX_BREAK) = vect_A(Inx)
                INDEX_BREAK=INDEX_BREAK+1
            end do
            exit main_do
        end if

        !comparación de los valores por defecto
		if (vect_A(Inx)> vect_B(Inx)) then
        	vect_sol(INDEX) = vect_B(Inx)
            vect_sol(INDEX+1) = vect_A(Inx)
        else
          	vect_sol(INDEX) = vect_A(Inx)
            vect_sol(INDEX+1) = vect_B(Inx)
        end if
    end do main_do
    
end subroutine

end module

program exam_2015
	use mod_1
	implicit none
    integer:: COLS_A, COLS_B, j, k, DIMe
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
        DIMe=COLS_A+COLS_B
		allocate(vect_sol(DIMe))
		
		!subrutinas
        call vector_generator(COLS_A,COLS_B,vect_A,vect_B,vect_sol,DIMe)
        print*, vect_sol
        
        !terminador del bucle
        call killer_loop()

    end do
end program exam_2015