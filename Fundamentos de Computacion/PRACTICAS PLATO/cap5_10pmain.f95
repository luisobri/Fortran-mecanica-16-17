!porgrama definitivo creado y editado 12/12/2016
program cap5_10pmain
	implicit none
    integer:: ROWS, COLS, j, i, k, v, selector, A
    real, allocatable, dimension(:,:)::mat_A, mat_B,mat_ans

	A=0
do 
	if(A==0) then
    	!lectura de matrizes por teclado dinamica
		print*, 'introduce el numero de filas de las matrizes'
        read*, ROWS
        print*,'introduce el numero de columnas de las matrizes'
        read*, COLS

		selector=0
		!asignación de dimensión de espacio de memoria
        allocate(mat_A(COLS,ROWS),mat_B(COLS,ROWS))

        !lectura de la matriz A
        do i=1,ROWS
            do j=1,COLS
                print*,'introduce el elemento de la matriz A: (',j,',',i,')'
                read*, mat_A(j,i)
            end do
        end do
        
        !lectura de la matriz B
        do k=1,ROWS
            do v=1,COLS
                print*,'introduce el elemento de la matriz B: (',k,',',v,')'
                read*, mat_B(v,k)
            end do
        end do
        A=1 !variable de control de flujo
        
     end if

do
        call menu_print()
        read*, selector
        

	select case (selector)
    	case (1)
        	print*, '1. Mostrar por monitor la Matriz A.'
			call matrix_print(mat_A,COLS,ROWS)
		case (2)
        	print*, '2. Mostrar por monitor la Matriz B.'
			call matrix_print(mat_B,COLS,ROWS)
        case (3)
        	allocate(mat_ans(ROWS,COLS))
            print*, '3. Mostrar por monitor la traspuesta de la Matriz A.'
            call matrix_trans(mat_A,mat_ans,COLS,ROWS)
            call matrix_print(mat_ans,ROWS,COLS)
        case (4)
         	allocate(mat_ans(ROWS,COLS))
            print*, '4. Mostrar por monitor la traspuesta de la Matriz B.'
            call matrix_trans(mat_B,mat_ans,COLS,ROWS)
            call matrix_print(mat_ans,ROWS,COLS)
        case (5)
        	allocate(mat_ans(COLS,ROWS))
        	print*, '5. Mostrar por monitor Matriz A + Matriz B.'
			mat_ans=mat_A+mat_B
            call matrix_print(mat_ans,COLS,ROWS)
        case (6)
        	allocate(mat_ans(COLS,ROWS))
        	print*, '6. Mostrar por monitor Matriz A - Matriz B.'
            mat_ans=mat_A-mat_B
            call matrix_print(mat_ans,COLS,ROWS)
        case (7)
        	print*,'7. resetear matrices'
			deallocate(mat_ans,mat_A,mat_B)
            A=0
            exit
        case (8)
        	print*, '8. Salir del programa'
        	call killer_loop
        case default
        	print*,'error selección'
	end select
    
    deallocate(mat_ans)
    
end do 
end do  	

end program

subroutine menu_print()
	print*, 'teclee el numero de la operación a realizar:'
	print*, '1. Mostrar por monitor la Matriz A.'
	print*, '2. Mostrar por monitor la Matriz B.:'
	print*, '3. Mostrar por monitor la traspuesta de la Matriz A.'
	print*, '4. Mostrar por monitor la traspuesta de la Matriz B.'
	print*, '5. Mostrar por monitor Matriz A + Matriz B.'
	print*, '6. Mostrar por monitor Matriz A - Matriz B.'
    print*, '7. resetear matrices'
	print*, '8. Salir.'

end subroutine

subroutine matrix_print(mat,COLS,ROWS)
	integer,intent(in)::COLS,ROWS
	real,dimension(COLS,ROWS),intent(in)::mat
    integer::j,i

	!impresión de matriz con puntero
	do i=1,ROWS
        do j=1,COLS
        	print*,'matriz (',i,',',j,')','=',mat(j,i)
     	end do
     end do

end subroutine

subroutine matrix_trans(mat,mat_ans,COLS,ROWS)
	integer,intent(in)::COLS,ROWS
	real,dimension(COLS,ROWS),intent(in)::mat
	real,dimension(ROWS,COLS),intent(inout)::mat_ans
    integer::j,i

	do i=1,ROWS
        do j=1,COLS
			mat_ans(j,i)= mat(i,j)
     	end do
     end do

end subroutine
    
!bucle de finalización del programa
subroutine killer_loop
    integer::killer
    killer=0

    print*, 'Salir? 1 - afirmativo'
    read*,killer
    if(killer==1) stop 'fin del programa'

end subroutine
