module mod_aux
contains
subroutine dinamic_array_read(matriz_A, ROWS_A, COLS_A)
	implicit none
	integer, INTENT(OUT):: ROWS_A,COLS_A
    integer::j,i
	real, INTENT(OUT),dimension(:,:), allocatable::matriz_A
    

		print*, 'introduce el numero de filas de la matriz A'
        read*, ROWS_A
        print*,'introduce el numero de columnas de la matriz A'
        read*, COLS_A
        
        allocate(matriz_A(ROWS_A,COLS_A))
        do i=0,COLS_A
            do j=0,ROWS_A
                print*,'introduce el elemento: (',ROWS_A,',',COLS_A,')'
                read*, matriz_A(j,i)
            end do
        end do

end subroutine
end module    