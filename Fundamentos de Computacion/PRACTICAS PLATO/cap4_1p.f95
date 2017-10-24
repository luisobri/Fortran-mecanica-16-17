program cap4_1p
	implicit none
	integer::i,j,grad_pol=0
    print*, 'grado del polinomio?'
    read*,grad_pol
    double precision, dimension(0:grad_pol)::coeficientes
    double precision::val_now, sum, X
    grad_pol=0
    
    print*,'introduce valor de X'
    read*,X
    coeficientes=0
    lectura_pol: do i=0,grad_pol,1
      	val_now=0
      	print*,'introduce el coeficiente de grado',i
        read*,val_now
		coeficientes(i)= val_now
    end do lectura_pol

    operacion: do j=0,grad_pol,1
    	sum=coeficientes(i)*X**i
    end do operacion
    print*, 'f(',X,')	=	',sum

end program