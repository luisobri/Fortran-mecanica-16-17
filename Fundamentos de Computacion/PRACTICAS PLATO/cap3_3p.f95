!progrma para el calculo de una función exponencial mediante el desarrollo en series de taylor
program cap_3p
	integer::x,killer,n,i
    double precision::fact
	print*, 'introduzca x'
    read*,x
    print*,'introduzca el grado del polinomio de taylor'
    read*,n
    do
    	x=0
        killer=0
        n=0
        i=0
        do i=n,1,-1
          do i
          