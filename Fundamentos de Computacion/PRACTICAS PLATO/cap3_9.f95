PROGRAM cap3_9
IMPLICIT NONE
!declaración de variables
INTEGER :: num,i,killer
double precision::fact


loop: do
	WRITE(*,*) 'DAME UN NUMERO'
	READ (*,*) num
	IF (num<0) THEN
		WRITE(*,*) 'NO EXISTE EL FACTORIAL DE UN NEGATIVO'
	ELSE IF (num==0) THEN
		WRITE(*,*) 'EL FACTORIAL DE CERO ES UNO'
		ELSE
			fact=1
			interno: DO i=num,1,-1
			fact=fact*i
			WRITE(*,*) 'RESULTADO PARCIAL',fact
		END DO interno
		WRITE(*,*) 'EL FACTORIAL DE',num,' ES ',fact
	END IF
	print*,'salir? 1 - Sí 2 - No'
	read*, killer
    if (killer==1) EXIT
end do loop
END PROGRAM cap3_9