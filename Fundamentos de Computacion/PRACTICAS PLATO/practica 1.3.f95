IMPLICIT NONE
INTEGER :: l,m,resul
PRINT*,'TECLEA 2 NUMEROS ENTEROS SEPARADOS CON INTRO'
READ*,l,m
resul=l**m
PRINT*,'EL RESULTADO ES:'
PRINT*,resul
STOP
END PROGRAM