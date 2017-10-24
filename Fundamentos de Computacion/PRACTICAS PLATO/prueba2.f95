PROGRAM cap1_2
INTEGER :: a
REAL :: b1,b2,sueldo_del_ultimo_mes
LOGICAL :: d1,d2
COMPLEX :: e
CHARACTER (LEN=18) :: pal
CHARACTER (LEN=180) :: frase_larga
a=-123
b1=-2.98
b2=0.9E-8
sueldo_del_ultimo_mes=2850.75
d1=.true.
d2=.false.
e=(2.3,3.7)
pal='CONSTANTE CARACTER'
frase_larga='"CONSTANTE CARACTER dividida en dos lineas usando &
&el caracter & al final de la primera y al principio de la siguiente"'
WRITE (*,*) 'CONSTANTE ENTERA',a
WRITE (*,*) 'CONSTANTES REALES (NOTAC NORMAL Y EXPON)',b1,b2
WRITE (*,*) 'IDENTIFICADOR DE VARIABLE REAL (MAX. 31 letras)',&
sueldo_del_ultimo_mes,'EUROS'
WRITE (*,*) 'CONSTANTES LOGICAS',d1,d2
WRITE (*,*) 'CONSTANTE COMPLEJA',e
WRITE (*,*) pal !OBSERVAR QUE NO SALEN LOS APOSTROFES
WRITE (*,*) frase_larga !AQUI SI SALEN LAS COMILLAS DOBLES
END PROGRAM cap1_2