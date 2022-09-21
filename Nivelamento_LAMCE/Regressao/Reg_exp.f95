
Program polyfit


integer :: i, ipred

parameter (np = 30)

real*8 :: x(np) , y(np) , a0, a1, det, y_avg, R2 , a0R, sumy, yBase(np), yPred(np)
!a0R é usado como a0Real para Reg. Exponencial


open (unit = 1, file = "dataset.txt" )
open (unit = 2, file = "results.txt" )


write(*,*) 'Enter: =1 (Linear Regression) ; =2 (LN regression) ; 3 (Exp. Regression)'
read(*,*) ipred



!.... Begin Reading dataset.txt file + Sumy for average
sumy = 0
do i = 1, np
    read (1,*) x(i), y(i)
    sumy = sumy + y(i)
enddo

yBase = y
!.... End: Reading dataset.txt file


if (ipred .eq. 1) print*, ' Linear Regression'


if (ipred .eq. 2) then
    print*, ' LN Regression'
   
    do i = 1, np
        x(i) = log (x(i))
    enddo
endif
if (ipred .eq. 3) then 
    print*, ' Exp. Regression'
    
    do i = 1, np
     	y(i) = log (y(i))
    enddo

endif


!Inicio de Regressao Linear, que serve para todas as 3 regressões.

det = np*sum(x**2) - sum(x)**2

if (abs(det).le.0.000001) then
    print* , 'ERROR: Determinant is null !!!'
    print* , 'ERROR: Stop program'
    stop
endif


A0  = ( -sum(x)*sum(x*y) + sum(y)*sum(x**2)   ) / det 
A1  = (   np*sum(x*y) - sum(x)*sum(y)         ) / det


!Se for Regressao Exponencial, A0Real = eˆA0
if (ipred .eq. 3) then
	A0 = EXP(A0)
endif

do i = 1, np
    if (ipred .eq. 1)  then
	yPred(i) = A0+A1*x(i)
	write (2,*) x(i), y(i), yPred(i)
    endif
    if (ipred .eq. 2)  then
	yPred(i) = A0+A1*x(i)
	write (2,*) exp(x(i)), y(i), yPred(i)
    endif
    if (ipred .eq. 3)  then 
	yPred(i) = A0 * EXP(A1 * x(i))
	write (2,*) x(i), exp(y(i)), yPred(i)
    endif
enddo


!A0 * EXP(A1 * x(i))

y_avg = sum(yBase)/np

R2 = 1 - (   sum( ( yBase - yPred )**2 ) / sum( (yBase - y_avg)**2 )    )


write (*,*) 'A0 = ', A0, 'A1 = ', A1
write (*,*) 'R2 = ', R2


End Program polyfit
