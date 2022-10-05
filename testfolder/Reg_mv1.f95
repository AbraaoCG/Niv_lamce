Program Reg_Multivariada
external :: sgesv
real :: a(2,2),b(2,1),c(2,1)

!a = reshape([ 2., 2., 1., 1. ], [ 2, 2 ])
!b = reshape( [2,2] ,[2,1] )

!c = matmul(a,b)

!print '("Solution (x1, x2): ",f0.4)', c

!Tentando Fazer de fato

integer :: i,j
parameter (np = 30)
parameter(num_var = 2)

real*8 :: x0(np * num_var), x(np),x2(np), y (np) , ypred(np)
real :: Ma(np,num_var), MaT(num_var,np)


open (unit = 1, file = "dataset.txt" )
open (unit = 2, file = "results.txt" )

! Ler dataset

read(1,*) x0

!Ma = reshape(x0, [np,num_var] )

!do i = 1, 2
!	do j = 1,2
!		read(1,*)  Ma(i,j) 
!	enddo
!enddo

Ma = reshape(x0, [2,num_var] )






print '(f0.4 )',Ma(1,:)


end program Reg_Multivariada