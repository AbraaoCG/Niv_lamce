Program Reg_Multivariada
external :: sgesv
real :: a(2,2),b(2,1),c(2,1)

a = reshape([ 2., 2., 1., 1. ], [ 2, 2 ])
b = reshape( [2,2] ,[2,1] )

c = matmul(a,b)

print '("Solution (x1, x2): ",f0.4)', c

end program Reg_Multivariada