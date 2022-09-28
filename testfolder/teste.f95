! example.f90
! Example program that solves the matrix equation AX = B using LAPACK.
program main
    implicit none (type, external)
    external :: sgesv
    real     :: a(2, 2)  ! Matrix A.
    real     :: b(2)     ! Vector b/x.
    real     :: pivot(2) ! Pivot indices (list of swap operations).
    integer  :: rc       ! Return code.
	integer :: i,j
	real :: Ma(3,3), MaT(3,3)

	Ma = reshape ([1,2,3,4,5,6,7,8,9] , [3,3])

    a = reshape([ 2., 3., 1., 1. ], [ 2, 2 ])
    b = [ 5., 6. ]

    call sgesv(2, 1, a, 2, pivot, b, 2, rc)

    if (rc /= 0) then
        print '(a, i0)', 'Error: ', rc
        stop
    end if
	! open(unit = 1, file = "testeresults.txt")
	do i = 1,3
		do j = 1,3
			MaT(i,j) = Ma(j,i)
		enddo
	enddo
	print '(f0.4 )',MaT(:,2)

    !print '("Solution (x1, x2): ", f0.4, ", ", f0.4)', b
end program main
