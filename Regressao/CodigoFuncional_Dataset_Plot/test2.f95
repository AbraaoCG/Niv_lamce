program main
	implicit none (type, external)
        integer :: i,j, nlines,nvar, neq,iout, xmouse,ymouse
        integer  , allocatable :: datas(:)
        real*8   , allocatable :: predictors(:,:),petr3(:,:),tpredictors(:,:),petr3_pred(:)
        real*8   , allocatable :: A(:,:),b(:,:),x(:)
        real*8  :: SQT, SQR, R2, R2adj, Yavg
        character*20 header(4)
	
	external lingauss

        open  (unit=21,file='output22.txt',form='formatted')
        open  (unit=22,file='output.txt',form='formatted')

    	nlines = 188       ! number of acquisitions from dataset.txt file
    	nvar   = 2       ! Number of predictors from dataset.txt file
    
    	neq    = nvar + 1  ! adding the bias coefficient
    	
        read(21,*) (header(i), i = 1,4)

	!Inicializando matrizes e vetores
    allocate  (predictors(1:nlines,0:nvar))  ; predictors(:,:)  = 0.d0
    allocate  (tpredictors(0:nvar,1:nlines)) ; tpredictors(:,:) = 0.d0
    allocate  (datas(nlines))                ; datas(:) = 0
    allocate  (petr3(nlines,1))              ; petr3(:,:) = 0.d0
    allocate  (petr3_pred(nlines))         ; petr3_pred(:) = 0.d0
    
    
    
    allocate  (A(neq,neq)) ; A(:,:) = 0.d0
    allocate  (b(neq,1))   ; b(:,:) = 0.d0
    allocate  (x(neq))     ; x(:) = 0.d0


	rewind (21)
	read (21,*)

    do i=1, nlines 
        predictors(i,0) = 1.d0 ! Bias coefficient
        read (21,*) datas(i),  (predictors(i,j),j=1,nvar) , petr3(i,1)
    enddo

 !...Begin Multiple Regression Algorithm
    
    tpredictors(:,:) = transpose(predictors(:,:))
    
    A(:,:) = matmul(tpredictors,predictors) ! Gˆt * G
    
    b(:,:) = matmul(tpredictors,petr3) ! Gˆt * R 
    
    
    call lingauss (A, x, b, neq, iout) ! Solving the system Ax=b
    
	

    !...End Multiple Regression Algorithm    

    
    if (iout .eq. 0) then ! Post processing
    
        write(*,*) 'Solution is done'
        
        do i = 1, neq
            write(*,'(a4,i4,a3,e12.5)') 'A(',i-1,')= ', x(i) ! printing Regression Coeficients 
        enddo
        
        
        petr3_pred(:) = 0.d0
        do i = 1, nlines
           
           do j = 1, nvar+1                   
              petr3_pred(i)   =  petr3_pred(i) +  x(j)*predictors(i , j-1 ) ! evaluating Ypred=A*X       
           enddo  
           
        enddo
    
        write(22,*)'acquisitions       data                    prediction '
        do i = 1, nlines
            write(22,'(i5,f10.2,f10.2)') i,petr3(i,1),petr3_pred(i) ! printing Y data, Y pred
        enddo
    
        
        SQT=0.d0
        Yavg = sum(petr3(1:nlines,1:1))/nlines
        
        do i = 1, nlines
           SQT = SQT  + ( petr3(i,1) - Yavg )**2  ! Total Variance    
        enddo  
        SQT = SQT/nlines
        
        SQR=0.d0
        do i = 1, nlines
           SQR = SQR  + ( petr3(i,1) - petr3_pred(i) )**2  ! residual Variance    
        enddo  
        SQR = SQR/nlines
        
        
        R2 = (SQT - SQR)/SQT
        
        R2adj = 1.d0 - (1.d0-R2)*(nlines-1)/(nlines-nvar-1) 
        
        
        
        write(*,'(//,a7,F10.4)') 'R2    =',R2      ! printing R2 metric
        write(*,'(a7,F10.4)') 'R2adj =',R2adj      ! printing R2 ajusted 
        
    endif
    
 
 


	
end program main

subroutine lingauss (a, x, b, n, iout)
          implicit none
          
          Integer :: n, i, j, k, iout
          Real*8 :: a(n,n), b(n), x(n)
          Real*8 , allocatable :: c(:)
          Real*8 :: d
          
          iout=0
          
          Allocate (c(n)) ; c(:)=0.0d0
          
          
          do k=1, n-1
              do i=k+1, n
                  if(a(k,k) /= 0) then
                      b(i) = b(i) - a(i,k)/a(k,k)*b(k)
          
                  else
                      iout=1
                      write(*,*) 'ERROR: matrix is singular'
                      return
                  endif
          
                  d = a(i,k)
                  do j=1, n, 1
                      a(i,j) = a(i,j) - a(k,j)*(d/a(k,k))
                  enddo
              enddo
          enddo
          
          
          do i=n, 1, -1
              do j=1, n, 1
                  if(j /= i ) then
                      c(i) = c(i) + a(i,j)*x(j)
                  else
                      cycle
                  endif
              enddo
              x(i) = (b(i) - c(i))/a(i,i)
          enddo
          
          
          return
    end subroutine lingauss


