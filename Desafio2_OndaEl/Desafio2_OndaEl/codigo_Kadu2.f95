!  wave1d.f90 
!
!  FUNCTIONS:
!  wave1d - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: wave1d
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

    program wave1d

    implicit none
    
    integer ::  neqmax ; parameter (neqmax=400)
    integer :: i, nume, numnp, neq, nsteps,  kn=100, iopt
    real*8  :: xL, rho, fi, fj, ui, uj, vi, vj, ai, aj, time, dt, area, Eyoung
    real*8  :: xk11, xk12, xk22, xM11, xM12, xM22, timetot, xLe
    real*8  :: omegan, omega, F0, damp1, damp2
    real*8  :: xKG(neqmax,neqmax), xMG(neqmax,neqmax), xCG(neqmax,neqmax)
    real*8  :: vel(neqmax,1), vel_aux(neqmax,1), disp(neqmax,1),  xMdiag_inv(neqmax,1)
    real*8  :: F(neqmax,1), fint_disp(neqmax,1), fdamp_vel(neqmax,1)
    real*8  :: a(neqmax,1),b(neqmax,1),c(neqmax,1),d(neqmax,1),x(neqmax,1)
    real*8  :: dpred(neqmax,1),vpred(neqmax,1),acel(neqmax,1)   
    
    nume   = neqmax
    numnp  = nume+1
    
    timetot= 10.0d0
    dt     = 2.0d-5
    
    nsteps = int(timetot/dt)
    
    Eyoung = 1.0d0
    Area   = 1.0d0 
    xL     = 1.0d0
    rho    = 1.0d0

    damp1  = 0.0d0
    damp2  = 0.0d0

    
    iopt   = 1     ! iopt = 0 (Euler) ; iopt = 1 (Newmark) 
    
    xLe    = xL/nume

    
    xk11 =  Eyoung*Area/xLe
    xk12 = -Eyoung*Area/xLe
    xk22 =  Eyoung*Area/xLe    

    if (iopt .eq. 0) then 
     
      xM11 = rho*Area*xLe/2.0d0
      xM12 = 0.0d0
      xM22 = rho*Area*xLe/2.0d0

    elseif (iopt .eq. 1) then 
      
      xM11 = rho*Area*xLe/3.0d0
      xM12 = rho*Area*xLe/6.0d0
      xM22 = rho*Area*xLe/3.0d0
    
    endif

    
    xKG(:,:) = 0.d0
    xMG(:,:) = 0.d0
    xCG(:,:) = 0.d0
    F(:,:)   = 0.d0
    
    omegan = dsqrt(xK22/xM22) ! 1 Grau de Liberdade 
    omega  = 0.00d0*omegan
    
    ! Caso 2 U(x=0) = 0, F(x=L) = Fj
    
    neq = numnp -1
    Ui = 0.d0
    Fj = 1.0d0
    
    a(:,:) = 0.d0
    b(:,:) = 0.d0
    c(:,:) = 0.d0
    d(:,:) = 0.d0
    x(:,:) = 0.d0
    
    do i = 1, neq -1   ! diagonal principal
      
      xKG(i,i) =  xk11 + xk22
      xMG(i,i) =  xM11 + xM22
      xCG(i,i) =  damp1*xMG(i,i) + damp2*xKG(i,i)
      
      b(i,1)     = xMG(i,i) + (0.25*dt**2)*xKG(i,i) + (0.5*dt)*(damp1*xMG(i, i) + damp2*xKG(i, i)) 
      
    enddo
    
      xKG(neq,neq) =  xk22
      xMG(neq,neq) =  xM22
      xCG(neq,neq) =  damp1*xMG(neq,neq) + damp2*xKG(neq,neq)
      
      b(neq,1)       =  xMG(neq,neq) + (0.25*dt**2)*xKG(neq,neq) + (0.5*dt)*(damp1*xMG(neq, neq) + damp2*xKG(neq, neq)) 
    
    do i = 1, neq -1    ! sub-diagonal superior
       xKG(i, i+1) =  xk12 
       xMG(i, i+1) =  xM12 
       xCG(i, i+1) =  damp1*xMG(i,i+1) + damp2*xKG(i,i+1)       
       
       c(i,1)        =  xMG(i, i+1)  + (0.25*dt**2)*xKG(i, i+1)  + (0.5*dt)*(damp1*xMG(i, i+1) + damp2*xKG(i, i+1)) 
    enddo 
    
    do i = 2, neq       ! sub-diagonal inferior
       xKG(i, i-1) =  xk12 
       xMG(i, i-1) =  xM12 
       xCG(i, i-1) =  damp1*xMG(i,i-1) + damp2*xKG(i,i-1)       
       
       a(i,1)        =  xMG(i, i-1)  + (0.25*dt**2)*xKG(i, i-1)  + (0.5*dt)*(damp1*xMG(i, i-1) + damp2*xKG(i, i-1)) 
    enddo 

    
    
    do i = 1, neq-1
      xMdiag_inv(i,1) = 1.d0/(xM11 +  xM22 )
    enddo
      xMdiag_inv(neq,1) = 1.d0/xM22
    
    
    F(neq,1) = Fj
    
    disp(:,:) = 0.d0
    vel (:,:) = 0.d0    
    
    time = 0.d0
    fint_disp(:,:) = 0.d0
    fdamp_vel(:,:)  = 0.d0
    acel(:,:)      = 0.d0
    f0             = Fj
    
    
    
    ! Begin Euler-Explicit (Lumped mass)
    
    if (iopt .eq. 0) then 

          open  (unit=55,file='Results2.txt',form='formatted')
          do i = 1 , nsteps   
          
             time = time + dt
             
             fint_disp(:,:) =  matmul(xKG,disp) 
             fdamp_vel(:,:) =  matmul((damp1*xMG),vel) 
             
             F(neq,1) = f0*dcos(omega*time)
             
             
             vel(:,1)  = vel(:,1)                          + &
			             xMdiag_inv(:,1)*F(:,1)*dt         - &
               			 xMdiag_inv(:,1)*fint_disp(:,1)*dt - &
						 xMdiag_inv(:,1)*fdamp_vel(:,1)*dt
             
             disp(:,1) = disp(:,1) + vel(:,1)*dt
             
            
             if (MOD(i-1,kn) .EQ. 0) write(55,*) time, disp(neq,1), vel(neq,1) 
              
          enddo
          ! End Euler Method
          
          
    elseif (iopt .eq. 1) then 
    
          open  (unit=55,file='Results2.txt',form='formatted')
          ! Begin Newmark-Implicit
          do i = 1 , nsteps   
          
              time = time + dt
               
              F(neq,1) = f0 !*dcos(omega*time)
              
              dpred(:,1) = disp(:,1) + dt*vel(:,1) + 0.25*dt**2*acel(:,1) 
              
              vpred(:,1) = vel(:,1) + 0.5*dt*acel(:,1)
              
                    fint_disp(:,:) =  matmul(xKG,dpred) 
                    
                    fdamp_vel(:,:) =  matmul((damp1*xMG + damp2*xKG),vpred) 
                    
                    d(:,1) = F(:,1) - fint_disp(:,1) - fdamp_vel(:,1)
                    
                    call solve_tridiag(a,b,c,d,acel,neq)     ! ( [M] + 0.5*dt*[damp1*M + damp2*K] + 0.25*dt**2*[K] )* {acel} = {F_ext} - {Fint} 
              
              vel(:,1)  = vpred(:,1) + acel(:,1)*dt*0.5
              
              disp(:,1)  = dpred(:,1) + acel(:,1)*dt**2*0.25
              
              
              
              if (MOD(i-1,kn) .EQ. 0) then 
                 write(55,*) time, disp(neq,1), vel(neq,1)  ! ,';'  aqui
                 !call plotdisp(vel,neq,time,i,kn)
              endif
                 
              
          enddo
          ! End Newmark     
          
    endif
         
    
    
    !call plotmesh (numnp-1,xLe)

    if (iopt .eq. 0 ) write(*,*) "Wave1d.f90 Explicit Euler: ..... Solution is done"    
    if (iopt .eq. 1 ) write(*,*) "Wave1d.f90 Implicit Newmark: ... Solution is done"    
      
    end program wave1d
    

    
    
    
      subroutine solve_tridiag(a,b,c,d,x,n)
      implicit none

      !	a - sub-diagonal (diagonal abaixo da diagonal principal)
      !	b - diagonal principal
      !	c - sup-diagonal (diagonal acima da diagonal principal)
      !	d - parte à direita
      !	x - resposta
      !	n - número de equações

        integer,intent(in) :: n
        real(8),dimension(n),intent(in) :: a,b,c,d
        real(8),dimension(n),intent(out) :: x
        real(8),dimension(n) :: cp,dp
        real(8) :: m
        integer i

        x(:) = 0.d0
        
! inicializar c-primo e d-primo
        cp(1) = c(1)/b(1)
        dp(1) = d(1)/b(1)
! resolver para vetores c-primo e d-primo
         do i = 2,n
           m = b(i)-cp(i-1)*a(i)
           cp(i) = c(i)/m
           dp(i) = (d(i)-dp(i-1)*a(i))/m
         enddo
! inicializar x
         x(n) = dp(n)
! resolver para x a partir de vetores c-primo e d-primo
        do i = n-1, 1, -1
          x(i) = dp(i)-cp(i)*x(i+1)
        end do

    end subroutine solve_tridiag


       
    subroutine plotdisp(ux,neq,time,istep,kn)
    
        implicit none
        
        character*6  filer            ! local   variables 
        character*70 filename,filetime ! local   variables 
        
        real*8  :: xle , time      ! global  variables
        integer :: istep,kn,neq            ! global  variables 
        
        real*8  :: ux(neq)        ! local   variables
        integer :: ineq               ! local   variables 
        
        
        
        filer = 'wave1d'
        
        write (filename(1:5),'(i5.5)') int(istep/kn)
        
        write (filename,'(a)') filer//'.'//'disp'//filename(1:5)
        

        
        
        open  (unit=21,file=filename,form='formatted')
        
        write (21,'(a,i5,1x,a,f10.5)')'Ensight Vector step =', istep,',   Ensight Vector Time = ', time
        
        write (21,'(1p,6e12.4E3)') (ux(ineq), 0.0d0, 0.0d0, ineq=1,neq)
        
        close (unit=21)
        
        filetime = 'wave.time.txt'
        
        open (unit=22,file=filetime,form='formatted')
        
        write (22,'(f10.5)') time

    return
    end subroutine plotdisp
    
    
    
    
    
    subroutine plotmesh (neq,xLe)
    
        implicit none
        
        character*16  filer            ! local   variables 
        character*70 filename ! local   variables 
        
        real*8  :: xLe       ! global  variables
        integer :: neq            ! global  variables 
        
        real*8  :: xli        ! local   variables
        integer :: i               ! local   variables 
        
        
        
        filename = 'wave1d.geo'
        
        open  (unit=20,file=filename,form='formatted')
        
        write (20,'(a)')'Ensight Mesh'
        write (20,'(a)')'Description: Bar Geometry'
        write (20,'(a)')'node id given'
        write (20,'(a)')'element id given'
        write (20,'(a)')'coordinates'
        write (20,'(i8)') neq
        
        xli = 0.d0
        do i = 1, neq
            
            xli = xli + xle
            write(20,'(i8,3e12.4)') i,xli , 0.0d0, 0.0d0
        
        enddo

        
        
        
        
        write (20,'(a)')'part 1'
        write (20,'(a)')'Bar1d'
        write (20,'(a)')'bar2'
        
        write (20,'(i8)') neq-1
        
        
        do i = 1, neq-1
            
            write(20,'(3i8)') i , i, i+1
        
        enddo

        close (unit=20)
        
      

    return
    end subroutine plotmesh

