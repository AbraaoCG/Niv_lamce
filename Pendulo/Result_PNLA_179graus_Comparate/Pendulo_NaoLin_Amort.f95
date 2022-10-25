program test
    implicit none

    integer:: i,nsteps
    real*8 :: tempo_Total,tempo_Atual,theta,omegaOld,omega,theta_0,omega_0, PeriodoOld
    real*8 :: grav,dt,xL,wl_2, wl, massa, Tracao
    real*8 :: Beta,Alfa,C,qsi
    open (unit= 21, file= 'data.txt' , form= 'formatted')                  
    open (unit = 22, file= 'data2.txt', form= 'formatted')    

    ! Parametros do Pendulo
    massa = 0.2
    tempo_Atual = 0.0
    tempo_Total = 400.0
    xL = 0.8 
    grav = 9.81
    dt = 0.01
    theta_0 = 179.0 !em Graus
    omega_0 = 0.0 
    nsteps = tempo_Total / dt
    theta_0 = theta_0 * 3.1416 / 180 
    wl_2 = grav /xL
    wl = sqrt(wl_2)

    qsi = 0.005
    Alfa = ( 2 * qsi ) * wl
    Beta = (2 * qsi) / wl
    C = Alfa
    ! Execucao do algoritmo.
    theta = theta_0
    omega = omega_0
    Tracao = massa * (grav * cos(theta) + omega * omega * xL)    
    PeriodoOld = 0 
    write(21,*)tempo_Atual, theta, omega, Tracao
    do i = 1,nsteps
        tempo_Atual = tempo_Atual + dt
	omegaOld = omega
        omega = omega - ( wl_2 * sin(theta) + C * omega )* dt

	! Muda de direcao
	if (omegaOld > 0 .and. omega < 0 ) then
		write(22,*) tempo_Atual, tempo_Atual - PeriodoOld	
		PeriodoOld = tempo_Atual
	endif

        theta = theta + omega * dt
        Tracao = massa * (grav * cos(theta) + omega * omega * xL)
        write(21,*)tempo_Atual, theta, omega, Tracao
	
    enddo

end program test


