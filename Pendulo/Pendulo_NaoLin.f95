program test
    implicit none

    integer:: i,nsteps
    real*8 :: tempo_Total,tempo_Atual,theta,omega,theta_0,omega_0
    real*8 :: grav,dt,xL,wl_2, massa, Tracao
    open (unit= 1, file= 'Result_PendNL_X.txt' , form= 'formatted')                  
    
    ! Parametros do Pendulo
    massa = 0.2
    tempo_Atual = 0.0
    tempo_Total = 35.0
    xL = 0.8 
    grav = 9.81
    dt = 0.01
    theta_0 = 179.0 !em Graus
    omega_0 = 0.0 
    nsteps = tempo_Total / dt
    theta_0 = theta_0 * 3.1416 / 180 
    wl_2 = grav /xL
    ! Execucao do algoritmo.
    theta = theta_0
    omega = omega_0
    Tracao = massa * (grav * cos(theta) + omega * omega * xL)
    write(1,*)tempo_Atual, theta, omega, Tracao
    do i = 1,nsteps
        tempo_Atual = tempo_Atual + dt
        omega = omega - wl_2 * sin(theta) * dt
        theta = theta + omega * dt
        Tracao = massa * (grav * cos(theta) + omega * omega * xL)
        write(1,*)tempo_Atual, theta, omega, Tracao
    enddo

end program test
