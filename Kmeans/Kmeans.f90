!  Kmeans.f90 
!
!  FUNCTIONS:
!  Kmeans - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: Kmeans
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

    program Kmeans

    implicit none

    ! Variables
    integer ::  numGrupos,numVariaveis, numPontos,flag_mudanca ! Flag Mudanca ser� usada para indicar se o algor�timo estabilizou em uma ordem de grupos.
    integer :: i,j,k ! Iteradores
    real*8 :: somaQuadrados,tmp,menorDist, distanciaPC
    real*8  , allocatable   ::  agrupamento(:), xMinimos(:), xMaximos(:)
    real*8  ,allocatable  ::  dados(:,:), centros(:,:),sumGroupXn(:,:)     ! dados(numPontos,numClusters)    
    
    numGrupos=3
    numVariaveis=2
    numPontos=4
    
    allocate  ( agrupamento(1:numPontos))  ; agrupamento(:)  = 0.d0
    allocate  ( dados(1:numPontos,1:numVariaveis))  ; dados(:,:)  = 0.d0
    allocate  ( centros(1:numGrupos,1:numVariaveis) )
    allocate  ( xMinimos(numVariaveis) ) ; xMinimos(:) = 2.d0
    allocate  ( xMaximos(numVariaveis) ) ; xMaximos(:) = -1.d0
    allocate  ( sumGroupXn(1:numGrupos,1:numVariaveis))  ; sumGroupXn(:,:)  = 0.d0
    ! Body of Kmeans
    

    open  (unit=11,file='dataset01.txt',form='formatted')
    open  (unit=13,file='agrupamento.txt',form='formatted')
    open  (unit=15,file='test.txt',form='formatted')
    do i = 1,numPontos
    read (11,*) (dados(i,j), j = 1,numVariaveis)
        do j = 1, numVariaveis
            if ( xMinimos(j) > dados(i,j) ) then
                xMinimos(j) = dados(i,j)
            endif
            if (xMaximos(j) < dados(i,j) ) then
                xMaximos(j) = dados(i,j)
            endif
        enddo
    enddo
    
    
    if (numGrupos == 1) then
        do j = 1,numVariaveis
        centros(1,j) = ( (( xMaximos(j) - xMinimos(j) ) / 2) + xMinimos(j) ) ! Ponto central entre maximos e minimos
        enddo
    endif
    if (numGrupos == 2) then
        do j = 1,numVariaveis
        centros(1,j) = xMinimos(j) 
        centros(2,j) = xMaximos(j)
        enddo
    endif
    if (numGrupos == 3) then
        do j = 1,numVariaveis
        centros(1,j) = xMinimos(j) 
        centros(2,j) = xMaximos(j)
        centros(3,j) = ( (( xMaximos(j) - xMinimos(j) ) / 2) + xMinimos(j) ) ! Ponto central entre maximos e minimos
        enddo
    endif
    if (numGrupos == 4) then
        do j = 1,numVariaveis
            centros(1,j) = xMinimos(j) 
            centros(3,j) = xMaximos(j)   
            if (MOD(j,2) == 0) then
                centros(2,j) = xMinimos(j)
                centros(4,j) = xMaximos(j)  
            endif
            if (MOD(j,2) == 1) then
                centros(2,j) = xMaximos(j)
                centros(4,j) = xMinimos(j)
            endif
        enddo
    endif
    if (numGrupos == 5) then
        do j = 1,numVariaveis
            centros(1,j) = xMinimos(j) 
            centros(3,j) = xMaximos(j) 
            centros(5,j) = ( (( xMaximos(j) - xMinimos(j) ) / 2) + xMinimos(j) ) ! Ponto central entre maximos e minimos
            if ( MOD(j,2) == 0) then
                centros(2,j) = xMinimos(j)
                centros(4,j) = xMaximos(j)  
            endif
            if (MOD(j,2) == 1) then
                centros(2,j) = xMaximos(j)
                centros(4,j) = xMinimos(j)
            endif
        enddo
    endif
    
    !centros(1,1) = 0 ; centros(1,2) = 0 
    !centros(2,1) = 0.5 ; centros(2,2) = 0.5
    !centros(3,1) = 1 ; centros(3,2) = 1
    
    
    write(15,*) "Segue distancia entre numPonto, centroide grupo j, distancia"
    flag_mudanca = 0
    do i = 1,numPontos
        menorDist = 2
        do j = 1,numGrupos
            ! Calcular distancia entre ponto e centro do grupo
            somaQuadrados = 0.0
            do k = 1, numVariaveis
                tmp = 0
                tmp = dados(i,k) - centros(j,k) ! tmp recebe a diferenca entre o centro e o ponto.
                somaQuadrados = somaQuadrados + (tmp ** 2)  
            enddo
            distanciaPC = sqrt(somaQuadrados)
            write(15,*) i , j , distanciaPC
            ! Vejo se preciso atualizar Grupo do ponto analisado
            if ( distanciaPC < menorDist ) then
                flag_mudanca = 1
                menorDist = distanciaPC
                agrupamento(i) = j
            endif
        enddo
    enddo


    

    do i = 1, numPontos
            write(13,*) ( agrupamento(i) ) 
    enddo
    write(15,*) " xMinimos: "
    do i = 1, numGrupos
            write(15,*) xMinimos(i) 
    enddo
    write(15,*) " xMaximos: "
    do i = 1, numGrupos
            write(15,*) xMaximos(i)
    enddo
    write(15,*) " Centros:"
    do i = 1, numGrupos
        write(15,*) ( centros(i,j) , j = 1,numVariaveis )
    enddo

    
    end program Kmeans

