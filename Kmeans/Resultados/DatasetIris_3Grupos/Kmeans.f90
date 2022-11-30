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
    ! Flag Mudanca será usada para indicar se o algorítimo estabilizou em uma ordem de grupos.
    ! itmp é uma variável para armazenar localmente um inteiro.
    ! numPontosGrupo é um vetor para armazenar o número de pontos contido em cada grupo.( para calculo de baricentro.)
    integer ::  numGrupos,numVariaveis, numPontos,flag_mudanca, itmp
    integer :: i,j,k ! Iteradores
    integer, allocatable :: agrupamento(:),numPontosGrupo(:)
    real*8 :: somaQuadrados,tmp,menorDist, distanciaPC
    real*8  , allocatable   :: xMinimos(:), xMaximos(:) , xMaximos_base(:)
    real*8  ,allocatable  ::  dados(:,:), centros(:,:),sumGroupXn(:,:), dados_norm(:,:)     ! dados(numPontos,numClusters)    
    
    numGrupos=3
    numVariaveis=2
    numPontos=150
    
    allocate  ( agrupamento(1:numPontos))  ; agrupamento(:)  = 0.d0
    allocate  ( numPontosGrupo(1:numGrupos)) ; numPontosGrupo(:) = 0.d0 
    allocate  ( dados(1:numPontos,1:numVariaveis))  ; dados(:,:)  = 0.d0
    allocate  ( dados_norm(1:numPontos,1:numVariaveis))  ; dados_norm(:,:)  = 0.d0
    allocate  ( centros(1:numGrupos,1:numVariaveis) )
    allocate  ( xMinimos(numVariaveis) ) ; xMinimos(:) = 100000.d0 ! escolhe-se 2 e -1, pois o valores de todas características é normalizado entre 0 e 1.
    allocate  ( xMaximos(numVariaveis) ) ; xMaximos(:) = -10000.d0
    allocate  ( xMaximos_base(numVariaveis) ) ;
    allocate  ( sumGroupXn(1:numGrupos,1:numVariaveis))  ; sumGroupXn(:,:)  = 0.d0
    ! Body of Kmeans
    
    open  (unit=11,file='dataset01.txt',form='formatted')
    
    open  (unit=15,file='test.txt',form='formatted')
    open  (unit=17,file='centrosFinal.txt',form='formatted')
    
    do i = 1,numPontos
    ! Ler dados, e encontrar mínimos e máximos em cada eixo/Variável.
    read (11,*) (dados(i,j) , j = 1,numVariaveis)
        do j = 1, numVariaveis
            if ( xMinimos(j) > dados(i,j) ) then
                xMinimos(j) = dados(i,j)
            endif
            if (xMaximos(j) < dados(i,j) ) then
                xMaximos(j) = dados(i,j)
            endif
        enddo
    enddo
    xMaximos_base = xMaximos
    
    ! Após encontrar máximos e mínimos, normalizar todos os pontos
    do i = 1,numPontos 
        do j = 1, numVariaveis
            dados_norm(i,j) = dados(i,j) / xMaximos(j)
        enddo   
    enddo
    
    ! Encontrar máximos e minimos normalizados
    xMinimos(:) = 100000.d0 ; xMaximos(:) = 1.d0

    do i = 1,numPontos
        do j = 1, numVariaveis
            if ( xMinimos(j) > dados_norm(i,j) ) then
                xMinimos(j) = dados_norm(i,j)
            endif
        enddo
    enddo

    
    
    ! Definir Inicialização de centros em função do número de grupos ( até 5)
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

    flag_mudanca = -1
    do while(flag_mudanca .ne. 0)
        flag_mudanca = 0
        !Reinicializo Vetores auxiliares de cálculo de baricentro.
        sumGroupXn(:,:)  = 0.d0 
        numPontosGrupo(:)  = 0.d0
        !Registro dos centroides de cada iteração.
        write(15,*) "Centroides de cada grupo:"
        do i = 1, numGrupos
            write(15,*) ( centros(i,j) , j = 1, numVariaveis)
        enddo
        write(15,*) "Segue distancia entre numPontos, centroide grupo j, distancia"
        ! Iteração para obter distancias entre cada ponto e cada centro de grupo, visando classificar os pontos nos grupos.
        do i = 1,numPontos
            itmp = agrupamento(i) ! Armazeno agrupamento do ponto i antes da atualização.
            menorDist = 2
            do j = 1,numGrupos
                somaQuadrados = 0.0
                do k = 1, numVariaveis ! Loop auxiliar para cálculo de distância euclidiana no R(numGrupos).
                    tmp = 0
                    tmp = dados_norm(i,k) - centros(j,k) ! tmp recebe a diferenca entre o centro e o ponto.
                    somaQuadrados = somaQuadrados + (tmp ** 2)  
                enddo
                distanciaPC = sqrt(somaQuadrados)
                write(15,*) i , j , distanciaPC
                ! Vejo se preciso atualizar Grupo do ponto analisado
                if ( distanciaPC < menorDist ) then
                    menorDist = distanciaPC
                    agrupamento(i) = j
                endif
            enddo
            numPontosGrupo( agrupamento(i) ) = numPontosGrupo( agrupamento(i) ) + 1 ! Mantenho rastreio do número de pontos de cada grupo ao final do algorítimo

            if ( itmp .ne. agrupamento(i)) then ! Verifico se houve alteração no agrupamento desse ponto; se sim a flag mudança é 1.
                flag_mudanca = 1                !  Se a flag se torna 1 devido a qualquer ponto, o algorítimo continua.
            endif

            do j = 1, numVariaveis ! Loop para auxiliar posterior cálculo de baricentro, somando valores de Xn em um vetor.
                itmp = agrupamento(i) ! Armazeno grupo atual do ponto i
                sumGroupXn(itmp,j) = sumGroupXn(itmp,j) + dados_norm(i,j) ! Computo o somatório dos Xk de todos os pontos de cada grupo.
            enddo
        enddo

        ! Atualização de centros, caso haja algum ponto no grupo.
        do i = 1,numGrupos
	    if (numPontosGrupo(i) .ne. 0) then
            do j = 1,numVariaveis
                tmp = ( sumGroupXn(i,j) / numPontosGrupo(i) ) ! tmp armazena a nova coordenada para a variável j do grupo i.
                centros(i,j) = tmp
            enddo
	    endif
	    ! Se o algoritimo for parar, posso registrar todos os centros finais.
            if ( flag_mudanca == 0 ) then
                ! desnormalizo coordenadas dos centroides.
                    do j = 1 , numVariaveis
                        centros(i,j) = centros(i,j) * xMaximos_base(j)
                    enddo 
		            write (17,*) centros(i,:)
	        endif
        enddo
        
    enddo
    ! Abrir arquivos de saida de cada grupo.
    open  (unit=13,file='agrupamento.txt',form='formatted')

    open  (unit=21,file='grupo1.txt',form='formatted')
    open  (unit=22,file='grupo2.txt',form='formatted')
    open  (unit=23,file='grupo3.txt',form='formatted')
    open  (unit=24,file='grupo4.txt',form='formatted')
    open  (unit=25,file='grupo5.txt',form='formatted')
    ! Registrar grupamentos finais gerar arquivos para plot de cada grupo.
    if (flag_mudanca == 0) then
	write(13,'(A11)') "Ponto;Grupo"
        do i = 1,numPontos
            write(13, fmt='(i4.4,A1,i4.4)') i - 1 ,";" ,agrupamento(i)
            itmp = agrupamento(i)
            if (itmp == 1) then
                write(21,*) ( dados(i,:) )
            else if(itmp == 2) then
                write(22,*) ( dados(i,:) )
            else if(itmp == 3) then
                write(23,*) ( dados(i,:) )
            else if(itmp == 4) then
                write(24,*) ( dados(i,:) )
            else if(itmp == 5) then
                write(25,*) ( dados(i,:) )
            endif
        enddo
    endif

    ! Escrever em arquivo de teste os mínimos e máximos
    write(15,*) " xMinimos --- xMaximos : "
    do i = 1, numVariaveis
        write(15,*) xMinimos(i) , " --- ", xMaximos(i)
    enddo
 

   pause 
    end program Kmeans