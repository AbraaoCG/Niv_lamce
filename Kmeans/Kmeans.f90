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
    integer ::  numGrupos,numVariaveis, numPontos,flag_mudanca, itmp , contadorErros , seAgrupado
    integer :: i,j,k ! Iteradores
    integer, allocatable :: agrupamento(:), agrupamento_org_treino(:) ,numPontosGrupo(:)
    integer, allocatable :: indexTreino(:) , correspondencia_centros(:) , erros(:)
    real*8 :: somaQuadrados,tmp,menorDist, distanciaPC
    real*8  , allocatable   :: xMinimos(:), xMaximos(:) , xMaximos_base(:) 
    real*8  ,allocatable  ::  dados(:,:), centros(:,:),sumGroupXn(:,:), dados_norm(:,:), centros_original(:,:)      
    character*20, allocatable  :: header(:), groupNames(:)
    character*20 :: ctmp
    
    open(unit=19, file = 'args.txt')
    read (19, '(A10 , i4.4)') ctmp , numGrupos
    read (19, '(A7 , i4.4)') ctmp , numVariaveis
    read (19, '(A16 , i4.4)') ctmp , numPontos
    read (19, '(A18 , i4.4)') ctmp , seAgrupado

    allocate  ( erros(1:numPontos))  ; erros(:)  = -1.d0 ! Lista de erros inicializa com -1, pontos com valor diferente de 1 nesse Array foram classificados errado pelo algorítimo.
    allocate  ( agrupamento(1:numPontos))  ; agrupamento(:)  = 0.d0 
    allocate  ( agrupamento_org_treino(1:numPontos))  ; agrupamento_org_treino(:)  = -1.d0 ! Antes de ler agrupamento = 1, invalido.
    allocate  ( correspondencia_centros(1:numGrupos))  ; correspondencia_centros(:)  = -1.d0 ! Antes de ler correspondencia = 1, invalido.
    allocate  ( numPontosGrupo(1:numGrupos)) ; numPontosGrupo(:) = 0.d0 
    ! Alocação de espaço para guardar indices de treino.
    allocate  ( indexTreino(1:numPontos))  ; agrupamento(:)  = -1.d0 
    ! Alocação de espaço para armazenar dados e dados normalizados ( de treino ).
    allocate  ( dados(1:numPontos,1:numVariaveis))  ; dados(:,:)  = 0.d0
    allocate  ( dados_norm(1:numPontos,1:numVariaveis))  ; dados_norm(:,:)  = 0.d0
    ! Alocação de espaço para armazenar centros de treino e original (Dataset original).
    allocate  ( centros(1:numGrupos,1:numVariaveis) )
    allocate  ( centros_original(1:numGrupos,1:numVariaveis) )
    ! Alocação de espaço para armazenar mínimos e máximos para cada eixo / variável.
    allocate  ( xMinimos(numVariaveis) ) ; xMinimos(:) = 1.0e15 ! escolhe-se e15 e -e15, para dar margem para o algorítimo funcionar na seleção de máximos e mínimos em cada eixo / variável.
    allocate  ( xMaximos(numVariaveis) ) ; xMaximos(:) = -1.0e15
    allocate  ( xMaximos_base(numVariaveis) ) ;xMaximos_base(:) = 0.d0
    ! Alocação de espaço para armazenar soma de valores referentes à cada variável e para cada grupo. (serve ao cálculo de centróides).
    allocate  ( sumGroupXn(1:numGrupos,1:numVariaveis))  ; sumGroupXn(:,:)  = 0.d0
    ! Alocação de espaço para armazenar header das variáveis e também dos nomes dos grupos.
    allocate  ( header(1:numVariaveis))  ; header(:)  = ""
    allocate  ( groupNames(1:numGrupos))  ; groupNames(:)  = ""
    
    ! Body of Kmeans
    
    open  (unit=11,file='dataSets/dataset_treino.txt',form='formatted')
    open  (unit=12,file='dataSets/dataset_teste.txt',form='formatted')
    open  (unit=15,file='Resultado_Plot/test.txt',form='formatted')
    open  (unit=17,file='Resultado_Plot/centrosTreino.txt',form='formatted')

    
    
   ! Ler dados, e encontrar mínimos e máximos em cada eixo/Variável.
    
    read (11, *) ctmp,header(:)

    do i = 1,numPontos
    read (11,*) indexTreino(i) , dados(i,:) 
    
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
    xMinimos(:) = 1.0e15 ; xMaximos(:) = 1.d0

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
            menorDist = 1.0e15
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
    ! Leitura do centroide Original para cálculo de correspondencia e erro, caso o dataSet seja pré agrupado.
    if (seAgrupado .eq. 1) then
        open(19, file = 'Resultado_Plot/Centroide_Original.txt', form = 'formatted')
        do i = 1,numGrupos
            read(19, *) groupNames(i), centros_original(i,:)
        enddo
        ! Calculo de correspondencia entre centroide Treino e centroide original. 
        do i = 1,numGrupos ! i --> index centroide de treino ( centros(:,:) )
            menorDist = 1.0e15
            do j = 1,numGrupos ! j --> index centroide original ( centros_original(:,:) )
                ! Verifico com qual centro original 'j' o centro de treino 'i' é mais compatível
                somaQuadrados = 0.0
                do k = 1, numVariaveis ! Loop auxiliar para cálculo de distância euclidiana no R(numGrupos).
                    tmp = 0
                    tmp = centros_original(i,k) - centros(j,k) ! tmp recebe a diferenca entre o centro e o ponto.
                    somaQuadrados = somaQuadrados + (tmp ** 2)  
                enddo
                distanciaPC = sqrt(somaQuadrados)
                write(15,*) i , j , distanciaPC
                ! Vejo se preciso atualizar Grupo do ponto analisado
                if ( distanciaPC < menorDist ) then
                    menorDist = distanciaPC
                    correspondencia_centros(i) = j - 1 ! Ouput do python para agrupamento é de 0 --> m ( m = número de grupos)
                endif
            enddo
            write(15,*) correspondencia_centros(i) 
        enddo
        ! Cálculo de erro entre treino e classificação especialista.
        open(20, file = 'agrupamentoOriginal/agrup_original_treino.txt' , form = 'formatted')
        open(31 , file = 'Resultado_Plot/erros.txt', form= 'formatted')
        read(20,*) ctmp ! Le nome do tipo de grupo (nome coluna).
        read(20,'(i4.3)') ( agrupamento_org_treino(i) , i = 1, numPontos )

        ! indexTreino
        write(31,'(100A)') ( header(i) , i = 1,numVariaveis ) ! OBS : Aqui permite até 100 variáveis.
        contadorErros = 0
        do i = 1,numPontos ! i --> index do ponto 'i' de treino, a ser avaliado.
            if ( correspondencia_centros(agrupamento(i)) .ne. agrupamento_org_treino(i) ) then 
                erros(i) = indexTreino(i)
                contadorErros = contadorErros + 1   
                write(31,*) dados(i,:)
            endif
        enddo!agrupamento_org_treino
        tmp = ( real( contadorErros) ) / (real(numPontos)) 
        print *, tmp
        print ('(A22 , i4.4 , A3 , i4.4 , A3 , F8.4)'), "O erro de treino é : " , contadorErros , " / " , numPontos , " = " , tmp 
    else
        print *, " Não é possível calcular erro de treino, pois o dataSet não é previamente agrupado."
    endif
    
    
    ! Abrir arquivos de saida de cada grupo.
    open  (unit=13,file='Resultado_Plot/agrupamento.txt',form='formatted')
    open  (unit=21,file='Resultado_Plot/grupo1.txt',form='formatted')
    open  (unit=22,file='Resultado_Plot/grupo2.txt',form='formatted')
    open  (unit=23,file='Resultado_Plot/grupo3.txt',form='formatted')
    open  (unit=24,file='Resultado_Plot/grupo4.txt',form='formatted')
    open  (unit=25,file='Resultado_Plot/grupo5.txt',form='formatted')
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

    print *, achar(10) , " Classificação executada com sucesso, verifique output em Resultado_Plot"
 

    
    end program Kmeans