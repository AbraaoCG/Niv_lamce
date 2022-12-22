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
    ! itmp é uma variável para armazenar temporariamente um inteiro. rtmp e ctmp são análogos para reais e caracteres.
    ! numPontosGrupo é um vetor para armazenar o número de pontos contido em cada grupo.( para calculo de baricentro.)
    integer ::  numGrupos,numVariaveis,numTotalVariaveis,numPontos,porcentTreino, numPontosTreino
    integer :: flag_mudanca, itmp, itmp2, contadorErros, seAgrupado
    integer :: i,j,k ! Iteradores
    integer, allocatable :: agrupamento(:), agrupamento_org_treino(:) ,numPontosGrupo(:), var_selecionadas(:), groupCodes(:)
    integer, allocatable :: indexTreino(:) , correspondencia_centros(:) , erros(:), ArrayIndicesUsados(:), ArrayColunasUsadas(:)
    real*8 :: somaQuadrados,tmp,menorDist, distanciaPC, rtmp
    real*8  , allocatable   :: xMinimos(:), xMaximos(:) , xMaximos_base(:), rtmpArray(:)
    real*8  ,allocatable  :: dadosCompleto(:,:), dadosTreino(:,:), dados_norm(:,:)  
    real*8  ,allocatable  :: centros(:,:), sumGroupXn(:,:), centros_original(:,:)    
    character*20, allocatable  :: header(:), groupNames(:), colAgrupOrCompleto(:),colAgrupOrTreino(:),colAgrupOrTeste(:)
    character*30 :: ctmp, legendaColAgrupOr
    
    numGrupos = 3
    numTotalVariaveis = 4 
    numVariaveis = 3
    allocate  ( var_selecionadas(1:numVariaveis) )
    var_selecionadas =  [2,3,4] ! 1 à número total de variáveis.
    porcentTreino = 90 ! Porcentagem de dados dedicado à treino.
    numPontos = 150 ! Número total de dados aquisitados.
    seAgrupado = 1
    

    rtmp = (numPontos * porcentTreino)/100
    numPontosTreino = FLOOR(rtmp)
    print *, "Números total de aquisições: ", numPontos
    print *, "Números de aquisições dedicadas à treino: ", numPontosTreino

    
    allocate ( erros(1:numPontosTreino))  ; erros(:)  = -1.d0 ! Lista de erros inicializa com -1, pontos com valor diferente de 1 nesse Array foram classificados errado pelo algorítimo.
    allocate ( agrupamento(1:numPontosTreino))  ; agrupamento(:)  = 0.d0 
    allocate ( agrupamento_org_treino(1:numPontosTreino))  ; agrupamento_org_treino(:)  = -1.d0 ! Antes de ler agrupamento = 1, invalido.
    allocate ( correspondencia_centros(1:numGrupos))  ; correspondencia_centros(:)  = -1.d0 ! Antes de ler correspondencia = 1, invalido.
    allocate ( numPontosGrupo(1:numGrupos)) ; numPontosGrupo(:) = 0.d0 
    ! Alocação de espaço para guardar indices de treino e Array com indexes usados para treino. (Colunas usadas também)
    allocate ( indexTreino(1:numPontosTreino))  ; agrupamento(:)  = -1.d0 
    allocate ( ArrayIndicesUsados(1:numPontos)) ; ArrayIndicesUsados(:) = 0
    allocate ( ArrayColunasUsadas(1:numTotalVariaveis)) ; ArrayColunasUsadas = 0
    !Alocação de espaço para coluna temporária para armazenar informações de uma aquisição.
    allocate ( rtmpArray(1:numVariaveis)) ; rtmpArray(:) = -1
    ! Alocação de espaço para armazenar dados.
    allocate ( dadosCompleto(1:numPontos,1:numTotalVariaveis)) ; dadosCompleto(:,:)  = 0.d0
    allocate ( dadosTreino(1:numPontosTreino,1:numVariaveis))  ; dadosTreino(:,:)  = 0.d0
    allocate ( dados_norm(1:numPontosTreino,1:numVariaveis))  ; dados_norm(:,:)  = 0.d0
    ! Alocação de espaço para armazenar agrupamento original
    allocate (colAgrupOrTreino(1:numPontosTreino))
    allocate (colAgrupOrCompleto(1:numPontos))
    ! Alocação de espaço para armazenar centros de treino e original (Dataset original).
    allocate ( centros(1:numGrupos,1:numVariaveis) )
    allocate ( centros_original(1:numGrupos,1:numVariaveis) )
    ! Alocação de espaço para armazenar mínimos e máximos para cada eixo / variável.
    allocate ( xMinimos(numVariaveis) ) ; xMinimos(:) = 1.0e15 ! escolhe-se e15 e -e15, para dar margem para o algorítimo funcionar na seleção de máximos e mínimos em cada eixo / variável.
    allocate ( xMaximos(numVariaveis) ) ; xMaximos(:) = -1.0e15
    allocate ( xMaximos_base(numVariaveis) ) ;xMaximos_base(:) = 0.d0
    ! Alocação de espaço para armazenar soma de valores referentes à cada variável e para cada grupo. (serve ao cálculo de centróides).
    allocate ( sumGroupXn(1:numGrupos,1:numVariaveis))  ; sumGroupXn(:,:)  = 0.d0
    ! Alocação de espaço para armazenar header das variáveis e também dos nomes e códigos dos grupos.
    allocate ( header(1:numVariaveis))  ; header(:)  = ""
    allocate ( groupNames(1:numGrupos))  ; groupNames(:)  = ""
    allocate ( groupCodes(1:numGrupos)) ; groupCodes(:) = -1
    
    ! Body of Kmeans
    
    open (unit=43, file ='dataSets/dataset_completoKM.txt',form='formatted');
    
    open  (unit=11,file='dataSets/dataset_treinoKM.txt',form='formatted');
    open  (unit=12,file='dataSets/dataset_testeKM.txt',form='formatted');
    open  (unit=15,file='Resultado_PlotKmeans/test.txt',form='formatted')
    open  (unit=17,file='Resultado_PlotKmeans/centrosTreino.txt',form='formatted');
    
    !Subrotina para obter informação dos centroides originais, para calcular o erro posteriormente.
    
    !Subrotina para selecionar índices de treino.
    CALL obterIndicesTreino(numPontos,numPontosTreino,ArrayIndicesUsados(:),indexTreino(:))

    !  Ler todos os dados.
    read (43, *) header(:),ctmp
    do i = 1,numPontos
        if (seAgrupado .eq. 0 ) then
            read (43,*) dadosCompleto(i,:)
        else
            read (43,*) dadosCompleto(i,:), colAgrupOrCompleto(i)
        endif
        !print *,dadosCompleto(i,:)
    enddo
    
    ! Definir array com indicação de uso para colunas.
    do i = 1,numVariaveis
        ArrayColunasUsadas(var_selecionadas(i)) = 1
    enddo
    ! Ler dados, e encontrar mínimos e máximos em cada eixo/Variável + Recorte de dados de treino e teste. + Recorte de colunas desejadas.
    itmp = 0
    ctmp = '-'
    do i = 1,numPontos
        itmp2 = 0
        do j = 1,numTotalVariaveis
            if (ArrayColunasUsadas(j) .eq. 1) then ! Se a coluna é usada como variável de classificação
                itmp2 = itmp2 + 1
                rtmpArray(itmp2) = dadosCompleto(i,j)  ! Array temporário recebe uma linha apenas com colunas selecionadas.
            endif
        enddo
        if (ArrayIndicesUsados(i) .eq. 1) then ! Se for um indice a ser usado para treino.
            itmp = itmp + 1
            dadosTreino(itmp,:) = rtmpArray(:)
            colAgrupOrTreino(itmp) = colAgrupOrCompleto(i)
            do j = 1, numVariaveis
                if ( xMinimos(j) > dadosTreino(itmp,j) ) then
                    xMinimos(j) = dadosTreino(itmp,j)
                endif
                if (xMaximos(j) < dadosTreino(itmp,j) ) then
                    xMaximos(j) = dadosTreino(itmp,j)
                endif
            enddo
            
        endif
    enddo
    xMaximos_base = xMaximos

    
    ! Após encontrar máximos e mínimos, normalizar todos os pontos
    do i = 1,numPontosTreino 
        do j = 1, numVariaveis
            dados_norm(i,j) = dadosTreino(i,j) / xMaximos(j)
        enddo   
    enddo
    
    ! Encontrar máximos e minimos normalizados
    xMinimos(:) = 1.0e15 ; xMaximos(:) = 1.d0

    do i = 1,numPontosTreino
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
        do i = 1,numPontosTreino
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
    ! Cálculo do centroide Original para determinação de correspondencia e erro, caso o dataSet seja pré agrupado.
    if (seAgrupado .eq. 1) then
        CALL codificarGrupos(numGrupos,numPontos,colAgrupOrCompleto(:),groupNames(:),groupCode(:))
        CALL calculoCentroideOr(numGrupos, numVariaveis, numPontos,numPontosTreino, dadosCompleto(:),dadosTreino(:),&
         colAgrupOrTreino(:),agrupamento_org_treino(:))
        open(19, file = 'Resultado_PlotKmeans/Centroide_Original.txt', form = 'formatted')
        do i = 1,numGrupos
            read(19, *) centros_original(i,:)
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
        open(31 , file = 'Resultado_PlotKmeans/erros.txt', form= 'formatted')
        read(20,*) ctmp ! Le nome do tipo de grupo (nome coluna).
        read(20,'(i4.3)') ( agrupamento_org_treino(i) , i = 1, numPontosTreino )

        ! indexTreino
        write(31,'(100A)') ( header(i) , i = 1,numVariaveis ) ! OBS : Aqui permite até 100 variáveis.
        contadorErros = 0
        do i = 1,numPontosTreino ! i --> index do ponto 'i' de treino, a ser avaliado.
            if ( correspondencia_centros(agrupamento(i)) .ne. agrupamento_org_treino(i) ) then 
                erros(i) = indexTreino(i)
                contadorErros = contadorErros + 1   
                write(31,*) dadosTreino(i,:)
            endif
        enddo!agrupamento_org_treino
        tmp = ( real( contadorErros) ) / (real(numPontosTreino)) 
        print *, tmp
        print ('(A22 , i4.4 , A3 , i4.4 , A3 , F8.4)'), "O erro de treino é : " &
        , contadorErros , " / " , numPontosTreino , " = " , tmp 
    else
        print *, " Não é possível calcular erro de treino, pois o dataSet não é previamente agrupado."
    endif
    
    
    ! Abrir arquivos de saida de cada grupo.
    open  (unit=13,file='Resultado_PlotKmeans/agrupamento.txt',form='formatted')
    open  (unit=21,file='Resultado_PlotKmeans/grupo1.txt',form='formatted')
    open  (unit=22,file='Resultado_PlotKmeans/grupo2.txt',form='formatted')
    open  (unit=23,file='Resultado_PlotKmeans/grupo3.txt',form='formatted')
    open  (unit=24,file='Resultado_PlotKmeans/grupo4.txt',form='formatted')
    open  (unit=25,file='Resultado_PlotKmeans/grupo5.txt',form='formatted')
    ! Registrar grupamentos finais gerar arquivos para plot de cada grupo.
    if (flag_mudanca == 0) then
	write(13,'(A11)') "Ponto;Grupo"
        do i = 1,numPontosTreino
            write(13, fmt='(i4.4,A1,i4.4)') i - 1 ,";" ,agrupamento(i)
            itmp = agrupamento(i)
            if (itmp == 1) then
                write(21,*) ( dadosTreino(i,:) )
            else if(itmp == 2) then
                write(22,*) ( dadosTreino(i,:) )
            else if(itmp == 3) then
                write(23,*) ( dadosTreino(i,:) )
            else if(itmp == 4) then
                write(24,*) ( dadosTreino(i,:) )
            else if(itmp == 5) then
                write(25,*) ( dadosTreino(i,:) )
            endif
        enddo
    endif

    ! Escrever em arquivo de teste os mínimos e máximos
    write(15,*) " xMinimos --- xMaximos : "
    do i = 1, numVariaveis
        write(15,*) xMinimos(i) , " --- ", xMaximos(i)
    enddo

    print *, achar(10) , " Classificação executada com sucesso, verifique output em Resultado_PlotKmeans"
 
    end program Kmeans
   
    
    SUBROUTINE obterIndicesTreino(numPontos,numPontosTreino,ArrayIndicesUsados,indexTreino)
    ! Declaração de variáveis.
    integer :: i2,indexAleatorio,flagIndexEncontrado 
    real,dimension(8) :: random_seed
    integer,dimension(8) :: values
    real :: rtmp1
    integer, INTENT(IN) :: numPontos,numPontosTreino;
    integer,INTENT(OUT) :: ArrayIndicesUsados(numPontos) 
    integer, INTENT(OUT) :: indexTreino(numPontosTreino); indexTreino(:) = -1
    
    !Itero sobre todos pontos de treino. (índices a serem definidos)
    do i2 = 1,numPontosTreino
        
        !Obtenção inicial de semente para geração de num aleatório.
        call date_and_time(VALUES=values) ! Gera 'semente' inteira para número aleatório oriunda do horário do sistema
        rtmp1 =  values(8) * 1.0 ! Compilador transforma essa semente em número real .
        call random_number(HARVEST = rtmp1) ! Gera número real aleatório normalizado.
        indexAleatorio = FLOOR(rtmp1  * numPontos) + 1 ! Originado número aleatório entre 1 e o número de pontos, utilizando o número normalizado aleatório.

        flagIndexEncontrado=0 
        do while(flagIndexEncontrado .eq. 0) !Enquanto não tiver encontrado indice novo obtenho um índice aleatório.
            if(ArrayIndicesUsados(indexAleatorio) .eq. 0) then
                indexTreino(i2) = indexAleatorio
                ArrayIndicesUsados(indexAleatorio) = 1
                flagIndexEncontrado = 1
            else 
                ! Se o número gerado já foi usado, gero outro até conseguir um diferente
                call date_and_time(VALUES=values) ! Gera 'semente' inteira para número aleatório oriunda do horário do sistema
                rtmp1 =  values(8) * 1.0 ! Compilador transforma essa semente em número real .
                call random_number(HARVEST = rtmp1) ! Gera número real aleatório normalizado.
                indexAleatorio = FLOOR(rtmp1 * numPontos) + 1 ! Originado número aleatório entre 1 e o número de pontos, uti.izando o número normalizado aleatório
            endif
        enddo
        
    enddo

    END SUBROUTINE

    SUBROUTINE calculoCentroideOr()

    END SUBROUTINE

    SUBROUTINE codificarGrupos(numGrupos,numPontos,colAgrupOrCompleto,groupNames,groupCode)
    integer :: i2,j2,itmp2, unique
    character*20 :: ctmp2
    integer, INTENT(IN) :: numGrupos,numPontos
    integer, INTENT(IN) :: groupCode(numGrupos)
    character*20, INTENT(IN) :: colAgrupOrCompleto(numPontos), groupNames(numGrupos)
    integer, INTENT(OUT) :: groupCode(numGrupos)

    itmp=0
    do i2 = 1,numPontos
        ctmp2 = colAgrupOrCompleto(i2)
        unique = 1
        do j2 = 1,numGrupos
            if (ctemp2 .eq. groupNames(j2)) then
                unique = 0
            endif
        enddo
        if (unique .eq. 1) then
            itmp = itmp + 1
            groupNames(itmp) = ctmp2
            groupCode(itmp) = itmp
        endif
    enddo   
    END SUBROUTINE