
    ! Programa Gradiente Descendente (1d linear)
    program GD1d
        
    implicit none 
    

    integer :: i,ICOUNT, ie, window, pointsPerWin,ipoint, endPoint
    integer, parameter :: np = 100, nepochs=300000,  numWin = 5 !

    real*8 :: x(np),y(np),yp(np), xy(np,2)
    real*8 :: y_avg, erro2, Af, Bf
    real*8 :: rand,ruido,signal,TA=0.001, xlixo
    real*8 :: A(numWin), B(numWin), DA(numWin), DB(numWin), erro(nepochs,numWin)

    open ( unit =  1, file = 'OutputCode/dataset02.txt') 
    open ( unit =  2, file = 'OutputCode/output_erro.txt')
    open ( unit =  3, file = 'OutputCode/dataset2.txt')
    open ( unit =  4, file = 'OutputCode/output_coef.txt')
    
    
    if ((np / numWin) .LT. 2) then
        print *, "Número de janelas deve ser menor, tal que cada janela tenha ao menos 2 pontos."
    endif 
    pointsPerWin = int(np / numWin) 
    
    !CALL SRAND(2023)
    
    !do i = 1 , np
    !   
    !   x(i) = RAND(0)
    !   ruido = RAND(0)
    !   signal = RAND(0)
    !   
    !    if (signal .gt. 0.5) then 
    !        signal=-1.0
    !    else
    !        signal = 1.0
    !    endif
    !    
    !    y(i) = 2.d0*x(i) + signal*ruido/2.d0  + 1.0
    !    write(1,*) x(i), y(i)! , 2.d0*x(i) + 1.0
    !   
    !enddo    
    
    
    do i = 1 , np
        read(1,*) xy(i,1), xy(i,2)! , xlixo
        ! y(i) = 2.0*x(i) + 1.0
    enddo 
    
    CALL insertionSort(np,xy) ! ordenação dos dados para ser possível segmentar em janelas sequenciais.
    
    x = xy(:,1)
    y = xy(:,2)
    do i = 1,np
        write(3,'(f8.4, f8.4)')  x(i), y(i)
    enddo
    A(:) = 2.d0 !RAND(0)
    B(:) = 2.d0 !RAND(0)
    
    ipoint = 1
    endPoint = pointsPerWin
    
    do window = 1,numWin
        do i = ipoint, endPoint
            yp(i) = A(window)*x(i) + B(window)
        enddo   
        ipoint = pointsPerWin * window + 1
        endPoint = pointsPerWin * (window + 1)
    enddo
    
    do ie = 1, nepochs
        ipoint = 1 
        endPoint = pointsPerWin
        
        DA(:) = 0.d0
        DB(:) = 0.d0
        erro(ie,:) = 0.d0
        erro2 = 0.d0
        do window = 1,numWin

           do i = ipoint, endPoint
               DA(window) = DA(window) + x(i)*( y(i) - yp(i) )
               DB(window) = DB(window) + 1.0 *( y(i) - yp(i) )
           enddo
           
           DA(window) = -2.0*DA(window)/np
           DB(window) = -2.0*DB(window)/np
           
           A(window) = A(window) - ta*DA(window)
           B(window) = B(window) - ta*DB(window)
           
           do i = ipoint, endPoint
               yp(i) = A(window)*x(i) + B(window)
           enddo    
           y_avg = sum( y(ipoint:endPoint) ) / pointsPerWin
           !if(ie .eq. nepochs ) then
           !print *, 1 - ( sum( ( y(ipoint:endPoint) - yp(ipoint:&
           !endPoint) )**2 ) / sum( ( y(ipoint:endPoint) - y_avg)**2 ) )
           !endif
           erro(ie,window) = 1 - ( sum( (yp(ipoint:endPoint) - y_avg)**2 ) / sum( (y(ipoint:endPoint)-y_avg)**2 ) ) ! Erro = 1 - R2
           
           ipoint = pointsPerWin * window
           endPoint = pointsPerWin * (window + 1)
           ! print *, ie, A(window), B(window)
        enddo
        do i = 1, np
            erro2 = erro2 +  (y(i) - yp(i))**2
            erro2 = erro2/np
        enddo  
       
        !write(2,*) erro(ie,numWin)
    enddo
    
    print *, "janela    CoefA   CoefB"
    do window = 1,numWin
        print *,window,  A(window), B(window)
    enddo
    
    do ie = 1,nepochs
        
    enddo
    do i = 1,numWin
        write(*,*) 'Janela =', i,'epoch= ',nepochs,'erro= ', erro(nepochs,i)
    enddo
    write(*,*) 'Erro Médio das janelas (Usando R2) = ', sum(erro(nepochs,:)) / numWin
    write(*,*) 'Erro geral (Usando alternativa) = ', erro2
    write(4,*) 'Janela  A   B   ErroFinal'
    
    do i = 1,numWin
        write(4,*) i , A(i), B(i), erro(nepochs,i)
    enddo

    end program
    
SUBROUTINE insertionSort(numPontosTreino,MatrixDistGrupo)
    integer :: flagSorted, i , j
    real*8 :: tmp1,tmp2
    real*8 :: atmp(2)
    integer, INTENT(IN) :: numPontosTreino
    real*8, INTENT(INOUT) :: MatrixDistGrupo(1:numPontosTreino,1:2)


    flagSorted = -1
    do while ( flagSorted .ne. 1 ) ! Enquanto houver mudança de ordenação na iteração anterior.
        flagSorted = 1
        do i = 1,numPontosTreino - 1 ! iterar sobre todos os pontos comparando 2 a dois e possivelmente trocando as 2 posições
            if (MatrixDistGrupo(i,1) > MatrixDistGrupo(i+1,1) ) then
                ! se dist(i) > dist(i+1), troco as duas posições, tanto a distância quanto o agrupamento.
                atmp(:) = MatrixDistGrupo(i+1,:)
                MatrixDistGrupo(i+1,:) = MatrixDistGrupo(i,:)
                MatrixDistGrupo(i,:) = atmp(:)
                !FlagSorted se torna falso.
                flagSorted = 0
            endif
        enddo
    enddo

END SUBROUTINE
