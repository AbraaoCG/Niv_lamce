program KNNeighbors
    implicit none

    integer :: itmp , numNeighbors,numGrupos,numVariaveis,numPontos
    integer, allocatable ::
    real*8 :: tmp
    real*8  ,allocatable  ::  dados(:,:), agrupamentoDados(:)
    character*20, allocatable  :: header(:), groupNames(:)
    character*20 :: atmp

    open(unit=19, file = 'args.txt')
    read (19, '(A10 , i4.4)') ctmp , numGrupos
    read (19, '(A7 , i4.4)') ctmp , numVariaveis
    read (19, '(A16 , i4.4)') ctmp , numPontos
    read (19, '(A16 , i4.4)') ctmp , numNeighbors
    
    

end program