program exer1
        implicit none
        integer :: N, N0 !Número de átomos iniciais N0 e os restantes N
        real(8) :: lambda, dt !constante de decaimento e intervalo de tempo
        real(8), allocatable, dimension(:) :: matriz_atomos
        integer :: i, j, atomos_decaidos

        !leitura das variáveis
        read(*,*) N0
        read(*,*) dt
        read(*,*) lambda

        !Alocando as N0 posições na matriz e colocando seus valores como 1
        allocate(matriz_atomos(N0))
        matriz_atomos = 1 

        open(1, file='decai_out', status='replace')

        atomos_decaidos = 0
        N = N0

        write(1,*) 0.0d0, N0

        do i = 1, int(10.0d0 / dt)
                do j = 1, N0
                        if (matriz_atomos(j) == 1) then !Verificando se o átomo não está decaido
                                if (rand() <= lambda * dt) then !Condição para que o átomo decaia
                                        matriz_atomos(j) = 0
                                        atomos_decaidos = atomos_decaidos + 1
                                end if
                        end if
                end do
                N = N - atomos_decaidos
                atomos_decaidos = 0
                write(1,*) (i * dt), N
        end do 

        close(1)       

end program exer1
