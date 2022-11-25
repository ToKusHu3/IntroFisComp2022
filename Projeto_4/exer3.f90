program exer3
        implicit none
        integer :: N, N0, T!Número de átomos iniciais N0 e os restantes N, T tempo total
        real(8) :: lambda, dt !constante de decaimento e intervalo de tempo
        integer :: i

        !leitura das variáveis
        open(1, file='decai_in', status='old')
        read(1,*) T
        read(1,*) N0
        read(1,*) dt
        read(1,*) lambda
        close(1)       

        open(2, file='decai_out', status='replace')

        N = N0

        write(2,*) 0.0d0, N0

        do i = 1, int(T / dt)
                N = N0 - (lambda * dt * N0) !Calculo do decaimento, conforme a realação dada 
                N0 = N
                write(2,*) (dt * i), N
        end do 

        close(2)

end program exer3
