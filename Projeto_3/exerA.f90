program exerA
    implicit none
    real(8), parameter :: m = 80.0d0, P = 400.0d0
    real(8) :: T, dt, v0, v
    real(8), dimension(2) :: tabela
    integer :: n_intervalos, i

    open(1, file='velA_out.dat', status='replace')

    read(*,*) T, dt, v0

    n_intervalos = int(T / dt)

    write(1, *) 0.0d0, v0

    do i = 1, n_intervalos

        v = v0 + ((P / (m * v0)) * dt)
        v0 = v

        write(1, *) (i * dt), v
    end do

    close(1) 


end program exerA

