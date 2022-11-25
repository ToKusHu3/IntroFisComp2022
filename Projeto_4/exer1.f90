program exer1
    implicit none
    real(8) :: N0, dt, lambda, dN
    integer :: i, t

    read(*,*) N0
    read(*,*) dt
    read(*,*) lambda

    open(1, file='decai_out.dat', status='replace')

    write(1,*) 0.0d0, int(N0)

    t = (10.0d0/dt)

    do i = 1, t
        dN = (-1) * lambda * N0 * dt
        N0 = int(N0 + dN)

        write(1,*) (i*dt),int(N0)
    end do

    close(1)

end program exer1