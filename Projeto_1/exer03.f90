program exer03
    implicit none
    integer :: i, j, M
    logical, allocatable, dimension(:) :: primos

    open (1, file = 'primos_out.dat', status = 'replace')

    read *, M
    allocate(primos(M))
    primos = .true.

    i = 2
    j = i * i 
    do while ((j) <= M)
        if (primos(i) .eqv. .true.) then
            do j = (i*i), M, i
                primos(j) = .false.
            end do
        end if
        i = i + 1
        j = i * i
    end do

    do i = 2, (M)
        if (primos(i) .eqv. .true.) then 
            write(1,*) i
        end if
    end do  

    close(1)

    deallocate(primos)

end program exer03

