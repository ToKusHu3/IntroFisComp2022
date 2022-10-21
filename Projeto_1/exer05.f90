program exer05
    implicit none
    real(16) :: precisao, erro = 1000000_16, lambda
    integer :: n
    integer :: i, j
    real(16), allocatable, dimension(:,:) :: A
    real(16), allocatable, dimension(:) :: y_0, y, x

    read *, precisao
    read *, n
    allocate(A(n,n))

    read *, A

    !chute inicial é y0 = (1, 0, 0, ..., 0)]
    allocate(y_0(n), y(n), x(n))
    y_0 = 0
    y_0(1) = 1
    

    do while (abs(erro) > precisao)
        x = matmul(A,y_0)
        y = x / sqrt(dot_product(x,x))
        erro = dot_product(y,matmul(A,y)) - lambda
        y_0 = y
        lambda = dot_product(y, matmul(A, y))
    end do

    print *, lambda
    
    do i = 1, size(y)
        print *, y(i)
    end do 

    deallocate(A, y_0, y, x)

end program