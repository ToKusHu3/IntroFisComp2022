program exerC
    integer :: n_interacoes, i, j
    real(8) :: f, derivada_f
    real(8), dimension(10) :: tabela_saida
    real(8), dimension(3) :: x_plus, x_minus, bdr !Busca direta
    real(8), dimension(3) :: x0_nr, newton_raphson, x0_sec, x1_sec, secante !Newton-Raphson e Secante
    real(8) :: xi, xi_minus !variáveis auxiliares para o metódo da secante

    read *, n_interacoes

    open(1, file='tabC_out.dat', status='replace')

    x_plus = (/-1.0d0, 0.0d0, 2.0d0/) !valores de x para a qual f(x) é positiva
    x_minus = (/-2.0d0, 1.0d0, 1.0d0/) !valores de x para a qual f(x) é negativa

    x0_nr = (/-1.5d0, 0.5d0, 1.5d0/) !Chutes para os valores da raíz para o método de Newton-Raphson

    x0_sec = (/-2.00d0, 0.00d0, 2.25d0/) !Chute para o valor de x0 usado no método da secante
    x1_sec = (/-1.99d0, 0.01d0, 2.24d0/) !Chute para o valor de x1 usado no método da secante

    do j = 1, n_interacoes
            tabela_saida(1) = j        
            
            !Busca Direta
            !Chutar dois valores de x tais que f(x+) > 0 e f(x-) < 0, dessa forma como a função muda
            !de valor, haverá uma raíz nesse intervalo, então reduziremos o intervalo para aproximar o valor da raíz
            !Chute inicial r1[-2,-1], r2[0,1], r3[1,2]
            do i = 1, 3
                bdr(i) = (x_plus(i) + x_minus(i)) / 2

                if (f(bdr(i)) > 0) then
                    x_plus(i) = bdr(i)
                else
                    x_minus(i) = bdr(i)
                end if

                tabela_saida(i+1) = bdr(i)
            end do 

            !Método de Newton-Raphson
            do i = 1, 3

                if (j == 1) then
                    tabela_saida(i+4) = x0_nr(i)
                else 
                    x0_nr(i) = x0_nr(i) - (f(x0_nr(i)) / derivada_f(x0_nr(i)))
                    newton_raphson(i) = x0_nr(i)
                    tabela_saida(i+4) = newton_raphson(i)
                end if
            end do

            !Método da secante
            do i = 1, 3
                if (j == 1) then
                    tabela_saida(i+7) = x0_sec(i)
                else if (j == 2) then 
                    tabela_saida(i+7) = x1_sec(i)
                else
                    xi_minus = x0_sec(i)
                    xi = x1_sec(i)
                    x0_sec(i) = xi

                    x1_sec(i) = xi - f(xi) * ((xi - xi_minus) / (f(xi) - f(xi_minus)))

                    secante(i) = x1_sec(i)
                    tabela_saida(i+7) = secante(i)
                end if 

            end do 
                    
            write(1, *) tabela_saida
    end do



    close(1)

end program exerC

!Calcular o valor de f no ponto x
real(8) function f(x)
        implicit none
        real(8) :: x 

        f = (x ** 3) - (x ** 2) - 2 * x + 1
end function f

!Calcular o valor da derivada em x
real(8) function derivada_f(x)
        implicit none
        real(8) :: x

        derivada_f = 3 * (x ** 2) - 2 * x - 2
end function derivada_f