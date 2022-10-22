program exerA
        implicit none 
        real(8), parameter :: x0 = (1.0d0/3.0d0)
        integer :: n_h, i
        real(8), allocatable, dimension(:) :: valores_h, tabela
        real(8) :: f, f_menos_2, f_menos_1, f0, f1, f2                         !Definição dos valores da função
        real(8) :: derivada_primeira, derivada_segunda, derivada_terceira      !Valores exatos das derivadas
        real(8) :: aux, erro, h


        !Recebendo os valores de h do arquivo
        open(1, file='tabA_in.dat', status='old')

        read(1,*) n_h
        allocate(valores_h(n_h), tabela(7))
        read(1,*) valores_h

        close(1)        

        !Definindo o valor exato das derivadas no ponto x0
        derivada_primeira = (0.5 * exp(4 * x0)) * (8 * cos(0.5 * x0) - sin(0.5 * x0))
        derivada_segunda = (0.25 * exp(4 * x0)) * (63 * cos(0.5 * x0) - 16 * sin(0.5 * x0))
        derivada_terceira = ((1.0d0/8.0d0) * exp(4 * x0)) * (488 * cos(0.5*x0) - 191 * sin(0.5*x0))

        open(2, file='tabA_out.dat', status='replace')

        !Loop para calcular os valores para cada h
        do i = 1, n_h

                h = valores_h(i)
                tabela(1) = h

                !Definindo os valores da função em cada ponto para fazer a aproximação numérica
                f0 = f(h, 0)
                f1 = f(h, 1)
                f_menos_1 =  f(h, -1)
                f2 = f(h, 2)
                f_menos_2 = f(h, -2)

                !derivada simétrica 3 pontos
                aux = (f1 - f_menos_1) / (2*h)
                erro = abs(derivada_primeira - aux)
                tabela(2) = erro 

                !derivada para frente 2 pontos     
                aux = (f1 - f0) / h
                erro = abs(derivada_primeira - aux)
                tabela(3) = erro
        
                !derivada pra trás 2 pontos
                aux = (f0 - f_menos_1 )/ h
                erro = abs(derivada_primeira - aux)
                tabela(4) = erro

                !derivada segunda simétrica 3 pontos
                aux = (f1 - 2*f0 + f_menos_1) / (h*h)
                erro = abs(derivada_segunda - aux)
                tabela(5) = erro 

                !derivada segunda simétrica 5 pontos 
                aux = (-f2 + 16*f1 - 30*f0 + 16*f_menos_1 - f_menos_2) / (12 * (h * h))
                erro = abs(derivada_segunda - aux)
                tabela(6) = erro 

                !derivada terceira anti-simétrica 5 pontos
                aux = (f2 - 2*f1 + 2*f_menos_1 - f_menos_2) / (2 * (h * h * h))
                erro = abs(derivada_terceira - aux)
                tabela(7) = erro

                write(2,*) tabela 
        end do

        print *, 'Considerando os resultados observados na tabela gerada, podemos tirar algumas conclusoes a respeito dos desvios',&
        ' calculados. Primeiramente, podemos concluir que um valor menor de h não necessáriamente implica em valores ', &
        'menores para o desvio. Devido a isso, deve-se avaliar as ordens de grandeza do erro em cada caso para que se faça ', &
        'a escolha de uma aproximação adequada que minimize o desvio. Para a derivada simetrica com 3 pontos o menor ', &
        'desvio ocorre quando h e da ordem de 10^-7. Para as derivadas para frente e para trás com 2 pontos o melhor ', &
        'valor de h é da ordem de 10^-8. Para a derivada segunda simetrica com 3 pontos o valor mais apropriado de h é ', &
        'da ordem de 10^-5, enquanto que para 5 pontos o mais adequado é da ordem de 10^-4. Enfim, para a derivada ', &
        'terceira antisimétrica de 5 pontos o valor de h que mais se adequa e da ordem de 10^-4.'



end program exerA

!Função para calcular f(x) no ponto
real(8) function f(h, n)
        implicit none
        integer :: n 
        real(8), parameter :: x0 = (1.0d0/3.0d0) 
        real(8) :: h, x 

        x = x0 + (n * h)

        f = exp(4.0d0*x) * cos(x*0.5d0)

end function f 
