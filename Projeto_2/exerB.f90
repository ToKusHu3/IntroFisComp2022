program exerB
        implicit none 
        integer :: num_N, i, j
        integer, allocatable, dimension(:) :: valores_N
        real(8), allocatable, dimension(:) :: valores_h, tabela_saida
        real(8) :: f_menos_1, f0, f1, f2, f3, f4
        real(8) :: f, integral_exata
        real(8) :: valor_integral, h, trap, simp, bode, erro

        !Recebendo os valores de h do arquivo
        open(1, file='tabB_in.dat', status='old')

        read(1,*) num_N
        allocate(valores_N(num_N), valores_h(num_N), tabela_saida(5))
        read(1,*) valores_N
        close(1) 

        !Cálculo do valor exato da integral
        valor_integral = integral_exata(0.0d0, 1.0d0)

        open(2, file='tabB_out.dat', status='replace')

        do i = 1, num_N
                valores_h(i) = 1.0d0 / (valores_N(i))
                h = valores_h(i)
                tabela_saida(1) = valores_N(i) !1° coluna
                tabela_saida(2) = h !2° coluna

                trap = 0
                simp = 0
                bode = 0

                !Regra do Trapézio e Regra de Simpson
                do j = 1, (valores_N(i)), 2
                
                        !Definição dos pontos de interesse
                        f_menos_1 = f((j - 1) * h)
                        f0 = f(j * h)
                        f1 = f((j + 1) * h)

                        trap = trap + (h / 2.0d0) * (f1 + 2 * f0 + f_menos_1) !Valor da aproximação pela regra do trapézio para um intervalo de 2h
                        simp = simp + (h / 3.0d0) * (f1 + 4 * f0 + f_menos_1) !Valor da aproximação pela regra de Simpson para um intervalo de 2h           
                end do

                erro = abs(valor_integral - trap)
                tabela_saida(3) = erro 

                erro = abs(valor_integral - simp)
                tabela_saida(4) = erro

                !Regra de Bode
                do j = 1, (valores_N(i)), 4
                        !Definição dos pontos de interesse
                        f0 = f((j - 1) * h)
                        f1 = f(j * h)
                        f2 = f((j + 1) * h)
                        f3 = f((j + 2) * h)
                        f4 = f((j + 3) * h)

                        bode = bode + ((2.0d0 * h) / 45.0d0) * (7 * f0 + 32 * f1 + 12 * f2 + 32 * f3 + 7 * f4) !Valor da aproximação para um intervalo de 4h
                end do

                erro = abs(valor_integral - bode)
                tabela_saida(5) = erro 

                write(2,*) tabela_saida   
        end do

        print *, 'Com base nos valores observados na tabela, temos que para a regra do trapézio e para a regra de Simpson', &
        ' que o valor mais adequado de N é o de 4096, pois com ele o valor atribuido para h é minimizado e, consequentemente', &
        ' os desvios também. No entanto, para a regra de Bode os melhores valores de N foram de 1024 e 2048.'

        close(2)            

end program exerB

!Função que calcula o valor de f para um ponto x
real(8) function f(x)
        implicit none
        real(8) :: x 

        f = sin(x * 0.5d0) * cos(x)
end function f

!Função que calcula a integral da função dado o intervalo
real(8) function integral_exata(a, b)
        implicit none 
        real(8) :: a, b
        !integral da função sen(x/2)cos(x) é dada por 
        integral_exata = (2 * (0.5d0 * cos(0.5d0 * b) - (1.0d0/6.0d0) * cos(1.5d0 * b))) - & 
        (2 * (0.5d0 * cos(0.5d0 * a) - (1.0d0/6.0d0) * cos(1.5d0 * a)))
end function integral_exata