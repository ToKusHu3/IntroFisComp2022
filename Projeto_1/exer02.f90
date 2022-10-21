program exer02
    implicit none
    !Declaração de variaveis
    integer(4) :: sinal = 0, fat = 1, fatorial, i
    real(4) :: x_s = 0.1e0, sin_simples, termo_taylor_s = 0, precisao_s, p_s
    real(8) :: x_d = 0.1d0, sin_dupla, termo_taylor_d, precisao_d, p_d

    do i = 1, 4
        !Definição das Variáveis para Precisão Simples
        x_s = 0.1e0
        x_s = x_s * i
        sin_simples = x_s
        fat = 3
        sinal = 1
        termo_taylor_s = ((-1) ** sinal) * ((x_s ** fat)/fatorial(fat))

        !Série de Taylor para Precisão Simples
        do while (sin_simples /= (sin_simples+termo_taylor_s)) 
            p_s = precisao_s
            termo_taylor_s = ((-1) ** sinal) * ((x_s ** fat)/fatorial(fat)) 
            sin_simples = sin_simples + termo_taylor_s
            fat = fat + 2
            sinal = sinal + 1
            precisao_s = abs(termo_taylor_s/sin_simples)
        end do

        !Definição das Variáveis para Precisão Dupla
        x_d = 0.1d0
        x_d = x_d * i
        sin_dupla = x_d
        fat = 3
        sinal = 1
        termo_taylor_d = ((-1) ** sinal) * ((x_d ** fat)/fatorial(fat))

        !Série de Taylor para Precisão Dupla
        do while (sin_dupla /= (sin_dupla+termo_taylor_d))
            p_d = precisao_d
            termo_taylor_d = ((-1) ** sinal) * ((x_d ** fat)/fatorial(fat))
            sin_dupla = sin_dupla + termo_taylor_d
            fat = fat + 2
            sinal = sinal + 1
            precisao_d = abs(termo_taylor_d/sin_dupla)
        end do
        print *, x_s, p_s, p_d

        
    end do

    print *, 'Sim, as funções trigonométricas podem ser aproximadas por séries, pois ao utilizarmos uma série nós&
       & podemos obter uma precisão tão boa quanto se queira para o seu valor, sendo ele limitado apenas pelo formato de precisão escolhido.'

end program exer02

integer function fatorial(n)
    implicit none
    integer :: n, i = 1, fat = 1
    do while(i <= n)
        fat = fat * i
        i = i + 1
    end do
    fatorial = fat 
end function

