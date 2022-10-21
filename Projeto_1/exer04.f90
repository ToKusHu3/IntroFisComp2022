program exer04
    implicit none
    integer :: i, j
    real :: produto_misto, calcular_area 
    real, dimension(3) :: coordenada_1, coordenada_2, coordenada_3, coordenada_4 !vetores dados
    real, dimension(3) :: v1, v2, v3, v4, v5 !vetores que formam o tetraedro
    real :: area_total, volume 
    real, dimension(4) :: lista_areas

    !leitura dos vetores
    open(1, file = 'vet_in.dat', status = 'old')

    read(1,*) (coordenada_1(i), i = 1,3)
    read(1,*) (coordenada_2(i), i = 1,3)
    read(1,*) (coordenada_3(i), i = 1,3)
    read(1,*) (coordenada_4(i), i = 1,3)
    close(1)

    !criando os vetores que formam o tetraedro
    do i = 1, 3
    v1(i) = coordenada_2(i) - coordenada_1(i)
    v2(i) = coordenada_3(i) - coordenada_1(i)
    v3(i) = coordenada_4(i) - coordenada_1(i)
    v4(i) = coordenada_3(i) - coordenada_2(i)
    v5(i) = coordenada_4(i) - coordenada_2(i)
    end do

    !calculo do volume
    produto_misto = ((v1(2)*v2(3)*v3(1) + v1(3)*v2(1)*v3(2) + v1(1)*v2(2)*v3(3)) - &
                        (v1(3)*v2(2)*v3(1) + v1(1)*v2(3)*v3(2) + v1(2)*v2(1)*v3(3)))
    volume = abs(produto_misto) / 6   

    !calculo da area das faces e alocação na lista
    lista_areas(1) = calcular_area(v1,v2)
    lista_areas(2) = calcular_area(v1,v3)
    lista_areas(3) = calcular_area(v2,v3)
    lista_areas(4) = calcular_area(v4,v5)

    !escrita dos valores no arquivo
    open(2, file = 'tetra_out.dat', status='replace')

    write(2,*) volume

    do i = 1, 4
       area_total = area_total + lista_areas(i)
    end do
    
    write(2,*) area_total
    
    call InsertionSort(lista_areas)
    
    do i = 1, 4
        do j = i + 1, 4
            if (lista_areas(i) == lista_areas(j)) then
                lista_areas(j) = 0
            end if 
        end do 
    end do

    do i = 1, 4
        if (lista_areas(i) /= 0) then
            write(2,*) lista_areas(i)
        end if
    end do 

    close(2)

contains
subroutine InsertionSort(lista)
        real, intent(inout) :: lista(4)

        integer :: i, j
        real :: temp

        do i = 1, 4 
                j = i
                temp = lista(j)
                do while ( j > 1 .AND. lista(j - 1) > temp)
                        lista(j) = lista(j - 1)
                        j = j - 1
                end do
                lista(j) = temp
        end do
end subroutine InsertionSort

end program exer04

real function calcular_area(v1, v2)
        implicit none
        real, dimension(3) :: v1, v2, produto_vetorial
        real :: modulo

        produto_vetorial(1) = (v1(2)*v2(3) - v1(3)*v2(2))
        produto_vetorial(2) = (v1(3)*v2(1) - v1(1)*v2(3))
        produto_vetorial(3) = (v1(1)*v2(2) - v1(2)*v2(1))

        modulo = sqrt((produto_vetorial(1) ** 2) + (produto_vetorial(2) ** 2) + (produto_vetorial(3) ** 2))

       calcular_area = modulo/2
end function 

