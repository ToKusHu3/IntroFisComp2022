program exer01
implicit none
real(4) :: a_simples = 1.0e0
real(8) :: a_dupla = 1.0d0
real(16) :: a_quad = 1.0_16
integer(16) :: count1 = 0, count2 = 0, count3 = 0, aux = 0.0_16

!Precisão Simples
print *, 'Precisão Simples'
do while (1+a_simples /= 1)
    aux = a_simples + 1
    print *, a_simples, aux
    a_simples = a_simples / 2
    count1 = count1 + 1

end do


!Precisão Dupla
print *, 'Precisão Dupla'
do while (1+a_dupla /= 1)
    aux = a_dupla + 1
    print *, a_dupla, aux
    a_dupla = a_dupla / 2
    count2 = count2 + 1
end do


!Precisão Quadrupla
print *, 'Precisão Quadrupla'
do while (1+a_quad /= 1)
    aux = a_quad + 1
    print *, a_quad, aux
    a_quad = a_quad / 2
    count3 = count3 + 1

end do


print *, count1, a_simples
print *, count2, a_dupla
print *, count3, a_quad


end program exer01