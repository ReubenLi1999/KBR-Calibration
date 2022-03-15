subroutine cal_acc_M06(MU, P_third, position, acc)

    use M06
    implicit none 
!----declare variables----------
    real(rk),     intent(in   )         :: MU
    real(rk),     intent(in   )         :: P_third(3)
    real(rk),     intent(in   )         :: position(3)
    real(rk),     intent(  out)         :: acc(3)

    !---------------------------
    real(rk)                            :: L(3)         !P_third --> position
    real(rk)                            :: P_third_norm
    real(rk)                            :: L_norm
    integer(ik)                         :: i, j
!-------------------------------

    L = position - P_third
    P_third_norm = sqrt(dot_product(P_third, P_third))
    L_norm = sqrt(dot_product(L, L))

    do i = 1, 3
        acc(i) = - MU * (L(i) / L_norm**3 + P_third(i) / P_third_norm**3 )
    end do

end subroutine cal_acc_M06