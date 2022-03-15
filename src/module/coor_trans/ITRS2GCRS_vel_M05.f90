subroutine ITRS2GCRS_vel_M05(flag_M, M_rotation, P_ITRS, V_ITRS, V_GCRS)
    use M05
    implicit none 
!----declare variables----------
    integer(ik),  intent(in   )         :: flag_M
    real(rk),     intent(in   )         :: M_rotation(3, 3)
    real(rk),     intent(in   )         :: P_ITRS(3)
    real(rk),     intent(in   )         :: V_ITRS(3)
    real(rk),     intent(  out)         :: V_GCRS(3)

    !---------------------------
    integer(ik)                         :: i, j
    real(rk)                            :: coe(3), M(3,3)
!-------------------------------

    if(flag_M == 1) then
        M = transpose(M_rotation)
    else
        M = M_rotation
    end if

    call cross_product_M05(Omega, P_ITRS, coe)
    coe = coe + V_ITRS
    V_GCRS = matmul(M, coe)

end subroutine ITRS2GCRS_vel_M05