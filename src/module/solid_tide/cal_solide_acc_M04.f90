subroutine cal_solid_acc_M04(JD_TT, gmst, P_sun, P_moon, position, acc)
    use M04 
    implicit none 

!----declare variables----------
    real(rk),     intent(in   )         :: JD_TT
    real(rk),     intent(in   )         :: gmst
    real(rk),     intent(in   )         :: position(3)
    real(rk),     intent(in   )         :: P_sun(3)
    real(rk),     intent(in   )         :: P_moon(3)
    real(rk),     intent(  out)         :: acc(3)
    !---------------------------
    real(rk)                            :: sph_sun(3)
    real(rk)                            :: sph_moon(3)
    real(rk)                            :: C_solid(0:4, 0:4), S_solid(0:4, 0:4)
    integer(ik)                         :: i, j
    real(rk)                            :: pi
!-------------------------------
    call car2sph_M04( P_sun, sph_sun)
    call car2sph_M04( P_moon, sph_moon)
    call tide_angle_M04(JD_TT, gmst)
    call cal_matrix_CS_M04(sph_sun, sph_moon, position, C_solid, S_solid)
    ! call AccXyz_solid(4, C_solid, S_solid, position(1),position(2),position(3),acc(1),acc(2),acc(3))
    call cal_acc_M04(4, C_solid, S_solid, position, acc)
end subroutine cal_solid_acc_M04