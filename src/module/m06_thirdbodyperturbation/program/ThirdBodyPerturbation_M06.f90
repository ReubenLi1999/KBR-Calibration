subroutine ThirdBodyPerturbation_M06(Number_data, P_sun, P_moon, position, acc)

    use M06
    implicit none 
    !----declare variables----------
    integer(ik),  intent(in   )         :: Number_data
    real(rk),     intent(in   )         :: P_sun(1:Number_data, 3)
    real(rk),     intent(in   )         :: P_moon(1:Number_data, 3)
    real(rk),     intent(in   )         :: position(1:Number_data, 3)
    real(rk),     intent(  out)         :: acc(1:Number_data, 3)

    !---------------------------
    real(rk)                            :: acc_moon(3), acc_sun(3)
    integer(ik)                         :: i, j
!-------------------------------

    do i = 1, Number_data
        Call cal_acc_M06(MU_sun,  P_sun(i,1:3),  position(i,1:3), acc_sun)
        Call cal_acc_M06(MU_moon, P_moon(i,1:3), position(i,1:3), acc_moon)
        acc(i, 1:3) = acc_moon(1:3) + acc_sun(1:3)
    end do

end subroutine ThirdBodyPerturbation_M06