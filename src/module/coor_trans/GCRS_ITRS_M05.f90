subroutine GCRS_ITRS_M05(transflag, JD_TT, M_Rotaion)

    use M05
    implicit none

    !flag=1, GCRS 2 ITRS; flag=2, ITRS 2 GCRS
    !----declare variables----------
    integer(ik),  intent(in   )         :: transflag
    integer(ik), parameter              :: number_data = 1_ik
    real(rk),     intent(in   )         :: JD_TT(number_data)
    real(rk),     intent(  out)         :: M_Rotaion(3, 3, number_data)

    !---------------------------
    integer(ik)                         :: leapsecond(1 : number_data)
    real(rk)                            :: MJD_UTC_knots(1: 368)
    real(rk)                            :: xp_knots(1: 368)
    real(rk)                            :: yp_knots(1: 368)
    real(rk)                            :: DUT1_knots(1: 368)

    real(rk)                            :: xp(1:number_data)
    real(rk)                            :: yp(1:number_data)
    real(rk)                            :: DUT1(1:number_data)
    integer(ik)                         :: N 

    integer(ik)                         :: i, j
    real(rk)                            :: pi

!-------------------------------
    pi = atan(1.0_rk) * 4.0_rk
    call leapsecond_M05(number_data, JD_TT, leapsecond)

    call data_segement_M05(number_data, leapsecond, JD_TT)

    do i = 1, number_segment
        call get_jiyi_data_M05(segment(i,3), MJD_UTC_knots, xp_knots, yp_knots, DUT1_knots)
        N = segment(i,2) - segment(i,1) + 1
        call interplation_M05(N, MJD_UTC_knots, xp_knots, yp_knots, DUT1_knots, JD_TT(segment(i,1):segment(i,2)), leapsecond(segment(i,1):segment(i,2)), xp(segment(i,1):segment(i,2)), yp(segment(i,1):segment(i,2)), DUT1(segment(i,1):segment(i,2)) )
        ! write(*,*) 
    end do
    xp = xp / 3600.0_rk / 180.0_rk * pi
    yp = yp / 3600.0_rk / 180.0_rk * pi
    do i = 1, number_data
        call iau_main_M05(transflag, JD_TT(i), leapsecond(i), DUT1(i), xp(i), yp(i), M_Rotaion(1:3, 1:3, i))
    end do

    DEALLOCATE(segment)


end subroutine GCRS_ITRS_M05