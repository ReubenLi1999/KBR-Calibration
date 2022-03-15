subroutine interplation_M05(num, MJD_UTC_knots, xp_knots, yp_knots, DUT1_knots, JD_TT, leapsecond, xp, yp, DUT1)

    use M05
    implicit none
!----declare variables----------
    integer(ik),  intent(in   )         :: num
    integer(ik),  intent(in   )         :: leapsecond(1 : num)
    real(rk),     intent(in   )         :: MJD_UTC_knots(1 : 368)
    real(rk),     intent(in   )         :: xp_knots(1 : 368)
    real(rk),     intent(in   )         :: yp_knots(1 : 368)
    real(rk),     intent(in   )         :: DUT1_knots(1 : 368)
    real(rk),     intent(in   )         :: JD_TT(1 : num)
    real(rk),     intent(  out)         :: xp(1 : num)
    real(rk),     intent(  out)         :: yp(1 : num)
    real(rk),     intent(  out)         :: DUT1(1 : num)

    !---------------------------

    integer(ik)                         :: i, j, k, l
    real(rk)                            :: MJD_UTC

!-------------------------------

    k = 1
    do i = 1 , 368
        do j = k, num
            MJD_UTC = JD_TT(j) - (32.184D0 + leapsecond(j)) / 86400D0 - 2400000.5D0
            if(MJD_UTC > MJD_UTC_knots(i) .and. MJD_UTC < MJD_UTC_knots(i + 1)) then
                call interplation_3points_M05(MJD_UTC, MJD_UTC_knots(i : i +2), xp_knots(i : i+2), xp(j))
                call interplation_3points_M05(MJD_UTC, MJD_UTC_knots(i : i +2), yp_knots(i : i+2), yp(j))
                call interplation_3points_M05(MJD_UTC, MJD_UTC_knots(i : i +2), DUT1_knots(i : i+2), DUT1(j))
                ! write(*,*) DUT1_knots(i : i+2), DUT1(j)
                l = j + 1   
            elseif(MJD_UTC == MJD_UTC_knots(i)) then
                DUT1(j) = DUT1_knots(i)
                xp(j) = xp_knots(i)
                yp(j) = yp_knots(i)
                l = j + 1   
            elseif(MJD_UTC >= MJD_UTC_knots(i + 1)) then
                l = j
                exit
            end if
        end do
        if(l == num + 1) then
            exit
        end if
        k = l
    end do
    ! write(*,*) i
    
end subroutine interplation_M05