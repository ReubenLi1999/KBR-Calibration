subroutine get_gmst_M04(number_data, JD_TT, gmst)

    use M04
    implicit none 
!----declare variables----------
    integer(ik),  intent(in   )         :: number_data
    real(rk),     intent(in   )         :: JD_TT(1 : number_data)
    real(rk),     intent(  out)         :: gmst(1 : number_data)
    !---------------------------
    integer(ik)                         :: leapsecond(1 : number_data)
    real(rk)                            :: MJD_UTC_knots(1:368)
    real(rk)                            :: DUT1_knots(1:368)
    real(rk)                            :: DUT1(1 : number_data)
    integer(ik)                         :: N, i
!-------------------------------
    call leapsecond_M04(number_data, JD_TT, leapsecond)
    call data_segement_M04(number_data, leapsecond, JD_TT)
    ! write(*,*) segment
    ! write(*,*) number_segment

    do i = 1, number_segment
        call get_jiyi_data_M04(segment(i,3), MJD_UTC_knots, DUT1_knots)
        N = segment(i,2) - segment(i,1) + 1
        call interplation_M04(N, MJD_UTC_knots, DUT1_knots, JD_TT(segment(i,1):segment(i,2)), leapsecond(segment(i,1):segment(i,2)), DUT1(segment(i,1):segment(i,2)) )
    end do
    
    do i = 1, number_data
        call cal_gmst_M04(JD_TT(i), leapsecond(i), DUT1(i), gmst(i))
    end do
    ! write(*,*) DUT1
    ! write(*,*) gmst

end subroutine get_gmst_M04

subroutine cal_gmst_M04(JD_TT, leapsecond, DUT1, gmst)
    use M04
    implicit none
    real(rk) DUT1
    real(rk) jd,gmst,t,tu,ear
    real(rk) pi,dds2r
    real(rk) JD_TT
    integer(ik)     leapsecond
  
    jd = JD_TT - 32.184 - DUT1 - leapsecond
    pi=3.1415926
    dds2r=4.84813681109535e-06
    tu      = jd-2451545.0
    ear     =2*pi*(0.7790572732640+1.00273781191135448*tu)
    t       = ((jd+(32+32.184)/86400)-2451545.0)/36525.0
    ear    = mod(ear,2*pi)
  
    gmst    = ear+(0.014506 + (4612.156534 + (1.3915817 + ( 0.00000044 + ( 0.000029956 +( 0.0000000368 *t)*t)*t)*t)*t)*t)*dds2r
    gmst    = mod(gmst,2*pi)
  end subroutine cal_gmst_M04