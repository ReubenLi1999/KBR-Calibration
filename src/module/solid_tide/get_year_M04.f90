subroutine get_year_M04(JD_TT, leapsecond, year_UTC)
    use M04
    implicit none
!----declare variables----------
    real(rk),       intent(in   )         :: JD_TT
    integer(ik),    intent(in   )         :: leapsecond
    integer(ik),    intent(  out)         :: year_UTC 

    !---------------------------

    real(rk)                :: UTCMJD
    integer(ik)             :: a, b, c, d, e, Month
!-------------------------------


    UTCMJD = JD_TT - (32.184D0 + leapsecond) / 86400D0 - 2400000.5D0

    a = int(UTCMJD + 2400000.5 + 0.5)
    b = a + 1537
    c = int((b - 122.1) / 365.25)
    d = int(365.25 * c)
    e = int((b - d) / 30.60)
    Month = e - 1 - 12 * int(e / 14.0)
    year_UTC = c - 4715 - int((7 + Month) / 10.0)

end subroutine get_year_M04