subroutine leapsecond_M04(number_data, JD_TT, leapsecond)

    use M04
    implicit none
!----declare variables----------
    integer(ik),    intent(in   )       :: number_data
    real(rk),       intent(in   )       :: JD_TT(1 : number_data)
    integer(ik),    intent(  out)       :: leapsecond(1 : number_data)   

    !---------------------------
    integer(kind = 4)                   :: i, j 
    real(rk)                            :: date1
    real(rk)                            :: date_leap(7)
    integer(4) a(number_data)
!-------------------------------

    date_leap = (/50630.0, 51179.0, 53736.0, 54832.0, 56109.0, 57204.0, 57754.0/)
    do i = 1, number_data
        date1 = JD_TT(i) - MOD(JD_TT(i) - 0.5, 1.0)
        do j = 1, 7
            if(date1 - 2400000.5 < date_leap(j)) then 
                leapsecond(i) = 30 + j - 1
                exit
            end if
            if(j == 7) then
                leapsecond(i) = 30 + j
            end if
        end do
    end do
end subroutine leapsecond_M04