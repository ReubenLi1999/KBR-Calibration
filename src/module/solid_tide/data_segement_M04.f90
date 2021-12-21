subroutine data_segement_M04(number_data, leapsecond, JD_TT)

    use M04
    implicit none

!----declare variables----------
    integer(ik),    intent(in   )   :: number_data
    integer(ik),    intent(in   )   :: leapsecond(1 : number_data)
    real(rk),       intent(in   )   :: JD_TT(1 : number_data)
    !---------------------------
    integer(ik)                     :: segment_temporary(number_data, 3)
    integer(ik)                     :: N_temporary
    integer(ik)                     :: year_UTC_1, year_UTC_2

    integer(ik)                     :: i, j
!-------------------------------

    call get_year_M04(JD_TT(1), leapsecond(1), year_UTC_1)
    N_temporary = 0
    j = 1
    do i = 1, number_data
        call get_year_M04(JD_TT(i), leapsecond(i), year_UTC_2)
        if(year_UTC_2 > year_UTC_1) then
            N_temporary = N_temporary + 1
            segment_temporary(N_temporary, 1) = j
            segment_temporary(N_temporary, 2) = i - 1
            segment_temporary(N_temporary, 3) = year_UTC_1
            year_UTC_1 = year_UTC_2
            j = i
        end if
        if(i == number_data) then
            N_temporary = N_temporary + 1
            segment_temporary(N_temporary, 1) = j
            segment_temporary(N_temporary, 2) = i
            segment_temporary(N_temporary, 3) = year_UTC_1
        end if
    end do
    allocate(segment(1:N_temporary, 3))

    segment(1:N_temporary, :) = segment_temporary(1:N_temporary, :)
    number_segment = N_temporary
end subroutine data_segement_M04