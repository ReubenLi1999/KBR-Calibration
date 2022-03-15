subroutine solid_tide_M04(number_data, JD_TT, position, P_sun, P_moon, acc)
    use M04
    use xml_module
    implicit none

!----declare variables----------
    integer(ik),  intent(in   )         :: number_data
    real(rk),     intent(in   )         :: JD_TT(1 : number_data)
    real(rk),     intent(in   )         :: position(1 : number_data, 3)
    real(rk),     intent(in   )         :: P_sun(1 : number_data, 3)
    real(rk),     intent(in   )         :: P_moon(1 : number_data, 3)
    real(rk),     intent(  out)         :: acc(1 : number_data, 3)
    !---------------------------
    character(len = 1000)               :: file_tidal_k20, file_tidal_k21, file_tidal_k22
    real(rk)                            :: gmst(1: number_data)
    integer(ik)                         :: i, j
    CHARACTER(len=:), ALLOCATABLE       :: config_path
    integer(ip)                         :: fplerror
!-------------------------------

    fplerror = xml_i%urlpaths%GetAsString(key='ConfigPath', string=config_path)

    file_tidal_k20 = trim(config_path)//'k20.txt'
    file_tidal_k21 = trim(config_path)//'k21.txt'
    file_tidal_k22 = trim(config_path)//'k22.txt'
    call read_data_M04(file_tidal_k20, file_tidal_k21, file_tidal_k22)
    call get_gmst_M04(number_data, JD_TT, gmst)

    do i = 1, number_data
        call cal_solid_acc_M04(JD_TT(i), gmst(i), P_sun(i,1:3), P_moon(i,1:3), position(i,1:3), acc(i,1:3))
    end do

    deallocate(segment)
    deallocate(delaunay_k20, delaunay_k21, delaunay_k22)
    deallocate(Amp_k20, Amp_k21, Amp_k22)
    deallocate(TA20, TA21, TA22)

end subroutine solid_tide_M04