subroutine get_jiyi_data_M05(year_UTC, MJD_UTC, xp, yp, DUT1)
    use M05
    use xml_module
    implicit none

!----declare variables----------
    integer(ik),  intent(in   )         :: year_UTC
    real(rk),     intent(  out)         :: MJD_UTC(1:368)
    real(rk),     intent(  out)         :: xp(1:368)
    real(rk),     intent(  out)         :: yp(1:368)
    real(rk),     intent(  out)         :: DUT1(1:368)
    !---------------------------
    
    character(len = 200)                :: jiyi_file_1, jiyi_file_2

    integer(ik)                         :: i, n
    real(kind = 8)                      :: coe
    CHARACTER(len=:), ALLOCATABLE       :: config_path
    integer(ip)                         :: fplerror
!-------------------------------

    MJD_UTC = 0
    xp = 0
    yp = 0
    DUT1 = 0
    !write(*,*) year_UTC
    write(jiyi_file_1, '(I4)') year_UTC
    write(jiyi_file_2, '(I4)') year_UTC + 1
    ! write(*,*) year_UTC
    
    !> get config path
    fplerror = xml_i%urlpaths%GetAsString(key='ConfigPath', string=config_path)

    jiyi_file_1 = trim(config_path) // trim(jiyi_file_1) // 'JIYI.txt'
    
    jiyi_file_2 = trim(config_path) // trim(jiyi_file_2) // 'JIYI.txt'
    call GetFileLength_M05(jiyi_file_1, 14, n)
    open(11, file = jiyi_file_1, status = 'old')
        do i = 1, 14
            read(11, *)
        end do
        do i = 1, n
            read(11, *) coe, coe, coe, MJD_UTC(i), xp(i), yp(i), DUT1(i)
        end do
    close(11)
    open(12, file = jiyi_file_2, status = 'old')
    do i = 1, 14
        read(12, *)
    end do
    do i = 1, 2
        read(12, *) coe, coe, coe, MJD_UTC(n+i), xp(n+i), yp(n+i), DUT1(n+i)
    end do
    close(12)



end subroutine get_jiyi_data_M05