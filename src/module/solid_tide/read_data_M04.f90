subroutine read_data_M04(file0, file1, file2)
    use M04
    implicit None

!----declare variables----------
    character(*),  intent(in   )        :: file0, file1, file2
    !---------------------------

    character(len = 30)                 :: str(9), str1
    integer(ik)                         :: i, j
!-------------------------------

    call GetFileLength_M04(file0, 0, n_tidal_k20)
    call GetFileLength_M04(file1, 0, n_tidal_k21)
    call GetFileLength_M04(file2, 0, n_tidal_k22)

    allocate(delaunay_k20(1:n_tidal_k20, 1:5), delaunay_k21(1:n_tidal_k21, 1:5), delaunay_k22(1:n_tidal_k22, 1:5))
    allocate(Amp_k20(1:n_tidal_k20, 1:2), Amp_k21(1:n_tidal_k21, 1:2), Amp_k22(1:n_tidal_k22))
    allocate(TA20(1:n_tidal_k20), TA21(1:n_tidal_k21), TA22(1:n_tidal_k22))
    open(11, file = file0, status = 'old')
    do i = 1, n_tidal_k20
        read(11, *) str, delaunay_k20(i,:), str1, Amp_k20(i, 1), str1, Amp_k20(i, 2)
    end do
    close(11)
    open(11, file = file1, status = 'old')
    do i = 1, n_tidal_k21
        read(11, *) str, delaunay_k21(i,:), str1, str1, Amp_k21(i, 1), Amp_k21(i, 2)
    end do
    close(11)
    open(11, file = file2, status = 'old')
    do i = 1, n_tidal_k22
        read(11, *) str, delaunay_k22(i,:), str1, Amp_k22(i)
    end do
    close(11)

    

end subroutine read_data_M04