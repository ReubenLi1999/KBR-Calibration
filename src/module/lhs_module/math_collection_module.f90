module math_collection_module
    use num_kinds_module
    use logger_mod, only: logger_init, logger => master_logger
    use lsqr_kinds
    use lsqr_module
    use random_integer_module
    use io_file_module
    use ogpf
    use pyplot_module
    implicit none
    !>----------------------------------------------------------------------------------------------
    !@      Arthur          => Hao-si Li
    !@      Revision        => 210206 LHS first edition, non oop
    !@                      => 210718 LHS second edition, add some functions to operate the strings
    !>
    !>      Description:
    !>          This is a module that contains subroutines and functions which are not compatible to
    !>          encapsulate in a object-oriented module for now. Maybe the OOP revision will come up
    !>          in the future.
    !>----------------------------------------------------------------------------------------------
    
    real(kind=wp), parameter                         :: pi = atan(1.0_wp) * 4.0_wp
    
contains

    subroutine date2num(c_date, ip_date)
        !> input 
        CHARACTER(len=*)                , INTENT(IN   )                 :: c_date
        !> output
        integer(kind=ip), DIMENSION(6)  , intent(  out)                 :: ip_date
        !> local
        integer(kind=ip)                                                :: err

        !> year
        call str2int(c_date(1: 4), ip_date(1), err)
        if (err /= 0) then
            call logger%fatal("math_collection_module", "Error opening the folder walker list file")
        end if
        !> month
        call str2int(c_date(6: 7), ip_date(2), err)
        if (err /= 0) then
            call logger%fatal("math_collection_module", "Error opening the folder walker list file")
        end if
        !> day
        call str2int(c_date(9: 10), ip_date(3), err)
        if (err /= 0) then
            call logger%fatal("math_collection_module", "Error opening the folder walker list file")
        end if
        !> hour
        call str2int(c_date(12: 13), ip_date(4), err)
        if (err /= 0) then
            call logger%fatal("math_collection_module", "Error opening the folder walker list file")
        end if
        !> minute
        call str2int(c_date(15: 16), ip_date(5), err)
        if (err /= 0) then
            call logger%fatal("math_collection_module", "Error opening the folder walker list file")
        end if
        !> second
        call str2int(c_date(18: 19), ip_date(6), err)
        if (err /= 0) then
            call logger%fatal("math_collection_module", "Error opening the folder walker list file")
        end if
        
    end subroutine date2num

    pure function second_order_diff(ip_length, ip_overlap) result(wp_coeffs)
        !> input variables
        integer(kind=ip)                , INTENT(IN   )                 :: ip_length 
        !> the length of the second-order differentiator
        integer(kind=ip)                , INTENT(IN   )                 :: ip_overlap
        !> the overlap length
    
        !> output variable
        real(kind=wp), DIMENSION(0: ip_length)                          :: wp_coeffs
    
        !> args
        integer(kind=ip)                                                :: n, i, k, j, m
        real(kind=wp)                                                   :: wp_temp1, wp_temp2, wp_temp3
    
        n_loop: do n = 0, ip_length, 1
            if (n /= ip_overlap) then
                wp_temp1 = 0.0_wp;
                m_loop: do m = 0, ip_length, 1
                    if (m /= n .and. m /= ip_overlap) then
                        wp_temp2 = 1.0_wp
                        k_loop: do k = 0, ip_length, 1
                            if (k /= m .and. k /= ip_overlap .and. k /= n) wp_temp2 = wp_temp2 * (real (ip_overlap, wp) - real(k, wp)) / (real(n, wp) - real(k, wp))
                        end do k_loop
                        wp_temp1 = wp_temp1 + wp_temp2 / (real(n, wp) - real(m, wp))
                    end if
                end do m_loop
                wp_coeffs(n) = wp_temp1 * 2.0_wp / (real(n, wp) - real(ip_overlap, wp))
            else
                wp_temp1 = 0.0_wp
                wp_temp2 = 0.0_wp
                wp_temp3 = 0.0_wp
    
                do k = 1, ip_overlap, 1
                    do m = k + 1, ip_overlap, 1
                        wp_temp1 = wp_temp1 + 1.0_wp / real(m, wp) / real(k, wp)
                    end do
                end do
    
                do k = 1, ip_length - ip_overlap, 1
                    do m = k + 1, ip_length - ip_overlap, 1
                        wp_temp2 = wp_temp2 + 1.0_wp / real(m, wp) / real(k, wp)
                    end do
                end do
    
                do k = 1, ip_overlap
                    do m = 1, ip_length - ip_overlap
                        wp_temp3 = wp_temp3 + 1.0_wp / real(m, wp) / real(k, wp) 
                    end do
                end do
                wp_coeffs(n) = 2.0_wp * (wp_temp1 + wp_temp2 - wp_temp3)
            end if
        end do n_loop
    end function second_order_diff

    function included_angle(wp_vec1, wp_vec2) result(wp_included_angle)
        !> input
        real(kind=wp)                   , intent(in   )                 :: wp_vec1(:), wp_vec2(:)
        
        real(kind=wp)                                                   :: wp_included_angle
        
        wp_included_angle = acos(dot_product(wp_vec1, wp_vec2) / norm2(wp_vec1) / norm2(wp_vec2))
    end function included_angle

    elemental subroutine str2int(str, int, stat)
        ! Arguments
        character(len=*),intent(in) :: str
        integer,intent(out)         :: int
        integer,intent(out)         :: stat

        read(str,*,iostat=stat)  int
    end subroutine str2int

    function std(wp_input) result(wp_std)
        !> input
        real(kind=wp)                   , INTENT(IN   )                 :: wp_input(:)
        
        !> temp
        real(kind=wp)                                                   :: x(size(wp_input))
        real(kind=wp)                                                   :: wp_mean
        real(kind=wp)                                                   :: wp_variance
        integer(kind=ip)                                                :: i, n
        
        !> output
        real(kind=wp)                                                   :: wp_std
        
        x = wp_input
        !> length of th array
        n = size(x)
        !> mean
        wp_mean = sum(x) / n
        
        !> init variance
        wp_variance = 0.0_wp
        do i = 1, n, 1
            wp_variance = wp_variance + (x(i) - wp_mean)**2
        end do
        wp_variance = wp_variance / (n - 1)
        wp_std = sqrt(wp_variance)
        
    end function std

    function sort(wp_input) result(wp_output)
        !> input
        real(kind=wp)                   , INTENT(IN   )                 :: wp_input(:)
        !> output
        real(kind=wp)                                                   :: wp_output(size(wp_input))
        
        !> temp
        real(kind=wp)                                                   :: wp_temp
        real(kind=wp)                                                   :: wp_temp_array(size(wp_input))
        integer(kind=ip)                                                :: ip_j, ip_k
        
        
        
        wp_temp_array = wp_input
        do ip_j = 1, ubound(wp_temp_array, 1) - 1
            do ip_k = ip_j + 1, ubound(wp_temp_array, 1)
                if (wp_temp_array(ip_j) > wp_temp_array(ip_k)) then
                    wp_temp = wp_temp_array(ip_k)
                    wp_temp_array(ip_k) = wp_temp_array(ip_j)
                    wp_temp_array(ip_j) = wp_temp
                end if
            end do
        end do
        
    end function sort

    function expec_without_outlier(wp_input) result(wp_output)
        !> input
        real(kind=wp)                   , INTENT(IN   )                 :: wp_input(:)
        
        !> output 
        real(kind=wp)                                                   :: wp_output(2)
        
        !> temp
        real(kind=wp)                                                   :: wp_dis_from_ave(size(wp_input))
        real(kind=wp)                                                   :: wp_ave
        INTEGER(kind=ip)                                                :: i
        logical                                                         :: mask(size(wp_input))
        
        !> initialise the mask array
        mask = .true.
        !> mean of the array
        wp_ave = sum(wp_input) / size(wp_input)
        !> distance from the mean
        wp_dis_from_ave = wp_input - wp_dis_from_ave
        !> assign the index of the max distance as false
        mask(maxloc(wp_dis_from_ave)) = .false.
        !> compute the expectation of the array except the max distance
        wp_output(1) = sum(pack(wp_input, mask)) / (size(wp_input) - 1_ip)
        !> compute the standard deviation
        wp_output(2) = std(pack(wp_input, mask))
        
    end function expec_without_outlier

    function real2str(int) result(str)
        ! Arguments
        character(len=3000)                     :: str
        real(kind=wp), intent(in)               :: int
        
        write(str, *)  int
    end function real2str

    subroutine pyplot_demo
        real(wp),dimension(100) :: x,sx
        type(pyplot) :: plt
        integer(ip) :: i, istat
            
        !generate some data:
        x = [(real(i, wp), i=0, size(x) - 1)] / 5.0_wp
        sx = sin(x)
            
        !plot it:
        call plt%initialize(grid=.true., xlabel='angle (rad)', &
                            title='Plot of $\sin(x)$', legend=.true.)
        call plt%add_plot(x, sx, label='$\sin(x)$', linestyle='b-o', markersize=5, linewidth=2, istat=istat)
        call plt%savefig('sinx.png', pyfile='sinx.py', ismat=.true., istat=istat)
    end subroutine pyplot_demo

    pure function deg2rad(deg) result(rad)
        !> This is a function to perform the same function as numpy.deg2rad()
        !> input variable
        real(wp)        , INTENT(IN   )         :: deg
        
        !> Output variable
        real(wp)                                :: rad
        
        rad = deg * pi / 180.0_wp
    end function deg2rad
    
    pure function rad2deg(rad) result(deg)
        !> This is a function to perform the same function as numpy.rad2deg()
        !> Input variable
        real(wp)        , intent(in   )         :: rad
        
        !> Output variable
        real(wp)                                :: deg

        deg = rad / pi * 180.0_wp
    end function rad2deg

    function folder_walk_win(folder, iflag) result(paths)
        !> input variable
        CHARACTER(len=*), INTENT(IN   )         :: folder
        CHARACTER(len=*), INTENT(IN   )         :: iflag
        
        !> output variable
        CHARACTER(len=3000), ALLOCATABLE        :: paths(:)
        
        !> temporary variable
        character(len=3000)                     :: temp
        TYPE(io_file)                           :: lists_file
        type(random_type)                       :: file_type
        INTEGER(ip)                             :: ios, i, err, j
        integer(ip)                             :: npaths
        character(len=3000)                     :: iflag_temp, id_temp
        
        !> walk the specific folder
        call system("dir /b /s /a-d "//folder//" > lists.txt")
        
        !> delete "File" in the flag string
        id_temp = '_X_'
        if (index(trim(iflag), "File") /= 0) then
            if (index(trim(iflag), "-") /= 0) Then
                iflag_temp = iflag(1: index(iflag, "-") - 1)
                id_temp = "_"//iflag(index(iflag, "-") + 1: index(iflag, "File") - 1)//"_"
            else
                iflag_temp = iflag(1: index(iflag, "File") - 1)
            end if
        else
            iflag_temp = iflag
            id_temp = ''
        end if
        
        !> initialise the file object
        lists_file%name = "lists.txt"
        lists_file%unit = file_type%random_int_range()
        lists_file%nrow = lists_file%get_file_n()
        open(unit=lists_file%unit, file=trim(lists_file%name), iostat=ios, status="old", action="read")
        if ( ios /= 0 ) Then
            call logger%fatal("math_collection_module", "Error opening the folder walker list file")
            !call xml_o%xml2file(1, "Error opening the folder walker list file")
            !stop
        end if
        
        npaths = 0_ip
        read_filenames_loop1: do i = 1, lists_file%nrow, 1
            read(lists_file%unit, "(a)") temp
            if (trim(iflag_temp) /= 'all') then
                if (index(trim(temp), 'GSM') /= 0 .and. index(trim(temp), trim(id_temp)) /= 0) then
                    npaths = npaths + 1_ip
                else
                    if (index(trim(temp), trim(iflag_temp)) /= 0 .and. index(trim(temp), trim(id_temp)) /= 0 .and. index(trim(temp), '.asc') /= 0) npaths = npaths + 1_ip
                end if
            end if
        end do read_filenames_loop1
        
        if (trim(iflag_temp) == 'all') npaths = lists_file%nrow
        
        if (npaths == 0_ip) then
            call logger%fatal("math_collection_module", trim(iflag_temp)//" file(s) not appear in "//trim(folder))
            !call xml_o%xml2file(1, trim(iflag_temp)//" file(s) not appear in "//trim(folder))
            !stop trim(iflag_temp)//" file(s) not appear in the input folder"
        end if
        
        rewind(lists_file%unit)
        
        allocate(paths(npaths), stat=err)
        if (err /= 0) Then
            call logger%fatal("math_collection_module", "paths: Allocation request denied")
            stop
        end if
        
        j = 1_ip
        read_filenames_loop2: do i = 1, lists_file%nrow, 1
            read(lists_file%unit, "(a)") temp
            
            if (index(temp, "GSM") /= 0 .and. index(trim(temp), trim(id_temp)) /= 0 ) then
                paths(j) = temp
                j = j + 1_ip
            else
                if (index(trim(temp), trim(iflag_temp)) /= 0 .and. index(trim(temp), trim(id_temp)) /= 0 .and. index(trim(temp), '.asc') /= 0) Then
                    paths(j) = temp
                    j = j + 1_ip
                end if
            end if
            
            !> read all
            if (trim(iflag_temp) == 'all') paths(i) = temp
        end do read_filenames_loop2

        close(unit=lists_file%unit, iostat=ios, status='delete')
        if ( ios /= 0 ) Then
            call logger%fatal("math_collection_module", "Error closing the folder walker list file")
            stop
        end if
        
    end function folder_walk_win
    
    function project_path(ipath) result(opath)
        !> input variable
        character(len=*)                        :: ipath
    
        !> output variable
        CHARACTER(len=1000)                     :: opath
        
        !> temporary variable
        CHARACTER(len=3000)                     :: temp1
        integer(ip)                             :: last_token
        
        if (index(ipath, '/')) Then
            last_token = index(ipath, '/', .true.)
        else
            last_token = index(ipath, '\', .true.)
        end if
        
        temp1 = trim(ipath(1: last_token - 1))
        
        if (index(temp1, '/')) Then
            last_token = index(temp1, '/', .true.)
        else
            last_token = index(temp1, '\', .true.)
        end if
        
        opath = trim(temp1(1: last_token))
        
    end function project_path

    function yyyymmddhhmmss(yyyy_i, mm_i, dd_i, hh_i, mimi_i, ss_i) result(now)
        !> input variables
        INTEGER(ip), OPTIONAL, INTENT(IN   )    :: yyyy_i, mm_i, dd_i, hh_i, mimi_i, ss_i
        !> output variables
        CHARACTER(len=14)                       :: now
        
        !> reg
        CHARACTER(len=4)                        :: yyyy
        character(len=2)                        :: mm, dd, hh, mimi, ss
        integer(ip)                             :: d(8)
        
        call date_and_time(values=d)
        if (present(yyyy_i)) d(1) = yyyy_i
        if (present(mm_i)) d(2) = mm_i
        if (present(dd_i)) d(3) = dd_i
        if (present(hh_i)) d(5) = hh_i
        if (present(mimi_i)) d(6) = mimi_i
        if (present(ss_i)) d(7) = ss_i
        
        !> yyyy
        write(yyyy, '(i4)') d(1)
        !> others
        if (d(2) < 10_ip) then
            write(mm, "(a, i1)") "0", d(2)
        else
            write(mm, "(i2)") d(2)
        end if
        if (d(3) < 10_ip) then
            write(dd, "(a, i1)") "0", d(3)
        else
            write(dd, "(i2)") d(3)
        end if
        if (d(5) < 10_ip) then
            write(hh, "(a, i1)") "0", d(5)
        else
            write(hh, "(i2)") d(5)
        end if
        if (d(6) < 10_ip) then
            write(mimi, "(a, i1)") "0", d(6)
        else
            write(mimi, "(i2)") d(6)
        end if
        if (d(7) < 10_ip) then
            write(ss, "(a, i1)") "0", d(7)
        else
            write(ss, "(i2)") d(7)
        end if
        
        now = yyyy//mm//dd//hh//mimi//ss
        
    end function yyyymmddhhmmss

    pure function check_if_time_continuous(itimes, threshold) result(yes_or_no)
        !> input variables
        real(wp)        , INTENT(in   )         :: itimes(:)
        real(wp)        , INTENT(IN   )         :: threshold

        !> output variables
        logical                                 :: yes_or_no

        !> reg
        integer(ip)                             :: i

        !> function
        yes_or_no = .true.
        check_if_time_continuous_loop: do i = 1, size(itimes) - 1, 1
            if (itimes(i + 1) - itimes(i) > threshold) then
                yes_or_no = .false.
            end if
        end do check_if_time_continuous_loop
    end function check_if_time_continuous

    subroutine fir_filter(fname, iarray, fs, oarray, nfilter)
        !> input variables
        CHARACTER(len=*)   , INTENT(IN   )      :: fname
        real(wp)           , INTENT(IN   )      :: iarray(:)
        real(wp)           , INTENT(IN   )      :: fs

        !> output variables
        real(wp)                                :: oarray(size(iarray))
        integer(ip)                             :: nfilter

        !> reg
        type(io_file)                           :: coeff_file
        integer(ip)                             :: ios, err, i
        
        real(wp), ALLOCATABLE                   :: coeff(:)

        coeff_file%name = trim(fname)
        call coeff_file%file_obj_init()

        nfilter = coeff_file%nrow - coeff_file%nheader
        
        open(unit=coeff_file%unit, file=coeff_file%name, iostat=ios, status="old", action="read")
        if ( ios /= 0 ) stop "Error opening filter coefficients file"

        allocate(coeff(coeff_file%nrow - coeff_file%nheader), stat=err)
        if (err /= 0) print *, "coeff: Allocation request denied"
        
        read_coeff_header_loop: do i = 1, coeff_file%nheader, 1
            read(coeff_file%unit, *) 
        end do read_coeff_header_loop
        read_coeff_data_loop: do i = 1, coeff_file%nrow - coeff_file%nheader, 1
            read(coeff_file%unit, *) coeff(i)
        end do read_coeff_data_loop

        oarray = convolution(iarray, coeff)

        !> -----------------------------------------------------------------------------------------
        !open(unit=3244, file='..//output//eq_b_non_simu.txt', iostat=ios, action="write")
        !if ( ios /= 0 ) stop "Error opening file name"
        !do i = 1, size(oarray), 1
        !    write(3244, '(f50.30)') oarray(i)
        !end do
        !close(unit=3244, iostat=ios)
        !if ( ios /= 0 ) stop "Error closing file unit 3244"
        !> -----------------------------------------------------------------------------------------
        
        if (allocated(coeff)) deallocate(coeff, stat=err)
        if (err /= 0) print *, "coeff: Deallocation request denied"

        close(unit=coeff_file%unit, iostat=ios)
        if ( ios /= 0 ) stop "Error closing filter coefficients file"
        
    end subroutine fir_filter
    
    function lagrange_value_1d (nd, xd, yd, ni, xi) result(yi)

        !*****************************************************************************80
        !
        !! LAGRANGE_VALUE_1D evaluates the Lagrange interpolant.
        !
        !  Discussion:
        !
        !    The Lagrange interpolant L(ND,XD,YD)(X) is the unique polynomial of
        !    degree ND-1 which interpolates the points (XD(I),YD(I)) for I = 1
        !    to ND.
        !
        !    The Lagrange interpolant can be constructed from the Lagrange basis
        !    polynomials.  Given ND distinct abscissas, XD(1:ND), the I-th Lagrange 
        !    basis polynomial LB(ND,XD,I)(X) is defined as the polynomial of degree 
        !    ND - 1 which is 1 at  XD(I) and 0 at the ND - 1 other abscissas.
        !
        !    Given data values YD at each of the abscissas, the value of the
        !    Lagrange interpolant may be written as
        !
        !      L(ND,XD,YD)(X) = sum ( 1 <= I <= ND ) LB(ND,XD,I)(X) * YD(I)
        !
        !  Licensing:
        !
        !    This code is distributed under the GNU LGPL license.
        !
        !  Modified:
        !
        !    11 September 2012
        !
        !  Author:
        !
        !    John Burkardt
        !
        !  Parameters:
        !
        !    Input, integer ( kind = 4 ) ND, the number of data points.
        !    ND must be at least 1.
        !
        !    Input, real ( kind = 8 ) XD(ND), the data points.
        !
        !    Input, real ( kind = 8 ) YD(ND), the data values.
        !
        !    Input, integer ( kind = 4 ) NI, the number of interpolation points.
        !
        !    Input, real ( kind = 8 ) XI(NI), the interpolation points.
        !
        !    Output, real ( kind = 8 ) YI(NI), the interpolated values.
        !
        implicit none

        integer(ip), INTENT(IN   )                          :: nd
        integer(ip), INTENT(IN   )                          :: ni

        real(wp)                                            :: lb(ni, nd)
        real(wp)   , INTENT(IN   )                          :: xd(nd)
        real(wp)   , INTENT(IN   )                          :: yd(nd)
        real(wp)   , INTENT(IN   )                          :: xi(ni)
        real(wp)                                            :: yi(ni)

        call lagrange_basis_1d (nd, xd, ni, xi, lb)

        yi = matmul (lb, yd)

        return
    end function lagrange_value_1d

    subroutine lagrange_basis_1d (nd, xd, ni, xi, lb)

        !*****************************************************************************80
        !
        !! LAGRANGE_BASIS_1D evaluates a 1D Lagrange basis.
        !
        !  Licensing:
        !
        !    This code is distributed under the GNU LGPL license.
        !
        !  Modified:
        !
        !    09 October 2012
        !
        !  Author:
        !
        !    John Burkardt
        !
        !  Parameters:
        !
        !    Input, integer ( kind = 4 ) ND, the number of data points.
        !
        !    Input, real ( kind = 8 ) XD(ND), the interpolation nodes.
        !
        !    Input, integer ( kind = 4 ) NI, the number of evaluation points.
        !
        !    Input, real ( kind = 8 ) XI(NI), the evaluation points.
        !
        !    Output, real ( kind = 8 ) LB(NI,ND), the value, at the I-th point XI, 
        !    of the Jth basis function.
        !
        implicit none

        integer(ip), INTENT(IN   )                          :: nd
        integer(ip), INTENT(IN   )                          :: ni
        integer(ip)                                         :: i
        integer(ip)                                         :: j
        real(wp)   , INTENT(  OUT)                          :: lb(ni, nd)
        real(wp)   , INTENT(IN   )                          :: xd(nd)
        real(wp)   , INTENT(IN   )                          :: xi(ni)

        do i = 1, ni, 1
            do j = 1, nd, 1
                lb(i, j) = product ( ( xi(i) - xd(1: j - 1)  ) / ( xd(j) - xd(1: j - 1)  ) ) &
                         * product ( ( xi(i) - xd(j + 1: nd) ) / ( xd(j) - xd(j + 1: nd) ) )
            end do
        end do

        return
    end subroutine lagrange_basis_1d

    function ls_solver(a_ival, b_ival) result(x_oval)
        real(wp), INTENT(IN   )                 :: a_ival(:, :), b_ival(:)
        integer(ip)                             :: icol(size(a_ival, 1) * size(a_ival, 2))
        integer(ip)                             :: irow(size(a_ival, 1) * size(a_ival, 2))
        real(wp)                                :: a(size(a_ival, 1) * size(a_ival, 2))
        integer(ip)                             :: i, j, k, istop
        real(wp)                                :: x_oval(size(a_ival, 2))
        type(lsqr_solver_ez)                    :: solver

        k = 1_ip
        assign_a_loop: do i = 1, size(a_ival, 1), 1
            do j = 1, size(a_ival, 2), 1
                icol(k) = j
                irow(k) = i
                a(k) = a_ival(i, j)
                k = k + 1_ip
            end do
        end do assign_a_loop

        call solver%initialize(size(a_ival, 1), size(a_ival, 2), a, irow, icol)
        call solver%solve(b_ival, 0.0_wp, x_oval, istop)

    end function ls_solver

    pure function isequal(num1, num2) result(flag)
        real(wp), intent(in   )                     :: num1, num2
        logical                                     :: flag

        flag = .false.
        if (num1 - num2 < 1.0e-5) flag = .true.
    end function isequal

    function q2m(quaternion) result(rotation_matrix)
        real(wp), INTENT(IN   )                     :: quaternion(4)
        real(wp)                                    :: rotation_matrix(3, 3)

        !> check if the norm2 of the quaternion is 1.0
        if (.not. isequal(norm2(quaternion), 1.0_wp)) then
            call logger%error('math_collection_module', 'Error with quaternion')
            stop
        end if

        ASSOCIATE(a => quaternion(1), &
                  b => quaternion(2), &
                  c => quaternion(3), &
                  d => quaternion(4))
            rotation_matrix(1, 1) = a**2 + b**2 - c**2 - d**2
            rotation_matrix(1, 2) = 2.0_wp * (b * c + a * d)
            rotation_matrix(1, 3) = 2.0_wp * (b * d - c * a)
            rotation_matrix(2, 1) = 2.0_wp * (b * c - d * a)
            rotation_matrix(2, 2) = a**2 + c**2 - b**2 - d**2
            rotation_matrix(2, 3) = 2.0_wp * (c * d + a * b)
            rotation_matrix(3, 1) = 2.0_wp * (b * d + c * a)
            rotation_matrix(3, 2) = 2.0_wp * (c * d - a * b)
            rotation_matrix(3, 3) = a**2 + d**2 - b**2 - c**2
        end associate 
    end function q2m

    function linear_interp(array_1, array_2, point) result(interped)
        implicit None

        real(wp)            , INTENT(IN   )             :: array_1(:)
        real(wp)            , intent(in   )             :: array_2(:)
        real(wp)            , INTENT(IN   )             :: point
        real(wp)                                        :: interped
        integer(ip)                                     :: i

        interped = 0.0_wp
        interpolation_loop: do i = 1, size(array_1) - 1
            if (point > array_1(i) .and. point < array_1(i + 1)) then
                interped = array_2(i + 1) + (array_2(i + 1) - array_2(i)) / &
                            (array_1(i + 1) - array_1(i)) * (point - array_1(i + 1))
            else if (isequal(point, array_1(i))) then
                interped = array_2(i)
            end if
        end do interpolation_loop

    end function linear_interp

    function convolution(x, h) result(convolve)
        !x is the signal array
        !h is the noise/impulse array
        real(wp), dimension(:), INTENT(IN   )           :: x, h
        real(wp)                                        :: convolve(size(x)), y(size(x))
        integer(ip)                                     :: kernelsize, datasize
        integer(ip)                                     :: i, j, k
        
        datasize = size(x)
        kernelsize = size(h)

        !last part
        do i = kernelsize, datasize, 1
            y(i) = 0.0_wp
            j = i
            do k = 1, kernelsize, 1
                y(i) = y(i) + x(j) * h(k)
                j = j - 1
            end do
        end do
        
        !first part
        do i = 1, kernelsize, 1
            y(i) = 0.0_wp
            j = i
            k = 1
            do while (j > 0)
                y(i) = y(i) + x(j) * h(k)
                j = j - 1
                k = k + 1
            end do
        end do
        
        convolve = y
    end function convolution
    function arange(xa, xb, dx)
        !..............................................................................
        !   returns a vector in the form of [xa, xa+dx, xa+2*dx, ...]
        !   the number of elements is calculated as m = n+ 1,
        !   where n= int ( (xa-xb)/dx) ).
        !   arange is similar to colon in Matlab and arange in Python!
        !
        !   NOTE:
        !    - If n calculated as zero, result is [xa]
        !    - If n calculated as Inf (dx=0), a fatal error will be raised
        !    - If n calculated as negative value (e.g xa<xb or dx<0.0), a
        !      fatal error will be raised
        !..............................................................................

        real(wp), intent(in)            :: xa
        real(wp), intent(in)            :: xb
        real(wp), intent(in), optional  :: dx
        real(wp), allocatable           :: arange(:)

        !   Local vars
        real(wp):: dxl
        integer:: i
        integer:: n
        integer:: ierr

        ! check the presence of dx and its correctness
        if (present(dx)) then
            dxl = dx
            if ( abs(dx) <= tiny(dx)) then
                print*, "arange procedure: Fatal Error: wrong dx, use a dx > 0.0 "
                stop
            end if
        else
            dxl = 1.0_wp
        end if

        if ( (xa < xb) .and. (dx < 0.0_wp) ) then
            print*, "arange procedure: Fatal Error: wrong dx, use a dx > 0.0 "
            stop
        end if

        n = int( (xb-xa)/ dxl) ! n+1 is the number of elements

        allocate(arange(n), stat=ierr)

        if (ierr /= 0) then
            print*, "arange procedure: Fatal Error, allocation failed in arange function"
            stop
        end if

        arange = [(xa + i*dxl, i=0, n)]

    end function arange

end module math_collection_module