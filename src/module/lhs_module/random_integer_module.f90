module random_integer_module
!>--------------------------------------------------------------------------------------------------
!@      Arthur          => Hao-si Li
!@      Date            => 2021-01-30
!@      Revision        => 1.0      :       210130 for first writing
!>
!>      Description:
!>              This module packages the class for generating a random integer number differing from
!>              ones ever generated.
!>--------------------------------------------------------------------------------------------------
    use num_kinds_module
    implicit none

    type, public :: random_type
        integer(ip)                                     :: random_int

        integer(ip)                                     :: random_int_array(1000) = 0_ip

        integer(ip)                                     :: random_int_range_num_called = 0_ip
    contains
        procedure, NON_OVERRIDABLE, public              :: random_int_range         => random_int_range
    end type random_type

contains
    function random_int_range(self, lbound_ival, ubound_ival) result(oval)
    !>----------------------------------------------------------------------------------------------
    !@      Arthur      => Hao-si Li
    !@      Date        => 2021-01-30
    !@      Revision    => 1.0      :       2021-01-30 for writing the subroutine
    !>
    !>      Description:
    !>              This function is designed to generate a random integer number between
    !>              the range [lbound, ubound], but usually, this procedure is used in the process 
    !>              of generating a file unit for opening a file.
    !>
    !>      Input arguments:
    !>              lbound: integer, optional, indicating the lower bound
    !>              ubound: integer, optional, indicating the upper bound
    !>
    !>      Output arguments
    !>              oval: the random integer number differing from ones ever generated
    !>
    !>      Methodology:
    !>              Using basic genric subroutine in fortran called random_number
    !>----------------------------------------------------------------------------------------------
        class(random_type)   , INTENT(inout)            :: self
        integer(ip), OPTIONAL, intent(in   )            :: ubound_ival, lbound_ival
        integer(ip)                                     :: ubound, lbound
        integer(ip)                                     :: oval
        integer(ip)                                     :: err

        ! temporary variable
        real(wp)                                        :: temp

        if (present(ubound_ival)) then
            ubound = ubound_ival
        else
            ubound = 1000_ip
        end if
        if (present(lbound_ival)) then
            lbound = lbound_ival
        else
            lbound = 30_ip
        end if

        self%random_int_range_num_called = self%random_int_range_num_called + 1
        call random_seed()
        random_int_gene: do
            
            call random_number(temp)
            self%random_int = lbound + floor(temp * (ubound + 1 - lbound))

            if (.not. any(self%random_int_array == self%random_int)) then
                oval = self%random_int
                self%random_int_array(self%random_int_range_num_called) = oval
                exit
            end if

        end do random_int_gene

    end function random_int_range

end module random_integer_module