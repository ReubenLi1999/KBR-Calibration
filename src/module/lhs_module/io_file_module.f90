module io_file_module
    use num_kinds_module
    use random_integer_module
    use logger_mod, only: logger_init, logger => master_logger
    use stringifor
    implicit none
    
    type, public:: io_file
        CHARACTER(len = 3000)                   :: name
        integer(ip)                             :: unit
        INTEGER(ip)                             :: nrow
        integer(ip)                             :: nheader
    contains
        procedure, NON_OVERRIDABLE, PUBLIC      :: get_file_n           => lhs_get_file_n
        procedure, NON_OVERRIDABLE, PUBLIC      :: file_obj_init        => lhs_file_obj_init
        procedure, NON_OVERRIDABLE, public      :: get_header_n         => lhs_get_header_n
    end type io_file

contains

    function lhs_get_header_n(self) result(nheaders)
        class(io_file), intent(in   )                   :: self
        integer(ip)                                     :: nheaders
        integer(ip)                                     :: nendofheader
        integer(ip)                                     :: ios, reason
        character(len=100)                              :: temp
        type(string)                                    :: astring

        open(unit=self%unit, file=self%name, iostat=ios, status="old", action="read")
        if ( ios /= 0 ) then
            call logger%fatal('io_file_module', 'Error opening '//trim(self%name))
            stop
        end if

        nheaders = 0_ip; nendofheader = 0_ip
        count_header: do 
            read(self%unit, *, iostat=reason) temp
            astring = temp
            if (reason < 0) exit
            if (astring%is_number()) then
                nendofheader = nendofheader + 1_ip
                exit
            end if
            nheaders = nheaders + 1_ip
        end do count_header

        !> check if this file contains the standard header end
        if (nendofheader == 0_ip .and. index(self%name, '.xml') == 0_ip) then
            call logger%fatal('io_file_module', 'The end of the header of '//trim(self%name)//' is not standard')
            stop
        end if

        close(unit=self%unit, iostat=ios)
        if ( ios /= 0 ) stop "Error closing file"
        
    end function lhs_get_header_n

    subroutine lhs_file_obj_init(self)
        class(io_file), intent(inout)                   :: self
        type(random_type)                               :: file_unit

        self%unit = file_unit%random_int_range()
        self%nrow = self%get_file_n()
        self%nheader = self%get_header_n()
    end subroutine lhs_file_obj_init

    function lhs_get_file_n(me) result(nrows)
        !> from www.fcode.cn
        implicit none
        CLASS(io_file), intent(in   )                   :: me
        character(len = 1)                              :: cDummy
        integer(ip)                                     :: ierr, ios
        integer(ip)                                     :: nrows

        open(unit=me%unit, file=me%name, iostat=ios, status="unknown", action="read")
        if (ios /= 0) then
            call logger%fatal('io_file_module', 'Error opening'//trim(me%name))
            stop
        end if
        nrows = 0
        do
            read(me%unit, *, ioStat=ierr) cDummy
            if(ierr /= 0) exit
            nrows = nrows + 1
        end do

        close(unit=me%unit, iostat=ios)
        if ( ios /= 0 ) stop "Error closing file"
    end function lhs_get_file_n
    
end module io_file_module