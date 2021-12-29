module xml_module
    use io_file_module
    use num_kinds_module
    use fpl
    use logger_mod, only: logger_init, logger => master_logger
    use math_collection_module
    use strings
    ! use M_strings, only: split
    use CalendarModule, only: dayOfYear
    use datetime_wrapper
    
    implicit none
    type, extends(io_file), public:: xml_file
        type(parameterlist_t)                                   :: urlpaths
    contains
        procedure, NON_OVERRIDABLE, PUBLIC                      :: read_xml           => lhs_read_xml
        procedure, NON_OVERRIDABLE, PUBLIC                      :: free_xml           => lhs_free_xml
        procedure, NON_OVERRIDABLE, PUBLIC                      :: xml2file           => xml2file
        procedure, NON_OVERRIDABLE, PUBLIC                      :: output_path        => output_path
    end type xml_file
    
    type(xml_file)                                              :: xml_i, xml_o

    PRIVATE                                                     :: content4tag, flag4tag
    
contains

    pure function flag4tag(c_input) result(c_output)
        character(len=*)                , INTENT(IN   )         :: c_input
        !> output
        CHARACTER(len=3000)                                     :: c_output

        !> local
        integer(kind=ip), DIMENSION(2)                          :: left, right

        !> the index of "<" and ">"
        left(1) = index( c_input, "<")
        left(2) = index( c_input(left(1) + 1: 3000), "<") + left(1)
        right(1) = index(c_input, ">")
        right(2) = index(c_input(right(1) + 1: 3000), ">") + right(1)
        !> start time
        c_output = c_input(left(1) + 1_ip: right(1) - 1_ip)
    end function flag4tag

    pure function content4tag(c_input) result(c_output)
        character(len=*)                , INTENT(IN   )         :: c_input
        !> output
        CHARACTER(len=3000)                                     :: c_output

        !> local
        integer(kind=ip), DIMENSION(2)                          :: left, right

        !> the index of "<" and ">"
        left(1) = index( c_input, "<")
        left(2) = index( c_input(left(1) + 1: 3000), "<") + left(1)
        right(1) = index(c_input, ">")
        right(2) = index(c_input(right(1) + 1: 3000), ">") + right(1)
        !> start time
        c_output = c_input(right(1) + 1: left(2) - 1)
    end function content4tag

    subroutine xml2file(self, ReturnCode, errorinfo)
        class(xml_file)                 , INTENT(INOUT)         :: self
        INTEGER(ip)                     , INTENT(IN   )         :: returncode
        character(len=*), optional      , intent(in   )         :: errorinfo
        
        !> reg
        INTEGER(ip)                                             :: ios, fplerror, path_length
        CHARACTER(len=1)                                        :: returncode_char
        character(len=14)                                       :: datenow
        CHARACTER(len=3000)                                     :: dir, path
        CHARACTER(len=3000)                                     :: result_path(2)
        character(len=5)                                        :: path_length_char
        
        !> output path
        fplerror = self%urlpaths%get(key='VKB', value=result_path)
        path = result_path(1)
        path_length = len_trim(path)
        write(path_length_char, '(i5)') path_length
        !> datetime
        datenow = yyyymmddhhmmss()
        !> current directory
        if (index(path, "/") .ne. 0) then
            dir = path(1: index(path, "/", .true.))
        else
            dir = path(1: index(path, "\", .true.))
        end if
        
        write(returncode_char, '(i1)') returncode
        !> assign the unit
        call self%file_obj_init()
        !> open
        open(unit=self%unit, file=self%name, iostat=ios, status="unknown", action="write")
        if ( ios /= 0 ) then
            call logger%fatal("xml_module", "Error opening "//trim(self%name))
            stop
        end if
        
        write(self%unit, "(a)") '<?xml version="1.0" encoding="utf-8" ?>'
        write(self%unit, "(a)") "<root>"
        write(self%unit, "(a)") "<data>"
        write(self%unit, "(4x, a)") "<ReturnCode>"//trim(returncode_char)//"</ReturnCode>"
        if (returncode_char == '0') then
            write(self%unit, "(4x, a)") "<ReturnAnalyse>process is successed!</ReturnAnalyse>"
        else 
            write(self%unit, "(4x, a)") "<ReturnAnalyse>"//trim(errorinfo)//"</ReturnAnalyse>"
        end if
        write(self%unit, "(4x, a)") "<FolderPath>"//trim(dir)//"</FolderPath>"
        write(self%unit, "(4x, a)") "<FileList>"
        write(self%unit, "(8x, a)") "<FileInfo>"
        write(self%unit, "(12x, a)") "<FilePath>"//trim(dir)//"</FilePath>"
        write(self%unit, "(12x, a)") "<FileName>"//trim(result_path(1))//','//trim(result_path(2))//"</FileName>"
        write(self%unit, "(12x, a)") "<FileLength>"//trim(path_length_char)//"</FileLength>"
        write(self%unit, "(8x, a)") "</FileInfo>"
        write(self%unit, "(4x, a)") "</FileList>"
        
        write(self%unit, "(a)") "</data>"
        write(self%unit, "(a)") "</root>"
        
        
        !> close file
        close(unit=self%unit, iostat=ios)
        if ( ios /= 0 ) then
            call logger%fatal("xml_module", "Error closing "//trim(self%name))
            stop
        end if

    end subroutine xml2file

    subroutine lhs_free_xml(self)
        class(xml_file), intent(inout)                          :: self

        call self%urlpaths%free()
    
    end subroutine lhs_free_xml
    
    function output_path(self) result(outputpath)
        !> class variable
        class(xml_file) , intent(inout)                         :: self
        
        !> output variable
        character(len=3000)                                     :: outputpath
        
        !> reg
        INTEGER(ip)                                             :: ios, reason, fplerror
        integer(ip)                                             :: i, num_path, err
        integer(ip)                                             :: left(2), right(2)
        character(len=3000)                                     :: temp1, temp2, temp3, flag
        
        
        ! assign unit
        call self%file_obj_init()
        open(unit=self%unit, file=self%name, iostat=ios, status="old", action="read")
        if ( ios /= 0 ) then
            call logger%error('read xml file', 'Error opening file '//trim(self%name))
            stop 
        end if

        read(self%unit, *, iostat=reason) temp1
        read(self%unit, *, iostat=reason) temp1
        read_in_outputpath: do
            read(self%unit, '(a)', iostat=reason) temp1
            if (reason < 0) exit
            !> OutputFileList
            if (index(temp1, "<OutputPath>")) then
                read_in_out0: do   
                    read(self%unit, "(a)") temp1
                    if (index(temp1, "</OutputPath>")) exit
                    !> the index of "<" and ">"
                    left(1) = index(temp1, "<")
                    left(2) = index(temp1(left(1) + 1: 3000), "<") + left(1)
                    right(1) = index(temp1, ">")
                    right(2) = index(temp1(right(1) + 1: 3000), ">") + right(1)
                    !> product flag
                    flag = temp1(left(1) + 1_ip: right(1) - 1_ip)
                    !> number of the paths
                    num_path = str_count(temp1, ",") + 1
                    
                    outputpath = temp1(right(1) + 1: left(2) - 1)
                end do read_in_out0
            end if
        end do read_in_outputpath
        
        !> mkdir ./Result & ./log & ./temp
        call system('mkdir '//trim(outputpath)//'Result')
        call system('mkdir '//trim(outputpath)//'log')
        call system('mkdir '//trim(outputpath)//'temp')

        close(unit=self%unit, iostat=ios)
        if ( ios /= 0 ) then
            call logger%error('read xml file', 'Error closing file parameters.xml')
            stop 
        end if
        
    end function output_path

    subroutine lhs_read_xml(self, date, version, i_maneuver_time, i_mirror)
        CLASS(xml_file) , INTENT(INOUT)                         :: self
        INTEGER(kind=ip)                                        :: ios, reason, fplerror
        integer(kind=ip)                                        :: i, num_path, err, k
        integer(kind=ip)                                        :: left(2), right(2)
        integer(kind=ip)                                        :: i_gsm_dayofyear
        integer(kind=ip)                                        :: i_date_dayofyear
        integer(kind=ip)                                        :: i_diff_date_gsm
        integer(kind=ip)                                        :: i_index_gsm
        integer(kind=ip)                                        :: i_date_d, i_date_m, i_date_y
        integer(kind=ip)                                        :: i_start_epoch, i_duration
        integer(kind=ip)                                        :: i_date_num(6)
        integer(kind=ip)                                        :: i_days, j
        integer(kind=ip)                                        :: i_count_inputinfo
        integer(kind=ip)                                        :: i_count_sca1b_a, i_count_sca1b_b, i_count_tha1b_a, i_count_tha1b_b, &
                                                                   i_count_roi1b_a, i_count_roi1b_b, i_count_koe1b_a, i_count_koe1b_b, &
                                                                   i_count_acc1b_a, i_count_acc1b_b, i_count_kbr1b_x
        integer(kind=ip), INTENT(  OUT)                         :: i_maneuver_time(8), i_mirror
        character(len=3000)                                     :: temp1, temp2, temp3, flag, c_date_man
        character(len=3000)                                     :: c_temp2, c_temp3, c_start_epoch, c_duration
        character(len=3000)                                     :: c_vkbrptfilename(2), paths
        CHARACTER(len=:)   , ALLOCATABLE                        :: path(:)
        character(len=3000), ALLOCATABLE                        :: c_path(:), c_inputfiles(:)
        CHARACTER(len=*), intent(  out)                         :: date, version
        character(len=3000)                                     :: result_path, vkbfile_name(2), config_path
        character(len=14)                                       :: datenow
        character(len=1)                                        :: sep
        character(len=3000)                                     :: config_names(27)
        character(len=3000)                                     :: gsm_file
        character(len=7)                                        :: c_gsm_dayofyear
        character(len=2)                                        :: c_date_d, c_date_m
        character(len=4)                                        :: c_date_y
        CHARACTER(len=1)                                        :: c_mirror !< character indicating mirror maneuver
        CHARACTER(len=3000), dimension(4, 1)                    :: c_kbr1b_x, c_acc1b_a, c_acc1b_b, &
                                                                   c_sca1b_a, c_sca1b_b, c_roi1b_a, &
                                                                   c_roi1b_b, c_koe1b_a, c_koe1b_b, &
                                                                   c_tha1b_a, c_tha1b_b, c_date
        character(len=3000), dimension(1)                       :: c_vac1b_a, c_vac1b_b, c_gkb1b_a, c_gkb1b_b
        character(len=3000), DIMENSION(8)                       :: c_date1

        !> datetime
        type(datetime_type)                                     :: start_epoch, man1, man2, man3, man4
        type(timedelta_type)                                    :: delta_man1, delta_man2, delta_man3, delta_man4

        !> assign the start epoch for cumulating second
        start_epoch = create_datetime(2009, 1, 1, 0, 0, 0)

        !> filenames that must appear in the config folder
        config_names = [ &
            '2001JIYI.txt', '2002JIYI.txt', '2003JIYI.txt', '2004JIYI.txt', '2005JIYI.txt', &
            '2006JIYI.txt', '2007JIYI.txt', '2008JIYI.txt', '2009JIYI.txt', '2010JIYI.txt', &
            '2011JIYI.txt', '2012JIYI.txt', '2013JIYI.txt', '2014JIYI.txt', '2015JIYI.txt', &
            '2016JIYI.txt', '2017JIYI.txt', '2018JIYI.txt', '2019JIYI.txt', '2020JIYI.txt', &
            "coeff_high_pass_0.004_100db.fcf", "jiyi.dat", "JPLEPH", "k20.txt", "k21.txt", "k22.txt", &
            'coeff_band_pass_0.001_0.009_ls.fcf' &
        ]

        !> datetime
        datenow = yyyymmddhhmmss()
        
        call xml_o%urlpaths%init()
        fplerror = xml_o%urlpaths%set("VKB", "null")
        
        !> dictionary initiation
        call self%urlpaths%init()

        ! assign unit
        call self%file_obj_init()
        open(unit=self%unit, file=self%name, iostat=ios, status="old", action="read")
        if ( ios /= 0 ) then
            call logger%error('read xml file', 'Error opening file'//trim(self%name))
            call xml_o%xml2file(1, "Error opening file "//trim(self%name))
            stop 
        end if

        read(self%unit, *, iostat=reason) temp1
        read(self%unit, *, iostat=reason) temp1
        read_in_xml: do
            read(self%unit, '(a)', iostat=reason) temp1
            
            !> skip the blank line
            if (index(temp1, '>') == 0) cycle
            !> convert tab to null
            if (index(temp1, achar(9)) /= 0) then
                temp1 = trim(temp1(index(temp1, achar(9), back=.true.) + 1: len_trim(temp1)))
            end if
            if (reason < 0) exit
            !> -------------------------------------------------------------------------------------
            !> Maneuverstarttime
            if (index(temp1, "<ManeuverStartTime>") /= 0) then
                !> the index of "<" and ">"
                left(1) = index(temp1, "<")
                left(2) = index(temp1(left(1) + 1: 3000), "<") + left(1)
                right(1) = index(temp1, ">")
                right(2) = index(temp1(right(1) + 1: 3000), ">") + right(1)
                !> start time
                call split(temp1(right(1) + 1: left(2) - 1), path, delimiters=';', order='sequential', nulls='ignore')
                !> make sure the dimension of maneuver start time is 4
                if (size(path) /= 4_ip) error stop "The dimension of tag <ManneuverStartTime> is not four"
                !> start time from string to num
                split_start_time_loop: do i = 1, 4, 1
                    call date2num(path(i), i_date_num)
                    man1 = create_datetime(i_date_num(1), i_date_num(2), i_date_num(3), i_date_num(4), &
                                       i_date_num(5), i_date_num(6))
                    delta_man1 = man1 - start_epoch
                    i_start_epoch = delta_man1%total_seconds()
                    select case (i)
                        case (1)
                            i_maneuver_time(3) = i_start_epoch + 18_ip - 28800_ip
                        case (2)
                            i_maneuver_time(7) = i_start_epoch + 18_ip - 28800_ip
                        case (3)
                            i_maneuver_time(1) = i_start_epoch + 18_ip - 28800_ip
                        case (4)
                            i_maneuver_time(5) = i_start_epoch + 18_ip - 28800_ip
                        case default
                            error stop "The dimension of tag <ManneuverStartTime> is not four"
                    end select
                end do split_start_time_loop
            end if

            if (index(temp1, "<ManeuverTimeLength>") /= 0) then
                !> the index of "<" and ">"
                left(1) = index(temp1, "<")
                left(2) = index(temp1(left(1) + 1: 3000), "<") + left(1)
                right(1) = index(temp1, ">")
                right(2) = index(temp1(right(1) + 1: 3000), ">") + right(1)
                !> duration
                call split(temp1(right(1) + 1: left(2) - 1), path, delimiters=';', order='sequential', nulls='ignore')
                !> make sure the dimension of maneuver start time is 4
                if (size(path) /= 4_ip) error stop "The dimension of tag <ManneuverTimeLength> is not four"
                !> start time from string to num
                split_duration_loop: do i = 1, 4, 1
                    call str2int(path(i), i_duration, err)
                    select case (i)
                        case (1)
                            i_maneuver_time(4) = i_duration + i_maneuver_time(3) - 1_ip
                        case (2)
                            i_maneuver_time(8) = i_duration + i_maneuver_time(7) - 1_ip
                        case (3)
                            i_maneuver_time(2) = i_duration + i_maneuver_time(1) - 1_ip
                        case (4)
                            i_maneuver_time(6) = i_duration + i_maneuver_time(5) - 1_ip
                        case default
                            error stop "The dimension of tag <ManneuverTimeLength> is not four"
                    end select
                end do split_duration_loop
            end if

            !> config path
            if (index(temp1, "<ConfigPath>") /= 0) then
                !> the index of "<" and ">"
                left(1) = index(temp1, "<")
                left(2) = index(temp1(left(1) + 1: 3000), "<") + left(1)
                right(1) = index(temp1, ">")
                right(2) = index(temp1(right(1) + 1: 3000), ">") + right(1)
                !> config path
                if (index(temp1, "/")) then
                    config_path = temp1(right(1) + 1: left(2) - 1)//"0VKB/config/"
                else
                    config_path = temp1(right(1) + 1: left(2) - 1)//"0VKB\config\"
                end if
                check_config_path_loop: do i = 1, 26, 1
                    paths = folder_walk_win(config_path, config_names(i))
                end do check_config_path_loop
                !> path array
                fplerror = self%urlpaths%set(key='ConfigPath', value=[config_path])
            end if

            !> mirror
            if (index(temp1, "<MirrorManeuver>") /= 0) then
                !> the index of "<" and ">"
                left(1) = index(temp1, "<")
                left(2) = index(temp1(left(1) + 1: 3000), "<") + left(1)
                right(1) = index(temp1, ">")
                right(2) = index(temp1(right(1) + 1: 3000), ">") + right(1)
                !> mirror
                c_mirror = temp1(right(1) + 1: left(2) - 1)
                call str2int(c_mirror, i_mirror, err)
                if (err /= 0) error stop "The mirror maneuver flag error"
            end if

            !> version
            if (index(temp1, "<Ver>") /= 0) then
                !> the index of "<" and ">"
                left(1) = index(temp1, "<")
                left(2) = index(temp1(left(1) + 1: 3000), "<") + left(1)
                right(1) = index(temp1, ">")
                right(2) = index(temp1(right(1) + 1: 3000), ">") + right(1)
                !> version
                version = temp1(right(1) + 1: left(2) - 1)
            end if

            !> date
            if (index(temp1, "<CalibrationDataTime>")) then
                !> the index of "<" and ">"
                left(1) = index(temp1, "<")
                left(2) = index(temp1(left(1) + 1: 3000), "<") + left(1)
                right(1) = index(temp1, ">")
                right(2) = index(temp1(right(1) + 1: 3000), ">") + right(1)
                !> product flag
                date = temp1(right(1) + 1: left(2) - 1)
                !> day of year
                c_date_y = date(1: 4)
                read(c_date_y, "(i)") i_date_y
                c_date_m = date(6: 7)
                read(c_date_m, "(i)") i_date_m
                c_date_d = date(9: 10)
                read(c_date_d, "(i)") i_date_d
                i_date_dayofyear = dayofyear(i_date_d, i_date_m, i_date_y) + i_date_y * 1000_ip
            end if

            !> InputFileInfoList
            if (index(temp1, "<InputFileInfoList>")) then
                i_count_inputinfo = 0_ip
                i_count_acc1b_a = 0_ip
                i_count_acc1b_b = 0_ip
                i_count_koe1b_a = 0_ip
                i_count_koe1b_b = 0_ip
                i_count_roi1b_a = 0_ip
                i_count_roi1b_b = 0_ip
                i_count_tha1b_a = 0_ip
                i_count_tha1b_b = 0_ip
                i_count_sca1b_a = 0_ip
                i_count_sca1b_b = 0_ip
                i_count_kbr1b_x = 0_ip
                read_input_flag_path: do
                    read(self%unit, "(a)") temp1
                    !> skip the blank line
                    if (index(temp1, '>') == 0) cycle
                    !> convert tab to null
                    if (index(temp1, achar(9)) /= 0) then
                        temp1 = trim(temp1(index(temp1, achar(9), back=.true.) + 1: len_trim(temp1)))
                    end if
                    if (index(temp1, "</InputFileInfoList>")) exit
                    if (index(temp1, "<InputFileInfo>") /= 0) then
                        i_count_inputinfo = i_count_inputinfo + 1_ip
                        if (i_count_inputinfo > 4_ip) error stop "The dimension of tag <InputFileInfo> is greater than 4"
                    end if
                    !> flag for the tag
                    flag = flag4tag(temp1)
                    select case (flag)
                        case ('ACC1B-AFile')
                            paths = folder_walk_win(content4tag(temp1), flag)
                            c_acc1b_a(i_count_inputinfo, 1) = paths
                            i_count_acc1b_a = i_count_acc1b_a + 1_ip
                            if (i_count_inputinfo /= i_count_acc1b_a) error stop "Error dimension of tag <ACC1B-A>"
                        case ('ACC1B-BFile')
                            paths = folder_walk_win(content4tag(temp1), flag)
                            c_acc1b_b(i_count_inputinfo, 1) = paths
                            i_count_acc1b_b = i_count_acc1b_b + 1_ip
                            if (i_count_inputinfo /= i_count_acc1b_b) error stop "Error dimension of tag <ACC1B-B>"
                        case ('KOE1B-AFile')
                            paths = folder_walk_win(content4tag(temp1), flag)
                            c_koe1b_a(i_count_inputinfo, 1) = paths
                            i_count_koe1b_a = i_count_koe1b_a + 1_ip
                            if (i_count_inputinfo /= i_count_koe1b_a) error stop "Error dimension of tag <KOE1B-A>"
                        case ('KOE1B-BFile')
                            paths = folder_walk_win(content4tag(temp1), flag)
                            c_koe1b_b(i_count_inputinfo, 1) = paths
                            i_count_koe1b_b = i_count_koe1b_b + 1_ip
                            if (i_count_inputinfo /= i_count_koe1b_b) error stop "Error dimension of tag <KOE1B-B>"
                        case ('ROI1B-AFile')
                            paths = folder_walk_win(content4tag(temp1), flag)
                            c_roi1b_a(i_count_inputinfo, 1) = paths
                            i_count_roi1b_a = i_count_roi1b_a + 1_ip
                            if (i_count_inputinfo /= i_count_roi1b_a) error stop "Error dimension of tag <ROI1B-A>"
                        case ('ROI1B-BFile')
                            paths = folder_walk_win(content4tag(temp1), flag)
                            c_roi1b_b(i_count_inputinfo, 1) = paths
                            i_count_roi1b_b= i_count_roi1b_b + 1_ip
                            if (i_count_inputinfo /= i_count_roi1b_b) error stop "Error dimension of tag <ROI1B-B>"
                        case ('THA1B-AFile')
                            paths = folder_walk_win(content4tag(temp1), flag)
                            c_tha1b_a(i_count_inputinfo, 1) = paths
                            i_count_tha1b_a = i_count_tha1b_a + 1_ip
                            if (i_count_inputinfo /= i_count_tha1b_a) error stop "Error dimension of tag <THA1B-A>"
                        case ('THA1B-BFile')
                            paths = folder_walk_win(content4tag(temp1), flag)
                            c_tha1b_b(i_count_inputinfo, 1) = paths
                            i_count_tha1b_b = i_count_tha1b_b + 1_ip
                            if (i_count_inputinfo /= i_count_tha1b_b) error stop "Error dimension of tag <THA1B-B>"
                        case ('SCA1B-AFile')
                            paths = folder_walk_win(content4tag(temp1), flag)
                            c_sca1b_a(i_count_inputinfo, 1) = paths
                            i_count_sca1b_a = i_count_sca1b_a + 1_ip
                            if (i_count_inputinfo /= i_count_sca1b_a) error stop "Error dimension of tag <SCA1B-A>"
                        case ('SCA1B-BFile')
                            paths = folder_walk_win(content4tag(temp1), flag)
                            c_sca1b_b(i_count_inputinfo, 1) = paths
                            i_count_sca1b_b = i_count_sca1b_b + 1_ip
                            if (i_count_inputinfo /= i_count_sca1b_b) error stop "Error dimension of tag <SCA1B-B>"
                        case ('KBR1BFile')
                            paths = folder_walk_win(content4tag(temp1), flag)
                            c_kbr1b_x(i_count_inputinfo, 1) = paths
                            i_count_kbr1b_x = i_count_kbr1b_x + 1_ip
                            if (i_count_inputinfo /= i_count_kbr1b_x) error stop "Error dimension of tag <KBR1B-X>"
                        case ('GKB1B-AFile')
                            paths = folder_walk_win(content4tag(temp1), flag)
                            c_gkb1b_a(1) = paths
                        case ('GKB1B-BFile')
                            paths = folder_walk_win(content4tag(temp1), flag)
                            c_gkb1b_b(1) = paths
                        case ('VAC1B-AFile')
                            paths = folder_walk_win(content4tag(temp1), flag)
                            c_vac1b_a(1) = paths
                        case ('VAC1B-BFile')
                            paths = folder_walk_win(content4tag(temp1), flag)
                            c_vac1b_b(1) = paths
                        case ('/InputFileInfo')
                        case ('InputFileInfo')
                        case default
                            error stop "Unknown flag encountered"
                    end select
                end do read_input_flag_path
            end if

            !> ExternalFileList
            if (index(temp1, "<ExternalFileList>")) then
                read_in_ex: do
                    read(self%unit, "(a)") temp1
                    !> skip the blank line
                    if (index(temp1, '>') == 0) cycle
                    !> convert tab to null
                    if (index(temp1, achar(9)) /= 0) then
                        temp1 = trim(temp1(index(temp1, achar(9), back=.true.) + 1: len_trim(temp1)))
                    end if
                    if (index(temp1, "</ExternalFileList>")) exit
                    !> the index of "<" and ">"
                    left(1) = index(temp1, "<")
                    left(2) = index(temp1(left(1) + 1: 3000), "<") + left(1)
                    right(1) = index(temp1, ">")
                    right(2) = index(temp1(right(1) + 1: 3000), ">") + right(1)
                    !> product flag
                    flag = temp1(left(1) + 1_ip: right(1) - 1_ip)
                    
                    !> folder walk
                    paths = folder_walk_win(temp1(right(1) + 1: left(2) - 1), flag)
                    !> extract the proper month for gsm file
                    ! i_diff_date_gsm = 1000_ip
                    ! i_index_gsm = 1_ip
                    ! if (index(flag, "GSM") /= 0) then
                    !     ex_proper_gsm: do i = 1_ip, size(paths), 1_ip
                    !         c_gsm_dayofyear = paths(i)(index(paths(i), 'GSM', .true.) + 6: index(paths(i), "GSM", .true.) + 13)
                    !         read(c_gsm_dayofyear, "(i7)") i_gsm_dayofyear
                    !         if (abs(i_gsm_dayofyear - i_date_dayofyear) < i_diff_date_gsm) then
                    !             i_index_gsm = i
                    !             i_diff_date_gsm = abs(i_gsm_dayofyear - i_date_dayofyear)
                    !         end if
                    !     end do ex_proper_gsm
                    ! end if
                    !> path array
                    fplerror = self%urlpaths%set(key=trim(flag), value=[paths])
                end do read_in_ex
            end if
            !> OutputFileList
            if (index(temp1, "<OutputDir>")) then
                !> the index of "<" and ">"
                left(1) = index(temp1, "<")
                left(2) = index(temp1(left(1) + 1: 3000), "<") + left(1)
                right(1) = index(temp1, ">")
                right(2) = index(temp1(right(1) + 1: 3000), ">") + right(1)
                !> product flag
                flag = temp1(left(1) + 1_ip: right(1) - 1_ip)
                !> number of the paths
                num_path = str_count(temp1, ",") + 1
                
                !> path array
                call split(temp1(right(1) + 1: left(2) - 1), path, delimiters=',', order='sequential', nulls='ignore')
                
                !> mkdir output/Result, output/log and output/temp and set it in the dictionary
                if (path(1)(len_trim(path(1)): len_trim(path(1))) == '/' .or. &
                    path(1)(len_trim(path(1)): len_trim(path(1))) == '\') then
                    call system("mkdir "//trim(path(1))//'Result')
                    call system("mkdir "//trim(path(1))//"log")
                    call system("mkdir "//trim(path(1))//"temp")
                    fplerror = self%urlpaths%set(key='ResultPath', value=[trim(path(1))//'Result'])
                    fplerror = self%urlpaths%set(key='LogPath', value=[trim(path(1))//'log'])
                    fplerror = self%urlpaths%set(key='TempPath', value=[trim(path(1))//'temp'])
                    result_path = trim(path(1))//'Result'
                else
                    if (index(trim(path(1)), "/") /= 0) then
                        sep = '/'
                    else
                        sep = '\'
                    end if
                    call system("mkdir "//trim(path(1))//sep//'Result')
                    call system("mkdir "//trim(path(1))//sep//'log')
                    call system("mkdir "//trim(path(1))//sep//'temp')
                    fplerror = self%urlpaths%set(key='ResultPath', value=[trim(path(1))//sep//'Result'])
                    fplerror = self%urlpaths%set(key='LogPath', value=[trim(path(1))//sep//'log'])
                    fplerror = self%urlpaths%set(key='TempPath', value=[trim(path(1))//sep//'temp'])
                    result_path = trim(path(1))//sep//'Result'
                end if
            end if
        end do read_in_xml

        !> date
        do i = 1, size(i_maneuver_time), 1
            man1 = start_epoch + create_timedelta(seconds=i_maneuver_time(i) - 18_ip + 28800_ip)
            temp1 = man1%isoformat()
            c_date1(i) = temp1(1: 10)
        end do
        c_date(1, 1) = c_date1(1)
        i_days = 1_ip
        do i = 2, size(i_maneuver_time), 1
            if (trim(c_date1(i)) /= trim(c_date(i_days, 1))) then
                i_days = i_days + 1_ip
                c_date(i_days, 1) = c_date1(i)
            end if
        end do

        !> assign input files
        if (i_days /= i_count_inputinfo) error stop "The number of days and the number of tag <InputFileInfo> are not compatible"
        allocate(c_inputfiles(i_count_inputinfo), stat=err)
        if (err /= 0) print *, "c_inputfiles: Allocation request denied"
        do i = 1, 15, 1
            select case (i)
            case (1)
                do j = 1, i_count_inputinfo, 1
                    do k = 1, size(c_acc1b_a), 1
                        if (index(trim(c_acc1b_a(k, 1)), trim(c_date(j, 1))) /= 0) c_inputfiles(j) = trim(c_acc1b_a(k, 1))
                    end do
                end do
                fplerror = self%urlpaths%set(key='ACC1B-AFile', value=c_inputfiles)
            case (2)
                do j = 1, i_count_inputinfo, 1
                    do k = 1, size(c_acc1b_b), 1
                        if (index(trim(c_acc1b_b(k, 1)), trim(c_date(j, 1))) /= 0) c_inputfiles(j) = trim(c_acc1b_b(k, 1))
                    end do
                end do
                fplerror = self%urlpaths%set(key='ACC1B-BFile', value=c_inputfiles)
            case (3)
                do j = 1, i_count_inputinfo, 1
                    do k = 1, size(c_acc1b_b), 1
                        if (index(trim(c_koe1b_a(k, 1)), trim(c_date(j, 1))) /= 0) c_inputfiles(j) = trim(c_koe1b_a(k, 1))
                    end do
                end do
                fplerror = self%urlpaths%set(key='KOE1B-AFile', value=c_inputfiles)
            case (4)
                do j = 1, i_count_inputinfo, 1
                    do k = 1, size(c_acc1b_b), 1
                        if (index(trim(c_koe1b_b(k, 1)), trim(c_date(j, 1))) /= 0) c_inputfiles(j) = trim(c_koe1b_b(k, 1))
                    end do
                end do
                fplerror = self%urlpaths%set(key='KOE1B-BFile', value=c_inputfiles)
            case (5)
                do j = 1, i_count_inputinfo, 1
                    do k = 1, size(c_acc1b_b), 1
                        if (index(trim(c_roi1b_a(k, 1)), trim(c_date(j, 1))) /= 0) c_inputfiles(j) = trim(c_roi1b_a(k, 1))
                    end do
                end do
                fplerror = self%urlpaths%set(key='ROI1B-AFile', value=c_inputfiles)
            case (6)
                do j = 1, i_count_inputinfo, 1
                    do k = 1, size(c_acc1b_b), 1
                        if (index(trim(c_roi1b_B(k, 1)), trim(c_date(j, 1))) /= 0) c_inputfiles(j) = trim(c_roi1b_B(k, 1))
                    end do
                end do
                fplerror = self%urlpaths%set(key='ROI1B-BFile', value=c_inputfiles)
            case (7)
                do j = 1, i_count_inputinfo, 1
                    do k = 1, size(c_acc1b_b), 1
                        if (index(trim(c_tha1b_a(k, 1)), trim(c_date(j, 1))) /= 0) c_inputfiles(j) = trim(c_tha1b_a(k, 1))
                    end do
                end do
                fplerror = self%urlpaths%set(key='THA1B-AFile', value=c_inputfiles)
            case (8)
                do j = 1, i_count_inputinfo, 1
                    do k = 1, size(c_acc1b_b), 1
                        if (index(trim(c_tha1b_B(k, 1)), trim(c_date(j, 1))) /= 0) c_inputfiles(j) = trim(c_tha1b_B(k, 1))
                    end do
                end do
                fplerror = self%urlpaths%set(key='THA1B-BFile', value=c_inputfiles)
            case (9)
                do j = 1, i_count_inputinfo, 1
                    do k = 1, size(c_acc1b_b), 1
                        if (index(trim(c_sca1b_a(k, 1)), trim(c_date(j, 1))) /= 0) c_inputfiles(j) = trim(c_sca1b_a(k, 1))
                    end do
                end do
                fplerror = self%urlpaths%set(key='SCA1B-AFile', value=c_inputfiles)
            case (10)
                do j = 1, i_count_inputinfo, 1
                    do k = 1, size(c_acc1b_b), 1
                        if (index(trim(c_sca1b_B(k, 1)), trim(c_date(j, 1))) /= 0) c_inputfiles(j) = trim(c_sca1b_B(k, 1))
                    end do
                end do
                fplerror = self%urlpaths%set(key='SCA1B-BFile', value=c_inputfiles)
            case (11)
                do j = 1, i_count_inputinfo, 1
                    do k = 1, size(c_acc1b_b), 1
                        if (index(trim(c_kbr1b_x(k, 1)), trim(c_date(j, 1))) /= 0) c_inputfiles(j) = trim(c_kbr1b_x(k, 1))
                    end do
                end do
                fplerror = self%urlpaths%set(key='KBR1BFile', value=c_inputfiles)
            case (12)
                fplerror = self%urlpaths%set(key='GKB1B-AFile', value=c_gkb1b_a)
            case (13)
                fplerror = self%urlpaths%set(key='GKB1B-BFile', value=c_gkb1b_b)
            case (14)
                fplerror = self%urlpaths%set(key='VAC1B-AFile', value=c_vac1b_a)
            case (15)
                fplerror = self%urlpaths%set(key='VAC1B-BFile', value=c_vac1b_b)
            end select
        end do
        if (allocated(c_inputfiles)) deallocate(c_inputfiles, stat=err)
        if (err /= 0) print *, "c_inputfiles: Deallocation request denied"
        
        !> create output filename for both satellite
        if (index(result_path, "/") .ne. 0) then
            call system("mkdir "//trim(result_path)//"/TH04-01_VKB1B_"//trim(date)//"_X_"//trim(version)//'/')
            vkbfile_name(1) = trim(result_path)//"/TH04-01_VKB1B_"//trim(date)//"_X_"//trim(version)//'/'//"TH04-01_VKB1B_"//trim(date)//"_A_"//trim(version)//'.asc'
            vkbfile_name(2) = trim(result_path)//"/TH04-01_VKB1B_"//trim(date)//"_X_"//trim(version)//'/'//"TH04-01_VKB1B_"//trim(date)//"_B_"//trim(version)//'.asc'
            c_vkbrptfilename(1) = trim(result_path)//"/TH04-01_VKB1B_"//trim(date)//"_X_"//trim(version)//'/'//"TH04-01_VKB1B_"//trim(date)//"_A_"//trim(version)//'.rpt'
            c_vkbrptfilename(2) = trim(result_path)//"/TH04-01_VKB1B_"//trim(date)//"_X_"//trim(version)//'/'//"TH04-01_VKB1B_"//trim(date)//"_B_"//trim(version)//'.rpt'
        else
            call system("mkdir "//trim(result_path)//"\TH04-01_VKB1B_"//trim(date)//"_X_"//trim(version)//'\')
            vkbfile_name(1) = trim(result_path)//"\TH04-01_VKB1B_"//trim(date)//"_X_"//trim(version)//'\'//"TH04-01_VKB1B_"//trim(date)//"_A_"//trim(version)//'.asc'
            vkbfile_name(2) = trim(result_path)//"\TH04-01_VKB1B_"//trim(date)//"_X_"//trim(version)//'\'//"TH04-01_VKB1B_"//trim(date)//"_B_"//trim(version)//'.asc'
            c_vkbrptfilename(1) = trim(result_path)//"\TH04-01_VKB1B_"//trim(date)//"_X_"//trim(version)//'\'//"TH04-01_VKB1B_"//trim(date)//"_A_"//trim(version)//'.rpt'
            c_vkbrptfilename(2) = trim(result_path)//"\TH04-01_VKB1B_"//trim(date)//"_X_"//trim(version)//'\'//"TH04-01_VKB1B_"//trim(date)//"_B_"//trim(version)//'.rpt'
        end if
        
        fplerror = xml_o%urlpaths%set(key="VKB", value=vkbfile_name)
        fplerror = xml_o%urlpaths%set(key='VKBRPT', value=c_vkbrptfilename)
        i_mirror = 0_ip

        ! call self%urlpaths%Print()
        ! call logger%info('xml_module', 'read in xml file successfully')

        close(unit=self%unit, iostat=ios)
        if ( ios /= 0 ) then
            !call logger%error('read xml file', 'Error closing file parameters.xml')
            stop 
        end if

    end subroutine lhs_read_xml

end module xml_module