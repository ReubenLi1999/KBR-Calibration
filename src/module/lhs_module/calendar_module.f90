module CalendarModule
 implicit none
 character(len=9), parameter :: monthNames(12) = [ "January ", "February ", "March ", "April ", &
 "May ", "June ", "July ", "August ", &
"September", "October ", "November ", "December " ]
 character(len=3), parameter :: shortMonthNames(12) = [ "Jan", "Feb", "Mar", "Apr", "May", "Jun", &
 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" ]
 integer, parameter :: daysInMonth(12) = [ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ]
 integer daysPreMonth(12)
 character(len=9), parameter :: weekDays(7) = [ "Sunday ", "Monday ", "Tuesday ", "Wednesday", &
 "Thursday ", "Friday ", "Saturday " ]
 character(len=3), parameter :: shortWeekDays(7) = [ "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" ]
 
 
 integer, parameter :: datumYear = 2000 ! 1 January 2000
 integer, parameter :: datumDay = 7 ! was a Sunday 
 character(len=*), parameter :: fmta = "( a )"
contains
 !------------------------------------
 subroutine setup
 integer i
 daysPreMonth(1) = 0
 do i = 2, 12
 daysPreMonth(i) = daysPreMonth(i-1) + daysInMonth(i-1)
 end do
 end subroutine setup
 !------------------------------------
 logical function isLeap( year )
 integer, intent(in) :: year
 if ( modulo( year, 4 ) == 0 ) then
 if ( modulo( year, 100 ) == 0 .and. modulo( year, 400 ) /= 0 ) then 
 isLeap = .false.
 else
 isLeap = .true.
 end if
 else
 isLeap = .false.
 end if
 end function isLeap
 !------------------------------------
 integer function firstDayOfYear( year ) ! returns number (1-7) of day in week of year start
 integer, intent(in) :: year
 firstDayOfYear = 365 * ( year - datumYear )
 if ( year > datumYear ) then
 firstDayOfYear = firstDayOfYear + 1 & ! datum year was leap
 + ( year - datumYear - 1 ) / 4 & ! with a further leap year every 4 years
 - ( year - datumYear - 1 ) / 100 & ! omitting a leap year every 100 years
 + ( year - datumYear - 1 ) / 400 ! reinstating every 400 years
 else if ( year < datumYear ) then
 firstDayOfYear = firstDayOfYear &
 - ( datumYear - year ) / 4 & ! with a further leap year every 4 years
 + ( datumYear - year ) / 100 & ! omitting a leap year every 100 years
 - ( datumYear - year ) / 400 ! reinstating every 400 years
 end if
 firstDayOfYear = 1 + modulo( firstDayOfYear + datumDay - 1, 7 ) ! put in range 1-7
 end function firstDayOfYear
 !------------------------------------
 integer function dayOfYear( d, m, y ) ! returns number (1-365 or 366) of day in year
 !> d for date, m for month, y for year
 integer, intent(in) :: d, m, y
 call setup
 dayOfYear = daysPreMonth( m ) + d
 if ( isLeap( y ) .and. m > 2 ) dayOfYear = dayOfYear + 1
 end function dayOfYear
 !------------------------------------
 integer function dayOfWeek( d, m, y ) ! returns number (1-7) of day in week
 integer, intent(in) :: d, m, y
 dayOfWeek = dayOfYear( d, m, y ) + firstDayOfYear( y ) - 1
 dayOfWeek = 1 + modulo( dayOfWeek - 1, 7 )
 end function dayOfWeek
 !------------------------------------
 subroutine monthCalendar( m, y )
 integer, intent(in) :: m, y
 integer d1, d2, dw1, dw2, pos
 integer i
 
 write( *, fmta ) monthNames(m)
 write( *, "( 7( a6 ) )" ) shortWeekDays
 
 d1 = 1
 d2 = daysInMonth(m); if ( isLeap( y ) .and. m == 2 ) d2 = d2 + 1
 dw1 = dayOfWeek( d1, m, y )
 dw2 = dayOfWeek( d2, m, y )
 pos = 1
 do i = 1, dw1 - 1
 write( *, "( a6 )", advance="no" ) "-"
 pos = pos + 1
 end do
 do i = d1, d2
 write( *, "( i6 )", advance="no" ) i
 pos = pos + 1
 if ( pos > 7 .and. i < d2 ) then
 write( *, * )
 pos = 1
 end if
 end do
 do while ( pos > 1 .and. pos <= 7 )
 write( *, "( a6 )", advance="no" ) "-"
 pos = pos + 1
 end do
 write( *, * )
 write( *, * )
 
 end subroutine monthCalendar
 !------------------------------------
 subroutine yearCalendar( y )
 integer, intent(in) :: y
 integer m
 write( *, "( 'Year: ', i4 )" ) y
 do m = 1, 12
 call monthCalendar( m, y )
 end do
 write( *, * )
 
 end subroutine yearCalendar
 !------------------------------------
 integer function menu()
 
 write( *, fmta ) "Choose:"
 write( *, fmta ) "1: find day"
 write( *, fmta ) "2: print month calendar"
 write( *, fmta ) "3: print year calendar"
 write( *, fmta ) "0: finish"
 read( *, * ) menu
 end function menu
 !-----------------------------------
end module CalendarModule