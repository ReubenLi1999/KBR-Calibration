subroutine llsq0 ( n, x, y, a )
use num_kinds_module

!*****************************************************************************80
!
!! LLSQ solves a linear least squares problem matching y=a*x to data.
!
!  Discussion:
!
!    A formula for a line of the form Y = A * X is sought, which
!    will minimize the root-mean-square error to N data points ( X(I), Y(I) );
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    15 January 2019
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( ip ) N, the number of data values.
!
!    Input, real ( wp ) X(N), Y(N), the coordinates of the data points.
!
!    Output, real ( wp ) A the slope of the 
!    least-squares approximant to the data.
!
  implicit none

  integer ( ip ) n

  real ( wp ) a
  real ( wp ) bot
  real ( wp ) top
  real ( wp ) x(n)
  real ( wp ) y(n)
!
!  Special case.
!
  if ( n == 1 ) then
    if ( x(1) == 0.0D+00 ) then
      a = 1.0D+00
    else
      a = y(1) / x(1)
    end if
    return
  end if

  top = dot_product ( x(1:n), y(1:n) )
  bot = dot_product ( x(1:n), x(1:n) )

  a = top / bot

  return
end
subroutine llsq ( n, x, y, a, b )
use num_kinds_module

!*****************************************************************************80
!
!! LLSQ solves a linear least squares problem matching a line to data.
!
!  Discussion:
!
!    A formula for a line of the form Y = A * X + B is sought, which
!    will minimize the root-mean-square error to N data points ( X(I), Y(I) );
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    07 March 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( ip ) N, the number of data values.
!
!    Input, real ( wp ) X(N), Y(N), the coordinates of the data points.
!
!    Output, real ( wp ) A, B, the slope and Y-intercept of the 
!    least-squares approximant to the data.
!
  implicit none

  integer ( ip ) n

  real ( wp ) a
  real ( wp ) b
  real ( wp ) bot
  real ( wp ) top
  real ( wp ) x(n)
  real ( wp ) xbar
  real ( wp ) y(n)
  real ( wp ) ybar
!
!  Special case.
!
  if ( n == 1 ) then
    a = 0.0D+00
    b = y(1)
    return
  end if
!
!  Average X and Y.
!
  xbar = sum ( x(1:n) ) / real ( n, wp )
  ybar = sum ( y(1:n) ) / real ( n, wp )
!
!  Compute Beta.
!
  top = dot_product ( x(1:n) - xbar, y(1:n) - ybar )
  bot = dot_product ( x(1:n) - xbar, x(1:n) - xbar )

  a = top / bot

  b = ybar - a * xbar

  return
end
subroutine timestamp ( )
use num_kinds_module
!*****************************************************************************80
!
!! TIMESTAMP prints the current YMDHMS date as a time stamp.
!
!  Example:
!
!    31 May 2001   9:45:54.872 AM
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    None
!
  implicit none

  character ( len = 8 ) ampm
  integer ( ip ) d
  integer ( ip ) h
  integer ( ip ) m
  integer ( ip ) mm
  character ( len = 9 ), parameter, dimension(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
  integer ( ip ) n
  integer ( ip ) s
  integer ( ip ) values(8)
  integer ( ip ) y

  call date_and_time ( values = values )

  y = values(1)
  m = values(2)
  d = values(3)
  h = values(5)
  n = values(6)
  s = values(7)
  mm = values(8)

  if ( h < 12 ) then
    ampm = 'AM'
  else if ( h == 12 ) then
    if ( n == 0 .and. s == 0 ) then
      ampm = 'Noon'
    else
      ampm = 'PM'
    end if
  else
    h = h - 12
    if ( h < 12 ) then
      ampm = 'PM'
    else if ( h == 12 ) then
      if ( n == 0 .and. s == 0 ) then
        ampm = 'Midnight'
      else
        ampm = 'AM'
      end if
    end if
  end if

  write ( *, '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end