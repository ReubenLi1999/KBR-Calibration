subroutine interplation_3points_M04(x, x_knots, y_knots, y)
    use M04
    implicit none
!----declare variables----------
    real(rk),     intent(in   )         :: x_knots(1 : 3)
    real(rk),     intent(in   )         :: y_knots(1 : 3)
    real(rk),     intent(in   )         :: x
    real(rk),     intent(  out)         :: y
!-------------------------------
    y = y_knots(1) * ( (x - x_knots(2)) * (x - x_knots(3)) ) / ( (x_knots(1) - x_knots(2)) * (x_knots(1) - x_knots(3)) )&
      + y_knots(2) * ( (x - x_knots(1)) * (x - x_knots(3)) ) / ( (x_knots(2) - x_knots(1)) * (x_knots(2) - x_knots(3)) )&
      + y_knots(3) * ( (x - x_knots(1)) * (x - x_knots(2)) ) / ( (x_knots(3) - x_knots(1)) * (x_knots(3) - x_knots(2)) )


end subroutine interplation_3points_M04