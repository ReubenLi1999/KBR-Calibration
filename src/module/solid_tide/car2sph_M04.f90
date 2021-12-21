subroutine car2sph_M04( car, sph)
    use M04
    implicit none

!----declare variables----------
    real(rk),     intent(in   )         :: car(3)
    real(rk),     intent(  out)         :: sph(3)
    !---------------------------

!-------------------------------
    !Car: X,   Y,     Z (m,      m,      m)   
    !Sph: phi, lambd, r (degree, degree, m)
    
    Sph(3) = sqrt(Car(1)**2 + Car(2)**2 + Car(3)**2)
    
    Sph(1) = ASIND( Car(3)/Sph(3) )
    
    Sph(2) = ATAN2D(Car(2), Car(1))
    
    if (Sph(2) < 0) then
        Sph(2) = Sph(2) + 360
    end if

end subroutine car2sph_M04