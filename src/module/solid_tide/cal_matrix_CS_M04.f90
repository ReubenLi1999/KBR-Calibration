subroutine cal_matrix_CS_M04(sph_sun, sph_moon, position, C_solid, S_solid)

    use M04 
    implicit None
!----declare variables----------
    real(rk),     intent(in   )         :: position(3)
    real(rk),     intent(in   )         :: sph_sun(3)
    real(rk),     intent(in   )         :: sph_moon(3)
    real(rk),     intent(  out)         :: C_solid(0:4, 0:4), S_solid(0:4, 0:4)
!-------------------------------
    C_solid(:,:) = 0
    S_solid(:,:) = 0
    call cal_C20_M04(sph_sun, sph_moon, position, C_solid(2,0), C_solid(4,0))
    call cal_CS21_M04(sph_sun, sph_moon, position, C_solid(2,1), S_solid(2,1), C_solid(4,1), S_solid(4,1))
    call cal_CS22_M04(sph_sun, sph_moon, position, C_solid(2,2), S_solid(2,2), C_solid(4,2), S_solid(4,2))
    call cal_CS3_M04(sph_sun, sph_moon, position, C_solid(3,0), C_solid(3,1), C_solid(3,2), S_solid(3,1), S_solid(3,2))

    ! write(*,*) C_solid(2,2), S_solid(2,2)!, C_solid(4,2), S_solid(4,2),&
    !  C_solid(3,0), C_solid(3,1), C_solid(3,2), S_solid(3,1), S_solid(3,2)

end subroutine cal_matrix_CS_M04

subroutine cal_C20_M04(sph_sun, sph_moon, position, C20, C40)
    use M04 
    implicit None
    !----declare variables----------
    real(rk),     intent(in   )         :: position(3)
    real(rk),     intent(in   )         :: sph_sun(3)
    real(rk),     intent(in   )         :: sph_moon(3)
    real(rk),     intent(  out)         :: C20, C40
    !---------------------------
    integer(ik)                         :: i, j
    real(rk)                            :: coe
    !-------------------------------
    C20 = (GM_SUN/GM_E) * ((R_E/sph_sun(3))**3) * (3 * (SIND(sph_sun(1)))**2 - 1)/2
    C20 = C20 + (GM_MOON/GM_E) * ((R_E/sph_moon(3))**3) * (3 * (SIND(sph_moon(1)))**2 - 1)/2
    C20 = C20 * k20/5D0 * SQRT(5D0)

    coe = 0
    DO i = 1, n_tidal_k20
        coe = coe + ( Amp_k20(i, 1) * COS( TA20(i) ) &
                     -Amp_k20(i, 2) * SIN( TA20(i) ) ) * 1D-12
    END DO
    C20 = C20 + coe


    !    C20 = C20 - (-0.3146D0) * A0 * k20

    C40 = (GM_SUN/GM_E) * ((R_E/sph_sun(3))**3) * (3 * SIND(sph_sun(1))**2 - 1)/2
    C40 = C40 + (GM_Moon/GM_E) * ((R_E/sph_moon(3))**3) * (3 * SIND(sph_moon(1))**2 - 1)/2
    C40 = C40 * k20_plus/5D0 * SQRT(5D0)

end subroutine cal_C20_M04

subroutine cal_CS21_M04(sph_sun, sph_moon, position, C21, S21, C41, S41)
    use M04 
    implicit None
    !----declare variables----------
    real(rk),     intent(in   )         :: position(3)
    real(rk),     intent(in   )         :: sph_sun(3)
    real(rk),     intent(in   )         :: sph_moon(3)
    real(rk),     intent(  out)         :: C21, S21, C41, S41
    !---------------------------
    integer(ik)                         :: i, j
    real(rk)                            :: coe1, coe2
    real(rk)                            :: C_v, S_v
    !-------------------------------
    
    C_v = (GM_SUN/GM_E) * ((R_E/sph_sun(3))**3) * (3*SIND(sph_sun(1))) &
        * SQRT(1 - SIND(sph_sun(1))**2) * COSD(sph_sun(2))
    C_v = C_v + (GM_Moon/GM_E) * ((R_E/sph_moon(3))**3) * (3*SIND(sph_moon(1))) &
        * SQRT(1 - SIND(sph_moon(1))**2) * COSD(sph_moon(2))

    S_v = (GM_SUN/GM_E) * ((R_E/sph_sun(3))**3) * (3*SIND(sph_sun(1))) &
        * SQRT((1 - (SIND(sph_sun(1)))**2)) * SIND(sph_sun(2))
    S_v = S_v + (GM_Moon/GM_E) * ((R_E/sph_moon(3))**3) * (3*SIND(sph_moon(1))) &
        * SQRT((1 - (SIND(sph_moon(1)))**2)) * SIND(sph_moon(2))

    C21 = C_v * k21/5D0 * SQRT(5D0/3D0)
    S21 = S_v * k21/5D0 * SQRT(5D0/3D0)
    C41 = C_v * k21_plus/5D0 * SQRT(5D0/3D0)
    S41 = S_v * k21_plus/5D0 * SQRT(5D0/3D0)
    coe1 = 0
    coe2 = 0
    DO i = 1, n_tidal_k21
        coe1 = coe1 + ( Amp_k21(i, 1) * SIN( TA21(i) ) &
                    + Amp_k21(i, 2) * COS( TA21(i) ) ) * 1D-12
        
        coe2 = coe2 + ( Amp_k21(i, 1) * COS( TA21(i) ) &
                    - Amp_k21(i, 2) * SIN( TA21(i) ) ) * 1D-12 
    END DO
  
    C21 = C21 + coe1
    S21 = S21 + coe2

end subroutine cal_CS21_M04

subroutine cal_CS22_M04(sph_sun, sph_moon, position, C22, S22, C42, S42)
    use M04 
    implicit None
    !----declare variables----------
    real(rk),     intent(in   )         :: position(3)
    real(rk),     intent(in   )         :: sph_sun(3)
    real(rk),     intent(in   )         :: sph_moon(3)
    real(rk),     intent(  out)         :: C22, S22, C42, S42
    !---------------------------
    integer(ik)                         :: i, j
    real(rk)                            :: C_v, S_v
    real(rk)                            :: coe1, coe2
    !-------------------------------
    C_v = (GM_SUN/GM_E)  * ((R_E/sph_sun(3))**3)  * 3 * (1 - (SIND(sph_sun(1)))**2)  * COSD(2 * sph_sun(2))
    C_v = C_v &
        + (GM_Moon/GM_E) * ((R_E/sph_moon(3))**3) * 3 * (1 - (SIND(sph_moon(1)))**2) * COSD(2 * sph_moon(2))

    S_v = (GM_SUN/GM_E)  * ((R_E/sph_sun(3))**3)  * 3 * (1 - (SIND(sph_sun(1)))**2)  * SIND(2 * sph_sun(2))
    S_v = S_v &
        + (GM_Moon/GM_E) * ((R_E/sph_moon(3))**3) * 3 * (1 - (SIND(sph_moon(1)))**2) * SIND(2 * sph_moon(2))

    C22 = C_v * k22/5D0 * SQRT(5D0/12D0)
    S22 = S_v * k22/5D0 * SQRT(5D0/12D0)
    C42 = C_v * k22_plus/5D0 * SQRT(5D0/12D0)
    S42 = S_v * k22_plus/5D0 * SQRT(5D0/12D0)

    coe1 = 0
    coe2 = 0
    do i = 1, n_tidal_k22

        coe1 = coe1 + Amp_k22(i) * COS( TA22(i) ) * 1D-12 
                  
        coe2 = coe2 + (-1) * Amp_k22(i) * SIN( TA22(i) ) * 1D-12 

    end do
  
    C22 = C22 + coe1
    S22 = S22 + coe2

end subroutine cal_CS22_M04

subroutine cal_CS3_M04(sph_sun, sph_moon, position, C30, C31, C32, S31, S32)
    use M04 
    implicit None
    !----declare variables----------
    real(rk),     intent(in   )         :: position(3)
    real(rk),     intent(in   )         :: sph_sun(3)
    real(rk),     intent(in   )         :: sph_moon(3)
    real(rk),     intent(  out)         :: C30, C31, C32, S31, S32
    !-------------------------------
    C30 = (GM_SUN/GM_E) * ((R_E/sph_sun(3))**4) * (5*SIND(sph_sun(1))**3 - 3 * SIND(sph_sun(1))) / 2
    C30 = C30 + (GM_Moon/GM_E) * ((R_E/sph_moon(3))**4) * (5*SIND(sph_moon(1))**3 - 3 * SIND(sph_moon(1))) / 2
    C30 = C30 * K30/SQRT(7D0)
  
    C31=(GM_SUN/GM_E)*((R_E/sph_sun(3))**4)*SQRT(1-SIND(sph_sun(1))**2)*(5*SIND(sph_sun(1))**2-1)*COSD(sph_sun(2))*3/2
    C31=C31+(GM_Moon/GM_E)*((R_E/sph_moon(3))**4)*SQRT(1-SIND(sph_moon(1))**2)*(5*SIND(sph_moon(1))**2-1)*COSD(sph_moon(2))*3/2
    C31=C31*K31/7*SQRT(7D0/6D0)
    
    S31=(GM_SUN/GM_E)*((R_E/sph_sun(3))**4)*SQRT(1-SIND(sph_sun(1))**2)*(5*SIND(sph_sun(1))**2-1)*SIND(sph_sun(2))*3/2
    S31=S31+(GM_Moon/GM_E)*((R_E/sph_moon(3))**4)*SQRT(1-SIND(sph_moon(1))**2)*(5*SIND(sph_moon(1))**2-1)*SIND(sph_moon(2))*3/2
    S31=S31*K31/7*SQRT(7D0/6D0)
  
    C32=(GM_SUN/GM_E)*((R_E/sph_sun(3))**4)*15*(1-SIND(sph_sun(1))**2)*SIND(sph_sun(1))*COSD(2*sph_sun(2))
    C32=C32+(GM_Moon/GM_E)*((R_E/sph_moon(3))**4)*15*(1-SIND(sph_moon(1))**2)*SIND(sph_moon(1))*COSD(2*sph_moon(2))
    C32=C32*K32/7*SQRT(7D0/60D0)
    
    S32=(GM_SUN/GM_E)*((R_E/sph_sun(3))**4)*15*(1-SIND(sph_sun(1))**2)*SIND(sph_sun(1))*SIND(2*sph_sun(2))
    S32=S32+(GM_Moon/GM_E)*((R_E/sph_moon(3))**4)*15*(1-SIND(sph_moon(1))**2)*SIND(sph_moon(1))*SIND(2*sph_moon(2))
    S32=S32*K32/7*SQRT(7D0/60D0)
end subroutine cal_CS3_M04