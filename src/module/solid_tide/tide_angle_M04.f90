subroutine tide_angle_M04(JD_TT, gmst)

    use M04 
    implicit none 
    !----declare variables----------
    real(rk),     intent(in   )         :: JD_TT
    real(rk),     intent(in   )         :: gmst
    !---------------------------
    real(kind = 16)                     :: pi
    real(kind = 16)                     :: t
    real(kind = 16)                     :: TURNAS
    real(kind = 16)                     :: L, LP, F, D, OM
    real(kind = 16)                     :: DAS2R
    real(kind = 16)                     :: FF(5)
    integer(kind = 4)                   :: i, j
!-------------------------------
    pi = atan(1.0) * 4.0
    DAS2R=4.848136811095359935899141D-6
    TURNAS = 1296000D0
    t = (JD_TT - 2451545.5) / 36525D0
    L=MOD(485868.249036D0+T*(1717915923.2178D0+T*(31.8792D0+T*(0.051635D0+T*(-0.00024470D0)))),TURNAS) * DAS2R
    LP=MOD(1287104.79305D0+T*(129596581.0481D0+T*(-0.5532D0+T*(0.000136D0+T*(-0.00001149D0)))),TURNAS) * DAS2R
    F=MOD(335779.526232D0+T*(1739527262.8478D0+T*(-12.7512D0+T*(-0.001037D0+T*(0.00000417D0)))),TURNAS) * DAS2R
    D=MOD(1072260.70369D0+T*(1602961601.2090D0+T*(-6.3706D0+T*(0.006593D0+T*(-0.00003169D0)))),TURNAS) * DAS2R
    OM=MOD(450160.398036D0+T*(-6962890.5431D0+T*(7.4722D0+T*(0.007702D0+T*(-0.00005939D0)))),TURNAS) * DAS2R

    FF(1)=L
    FF(2)=LP
    FF(3)=F
    FF(4)=D
    FF(5)=OM

    do i = 1, n_tidal_k20
        TA20(i)=0
        do j = 1, 5
            TA20(i) = TA20(i) - delaunay_k20(i, j) * FF(j)
        end do
    end do
    
    do i = 1, n_tidal_k21
        TA21(i) = gmst + pi
        do j = 1, 5
            TA21(i) = TA21(i) - delaunay_k21(i, j) * FF(j)
        end do
    end do

    do i = 1, n_tidal_k22
        TA22(i) = 2.0 * (gmst + pi)
        do j = 1, 5
            TA22(i) = TA22(i) - delaunay_k22(i, j) * FF(j)
        end do
    end do
end subroutine tide_angle_M04