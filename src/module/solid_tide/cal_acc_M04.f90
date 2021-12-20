subroutine cal_acc_M04(n, C, S, position, acc)
    use M04 
    implicit none 

!----declare variables----------
    integer(ik),    intent(in   )           :: n
    real(rk),       intent(in   )           :: C(0:n, 0:n), S(0:n, 0:n)
    real(rk),       intent(in   )           :: position(3)
    real(rk),       intent(  out)           :: acc(3)
    !---------------------------
    real(rk)                                :: E(0:n+1, 0:n+1), F(0:n+1, 0:n+1)
    real(rk)                                :: r, x, y, z
    real(rk)                                :: accx, accy, accz
    integer(ik)                             :: i, j
    real(rk)                                :: pi   
    real(rk) CalBi_M04
!-------------------------------
    
    x = position(1)
    y = position(2)
    z = position(3) 
    pi = atan(1.0) * 4.0
    r = sqrt(position(1)**2 + position(2)**2 + position(3)**2)
    call CalculateEF_M04(position, n + 1, E, F)

    acc(1:3) = 0
    accx = 0
    accy = 0
    accz = 0
    do i = 0, n
        do j = 0, i
            if(j == 0) then
                accx = GM_E / R_E**2 * (-CalBi_M04(i,j,1)) * E(i+1,1) * C(i,0)
            elseif(j > 0) then
                accx = GM_E / (2D0 * R_E**2) * (CalBi_M04(i,j,2) * (-E(i+1,j+1) * C(i,j) - F(i+1,j+1) * S(i,j))&
                     + CalBi_M04(i,j,3) * (E(i+1,j-1) * C(i,j) + F(i+1,j-1) * S(i,j)))
            end if
            if(j==0) then
                accy = GM_E / R_E**2 * (-CalBi_M04(i,j,1)) * F(i+1,1) * C(i,0)
            else if(j>0) then
                accy = GM_E / 2.0 / R_E**2 &
                     * (CalBi_M04(i,j,2) * (-F(i+1,j+1) * C(i,j) + E(i+1,j+1) * S(i,j))&
                     + CalBi_M04(i,j,3) * (-F(i+1,j-1) * C(i,j) + E(i+1,j-1) * S(i,j)))
            end if

            accz = GM_E / R_E**2 * (CalBi_M04(i,j,4) * (-E(i+1,j) * C(i,j) - F(i+1,j) * S(i,j)))
            
            acc(1) = acc(1) + accx
            acc(2) = acc(2) + accy
            acc(3) = acc(3) + accz
    
        end do
    end do


end subroutine cal_acc_M04

subroutine CalculateEF_M04(position, n, E, F)
    use M04 
    implicit none
    !----declare variables----------
    integer(ik),    intent(in   )           :: n
    real(rk),       intent(in   )           :: position(3)
    real(rk),       intent(  out)           :: E(0:n,0:n), F(0:n,0:n)
    !---------------------------
    real(rk)                                :: r, x, y, z
    integer(ik)                             :: i, j
    real(rk)                                :: coe1, coe2
    !-------------------------------
    x = position(1)
    y = position(2)
    z = position(3)


    r=sqrt(x**2+y**2+z**2)
    E = 0
    F = 0
    E(0,0) = R_E/r
    E(1,0) = sqrt(3.0) * z * (R_E**2)/(r**3)
    E(1,1) = sqrt(3.0) * x * (R_E**2)/(r**3)
    F(0,0) = 0
    F(1,0) = 0
    F(1,1) = sqrt(3.0) * y * (R_E**2)/(r**3)
 
    do i = 2, n
        do j = 0, i
            if(i == j) then
                coe1 = sqrt( real(2*j+1)/(2*j) )
                E(i,j) = coe1 * (x * R_E * E(i-1,j-1) / (r**2) - y * R_E * F(i-1, j-1) / (r**2))
                F(i,j) = coe1 * (x * R_E * F(i-1,j-1) / (r**2) + y * R_E * E(i-1, j-1) / (r**2))
            else
                coe1 = sqrt( real(2*i+1) * (2*i-1) / ((i-j) * (i+j)) )
                coe2 = sqrt( (real(2*i+1) * (i-j-1) * (i+j-1)) / ((i-j) * (i+j)*  (2*i-3)) )
                E(i,j) = coe1 * z * R_E * E(i-1,j) / (r**2) - coe2 * (R_E**2) * E(i-2,j) / (r**2)
                F(i,j) = coe1 * z * R_E * F(i-1,j) / (r**2) - coe2 * (R_E**2) * F(i-2,j) / (r**2)
            end if
        end do
    end do
 
end subroutine CalculateEF_M04


Real(rk) Function CalBi_M04(i, j, m)
    use M04
    implicit none
    integer(ik) i, j, m
    real(rk) fac
    if(m == 1) then
        fac   = real((i + 1) * (i + 2) * (2 * i + 1))
        CalBi_M04 = sqrt(fac / 2.0 / real(2 * i + 3))
    else if(m == 2) then
        fac   = real((i + j + 1) * (i + j + 2) * (2 * i + 1))
        CalBi_M04 = sqrt(fac / real(2 * i + 3))
    else if(m == 3) then
        fac   = real((i - j + 1) * (i - j + 2) * (2 * i + 1))
        if (j == 1) then
            CalBi_M04 = sqrt(fac / real(2 * i + 3) * 2.0)
        else
            CalBi_M04 = sqrt(fac / real(2 * i + 3))
    end if
    else if(m == 4) then
        fac   = real((i - j + 1) * (i + j + 1) * (2 * i + 1))
        CalBi_M04 = sqrt(fac / real(2 * i + 3))
    else
        write(*, *) 'mֵ�������?', m
    end if

End Function CalBi_M04
