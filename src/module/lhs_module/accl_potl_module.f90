module accl_potl_module
    use num_kinds_module
    
    implicit none
    type sate_char
        real(wp)                                :: r_inertial(3)
        real(wp)                                :: r_earth(3) = 0
        real(wp)                                :: v_inertial(3)
        real(wp)                                :: acc_rela_inertial(3)
        real(wp)                                :: acc_thre_inertial(3)
        real(wp)                                :: acc_solid_inertial(3)
        real(wp)                                :: potential
        real(wp)                                :: acc_grav_inertial(3)
        real(wp)                                :: acc_grav_earth(3)
        real(wp)                                :: acc_non_grav(3)
    end type

    real(wp)                                    :: gm_other(10)
    real(wp), allocatable                       :: cb09(:, :)
    real(wp), allocatable                       :: cb10(:, :)
    
    type(sate_char)                             :: gl

contains
    !*****************************************************************************************

!*****************************************************************************************
!>
!  Compute the gravitational acceleration vector using the Mueller method.
!
!# References
!  1. Alan C. Mueller, "A Fast Recursive Algorithm for Calculating the
!     Forces due to the Geopotential (Program GEOPOT)",
!     JSC Internal Note 75-FM-42, June 9, 1975.
!
!@warning WARNING: this one crashes if nmax/=mmax

    subroutine geopot(x,y,z,nmax,mmax,re,ksq,c,s,fx,fy,fz)
        !subroutine arguments:
        real(wp),intent(in)              :: x      !! position vector x-component
        real(wp),intent(in)              :: y      !! position vector y-component
        real(wp),intent(in)              :: z      !! position vector z-component
        integer,intent(in)               :: nmax   !! degree of model
        integer,intent(in)               :: mmax   !! order+1 of model
        real(wp),intent(in)              :: re     !! body radius
        real(wp),intent(in)              :: ksq    !! body GM
        real(wp),dimension(:),intent(in) :: c      !! C coefficients
        real(wp),dimension(:),intent(in) :: s      !! S coefficients
        real(wp),intent(out)             :: fx     !! gravitational acceleration x-component
        real(wp),intent(out)             :: fy     !! gravitational acceleration y-component
        real(wp),intent(out)             :: fz     !! gravitational acceleration z-component

        !local variables:
        real(wp) :: r,ri,reor,reorn,ksqor2,xor,yor,zor,rdedx,rdedy,rdedz,&
                    sum1,sum2,sum3,sum4,temp1,temp2,temp3,temp4,fact,dcstld,temp
        integer :: i,j,k,im1,l,jm1,jp1,kk
        real(wp),dimension(0:mmax) :: p0,p1,p2,ctil,stil

        !write(*,'(A,1x,*(e20.5,1x/))') 'c=',c
        !write(*,'(A,1x,*(e20.5,1x/))') 's=',s

        !abbreviations:

        r      = sqrt(x*x + y*y + z*z)
        ri     = 1.0_wp/r
        reor   = re*ri
        reorn  = reor
        ksqor2 = ksq*ri*ri
        zor    = z*ri
        xor    = x*ri
        yor    = y*ri

        !the derivatives of the argument of the legendre polynomial - zor

        rdedz = zor*zor - 1.0_wp
        rdedx = zor*xor
        rdedy = zor*yor

        !initialization:

        k = 0
        do i=1,mmax
            p0(i) = 0.0_wp
            p1(i) = 0.0_wp
        end do
        p0(0)   = 1.0_wp
        p1(0)   = zor
        p1(1)   = 1.0_wp
        ctil(0) = 1.0_wp
        stil(0) = 0.0_wp
        ctil(1) = xor
        stil(1) = yor
        sum1    = 0.0_wp
        !sum2   = 0.0_wp       !original
        sum2    = 1.0_wp        !JW : include central body term
        sum3    = 0.0_wp
        sum4    = 0.0_wp

        !computation of forces:

        do i = 2,nmax

            reorn = reorn*reor
            fact  = 2*i - 1
            im1   = i-1
            l     = 1

            !recursion formulas for legendre polynomial - p2(0)

            p2(0) = (fact*zor*p1(0)-im1*p0(0))/i
            k     = k + 1
            p2(1) = p0(1)+fact*p1(0)
            temp1 = p2(1)*c(k)
            temp2 = p2(0)*c(k)*(i+1)

            if (i < mmax) then

                !recursive formulas for:
                !    'ctilda' - ctil
                !    'stilda' - stil

                ctil(i) = ctil(1)*ctil(im1) - stil(1)*stil(im1)
                stil(i) = stil(1)*ctil(im1) + ctil(1)*stil(im1)
                temp3   = 0.0_wp
                temp4   = 0.0_wp

                do j=1,i

                    jm1 = j-1
                    jp1 = j+1

                    !recursive formula for derivative of legendre polynomial - p2(j)

                    p2(jp1) = p0(jp1) + fact*p1(j)
                    kk      = k + j
                    dcstld  = j*p2(j)
                    temp    = (c(kk)*ctil(j)+s(kk)*stil(j))
                    temp1   = temp1+p2(jp1)*temp
                    temp2   = temp2+(i+jp1)*p2(j)*temp
                    temp3   = temp3+dcstld*(c(kk)*ctil(jm1)+s(kk)*stil(jm1))
                    temp4   = temp4-dcstld*(c(kk)*stil(jm1)-s(kk)*ctil(jm1))

                end do

                l = i
                sum3 = sum3+reorn*temp3
                sum4 = sum4+reorn*temp4

            end if

            sum1 = sum1+reorn*temp1
            sum2 = sum2+reorn*temp2
            k = k + i

            !shift indices:

            do j = 0, l
                p0(j) = p1(j)
                p1(j) = p2(j)
            end do

        end do

        fx = -ksqor2*(sum1*rdedx + sum2*xor - sum3 )
        fy = -ksqor2*(sum1*rdedy + sum2*yor - sum4 )
        fz = -ksqor2*(sum1*rdedz + sum2*zor        )

    end subroutine geopot

    subroutine AccXyz(position, acc_wp, degree, c, s)
        use ddeabm_kinds
        Implicit None

        integer          , intent(in   )             :: degree
        real(wp), intent(in   )             :: position(3)
        double  precision            :: acc(3)
        real(wp) acc_wp(3)

        Real(wp) :: C(0: degree, 0: degree),         S(0: degree, 0: degree)
        real(wp) :: E(0: degree + 1, 0: degree + 1), F(0: degree + 1, 0: degree + 1)
        Real(wp) AccXx, AccYy, AccZz, AccXS, AccYS, AccZS
        Integer i, j
        Real(wp) x, y, z
        Real(wp) :: mu, REarth

        x = position(1)
        y = position(2)
        z = position(3)


        mu     = 398600.4415_wp * 1000000000.0_wp
        REarth = 6.3781363E+6_wp   

        !Call ReadCS(C, S, degree)
        !c = th%cs_coffes%c_coeffs
        !s = th%cs_coeffs%s_coeffs

        Call CalculateEF(x, y, z, degree + 1, E, F)

        AccXS = 0
        AccYS = 0
        AccZS = 0

        do i = 0, degree
            do j = 0, i

                if(j == 0) then
                    AccXx = mu / REarth**2 * (-CalBi(i, j, 1)) * E(i + 1, 1) * C(i, 0)
                else
                    AccXx = mu / 2.0_wp / REarth**2 * &
                        ( CalBi(i, j, 2) * (-E(i + 1, j + 1) * C(i, j) - F(i + 1, j + 1) * S(i, j))&
                        + CalBi(i, j, 3) * ( E(i + 1, j - 1) * C(i, j) + F(i + 1, j - 1) * S(i, j)))
                end if

                if(j == 0) then
                    AccYy = mu / REarth**2 * (-CalBi(i, j, 1)) * F(i + 1, 1) * C(i, 0)
                else
                    AccYy = mu / 2.0_wp / REarth**2 * &
                        ( CalBi(i, j, 2) * (-F(i + 1, j + 1) * C(i, j) + E(i + 1, j + 1) * S(i, j))&
                        + CalBi(i, j, 3) * (-F(i + 1, j - 1) * C(i, j) + E(i + 1, j - 1) * S(i, j)))
                end if

                AccZz = mu / REarth**2 * &
                        ( CalBi(i, j, 4) * (-E(i + 1, j) * C(i, j) - F(i + 1, j) * S(i, j)))

                AccXS = AccXS + AccXx
                AccYS = AccYS + AccYy
                AccZS = AccZS + AccZz

            end do
        end do

        acc(1) = accxs
        acc(2) = accys
        acc(3) = acczs
        acc_wp = acc
    end subroutine accxyz

    Real(wp) Function CalBi(i, j, m)
        integer i, j, m
        real(wp) fac
        if(m == 1) then
            fac   = real((i + 1) * (i + 2) * (2 * i + 1))
            CalBi = sqrt(fac / 2.0_wp / real(2 * i + 3))
        else if(m == 2) then
            fac   = real((i + j + 1) * (i + j + 2) * (2 * i + 1))
            calbi = sqrt(fac / real(2 * i + 3))
        else if(m == 3) then
            fac   = real((i - j + 1) * (i - j + 2) * (2 * i + 1))
            if (j == 1) then
                CalBi = sqrt(fac / real(2 * i + 3) * 2.0_wp)
            else
                calbi = sqrt(fac / real(2 * i + 3))
            end if
        else if(m == 4) then
            fac   = real((i - j + 1) * (i + j + 1) * (2 * i + 1))
            CalBi = sqrt(fac / real(2 * i + 3))
        else
            write(*, *) 'm error', m
        end if

    End Function CalBi

    subroutine CalculateEF(x, y, z, n, E, F)
        implicit none
        integer(kind = 4), intent(in   ) :: n
        real(wp)   , intent(in   ) :: x, y, z
        real(wp)   , intent(  out) :: E(0: n, 0: n), F(0: n, 0: n)
        integer(kind = 4)                :: i, j
        real(wp)                   :: r, REarth
        real(wp)                   :: coe1, coe2


        REarth = 6.3781363E+6_wp

        r = sqrt(x**2 + y**2 + z**2)
        
        E = 0
        F = 0
        E(0, 0) = REarth / r
        E(1, 0) = sqrt(3.0_wp) * z * (REarth**2) / (r**3)
        E(1, 1) = sqrt(3.0_wp) * x * (REarth**2) / (r**3)
        F(0, 0) = 0.0_wp
        F(1, 0) = 0.0_wp
        F(1, 1) = sqrt(3.0_wp) * y * (REarth**2) / (r**3)

        do i = 2, n
            do j = 0, i
                if(i == j) then
                    coe1 = sqrt(real(2 * j + 1) / (2 * j))
                    E(i, j) = coe1 * (x * REarth * E(i - 1, j - 1) / (r**2) - &
                                      y * REarth * F(i - 1, j - 1) / (r**2))
                    F(i, j) = coe1 * (x * REarth * F(i - 1, j - 1) / (r**2) + &
                                      y * REarth * E(i - 1, j - 1) / (r**2))
                else
                    coe1 = sqrt(real(2 * i + 1) * (2 * i - 1) / ((i - j) * (i + j)))
                    coe2 = sqrt((real(2 * i + 1) * (i - j - 1) * (i + j - 1)) / &
                                     ((i - j) * (i + j) * (2 * i - 3)))
                    E(i, j) = coe1 * z * REarth * E(i - 1, j) / (r**2) - &
                              coe2 * REarth**2 * E(i - 2, j) / (r**2)
                    F(i, j) = coe1 * z * REarth * F(i - 1, j) / (r**2) - &
                              coe2 * REarth**2 * F(i - 2, j) / (r**2)
                    !if((i - j - 1) == 0) then
                    !    write(*,*) F(i, j), F(i - 2, j), i
                    !end if
                end if
            end do
        end do

    end subroutine

    subroutine RelativisticEffect(Velocity, Coordinate, gamma, beta, Acceleration)
        use ddeabm_kinds
        implicit none
        real(wp), intent(in   )      :: Velocity(1:3)             
        real(wp), intent(in   )      :: Coordinate(1:3)     
        real(kind = 8), intent(in   )      :: gamma
        real(kind = 8), intent(in   )      :: beta           
        real(wp), intent(  out)      :: Acceleration(1:3)         

        real(kind = 8)      :: REarth=6.371393E+6_wp, Vlight=2.99792458E+8_wp
        real(kind = 8)      :: MU=0.3986004415E+15_wp
        real(wp)      :: JEarth(3) = (/ 0.0_wp, 0.0_wp, 9.8E+8_wp /)

        real(kind = 8)      :: beta_1(3)
        real(kind = 8)      :: gamma_1(3), gamma_2(3)
        real(kind = 8)      :: const(3)
        real(wp)      :: coe1, coe2(3), coe3(3)
        real(kind = 8)      :: V, R
        integer(kind = 4)   :: i

        V = 0
        R = 0
        do i=1,3
            V=sqrt(V**2+Velocity(i)**2)
            R=sqrt(R**2+Coordinate(i)**2)
        end do

        coe1 = MU / ((R**3) * (Vlight**2))

        beta_1 = (coe1 * 2 * MU / R ) *  Coordinate

        gamma_1 = coe1 * ( (2 * MU / R - V**2) * Coordinate & 
                      + 2 * dot_product(Coordinate, Velocity) * Velocity ) 
    
        call cross_product(Velocity, JEarth, coe2)
        call cross_product(Coordinate, Velocity, coe3)
        gamma_2 = coe1 * ( (3 * dot_product(Coordinate, JEarth) / R**2 ) * coe3 + coe2 )
        ! gamma_2 = 0
        const = coe1 * 2 * dot_product(Coordinate, Velocity) * Velocity + gamma_2

        Acceleration = beta_1 * beta + (gamma_1 + gamma_2) * gamma + const

    end subroutine RelativisticEffect

    subroutine cross_product(a, b, c)
        use ddeabm_kinds
        implicit none 
        real(wp)    ::a(3), b(3), c(3)
    
        c(1) = a(2) * b(3) - a(3) * b(2)
        c(2) = a(3) * b(1) - a(1) * b(3)
        c(3) = a(1) * b(2) - a(2) * b(1)
    
    end subroutine
    
    


    subroutine Process(C, S, degree, position, n1, n2, V)
        use ddeabm_kinds
        implicit none
        real(wp)   , intent(in   ) :: position(3)  ! positions in the earth-fixed system
        real(wp)   , intent(  out) :: V            ! gravitional v
        integer(kind = 4), intent(in   ) :: degree       ! the max degree fo the stocks coefficients
        integer(kind = 4), intent(in   ) :: n1, n2       ! the degree span
        real(wp)   , intent(in   ) :: C(0: n2, 0: n2), S(0: n2, 0: n2)

        real(wp), allocatable           :: E(:, :), F(:, :)
        real(wp)                        :: x, y, z

        allocate(e(0: degree, 0: degree), f(0: degree, 0: degree))

        x = position(1)
        y = position(2)
        z = position(3)


        Call CalculateEF(x, y, z, degree, E, F)
        Call CalculateV(E, F, degree, n1, n2, V, c, s)

        deallocate(e)
        deallocate(f)

    end subroutine Process

    subroutine CalculateV(E, F, degree, n1, n2, V, c, s)
        use ddeabm_kinds
        implicit none

        integer(kind = 4), intent(in   )             :: n1, n2
        integer(kind = 4), intent(in   )             :: degree
        real(wp)   , intent(  out)             :: V
        real(wp)   , intent(in   )             :: E(0: Degree, 0: Degree)
        real(wp)   , intent(in   )             :: F(0: Degree, 0: Degree)
        real(wp)   , intent(in   )             :: s(0: n2, 0: n2)
        real(wp)   , intent(in   )             :: c(0: n2, 0: n2)

        integer(kind = 4)                            :: i, j
        real(kind = 8)                               :: REarth, MU

        REarth = 6.3781363E+6_wp
        MU = 3.986004415000000e+14_wp
        V = 0.0_wp

        do i = n1, n2
            do j = 0, i
                V = V + E(i, j) * C(i, j) + F(i, j) * S(i, j)
            end do
        end do

        V = V * MU / REarth
    end subroutine


    

end module accl_potl_module