module M06
    use, intrinsic :: iso_fortran_env

    implicit none

    integer,parameter,public    :: rk = real128

    integer,parameter,public    :: ik = int32

    real(rk)                    :: MU_sun = 1.327124400419390E+20
    real(rk)                    :: MU_moon = 4.902801000000000E+12

end module M06