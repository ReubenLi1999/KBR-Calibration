module M05
    use, intrinsic :: iso_fortran_env

    implicit none

    integer,parameter,public  :: rk = real64

    integer,parameter,public  :: ik = int32

    integer(ik), allocatable  :: segment(:, :)
    integer(ik)               :: number_segment
    
    real(rk)                  :: Omega(3) = [0.0_rk, 0.0_rk, 7.2921158553e-5_rk]
end module M05