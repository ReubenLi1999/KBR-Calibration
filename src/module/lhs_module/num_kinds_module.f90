module num_kinds_module

    use, intrinsic :: iso_fortran_env

    implicit none

    private

    integer, parameter, public :: wp = real128 !! Using "quarble precision" real kinds
    integer, parameter, public :: dp = real64  !! Using "double precision" real kinds
    integer, parameter, public :: ip = int32   !! Integer working precision

end module num_kinds_module