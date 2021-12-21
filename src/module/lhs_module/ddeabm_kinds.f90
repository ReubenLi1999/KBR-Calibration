!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/22/2015
!  license: BSD
!
!  Numeric kind definitions.

module ddeabm_kinds

    use, intrinsic :: iso_fortran_env

    implicit none

    private

    integer, parameter, public :: wp = real128  !! Using "double precision" real kinds
    integer, PARAMETER, public :: ip = int32

end module ddeabm_kinds
!*****************************************************************************************

