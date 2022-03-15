module M04
    use, intrinsic :: iso_fortran_env

    implicit none

    integer,parameter,public    :: rk = real128

    integer,parameter,public    :: ik = int32

    integer(ik)                 :: n_tidal_k20, n_tidal_k21, n_tidal_k22
    
    real(rk),allocatable        :: delaunay_k20(:,:)
    real(rk),allocatable        :: delaunay_k21(:,:)
    real(rk),allocatable        :: delaunay_k22(:,:)
    real(rk),allocatable        :: Amp_k20(:,:)
    real(rk),allocatable        :: Amp_k21(:,:)
    real(rk),allocatable        :: Amp_k22(:)
    real(rk),allocatable        :: TA20(:)
    real(rk),allocatable        :: TA21(:)
    real(rk),allocatable        :: TA22(:)

    real(rk)                    :: GM_SUN = 1.327124400419390D20
    real(rk)                    :: GM_MOON=4.902801000000000D12
    real(rk)                    :: GM_E=3.986004415000000e+14   
    real(rk)                    :: R_E=6.3781363D6

    real(rk)                    :: A0= 4.4228D-8
    real(rk)                    :: k20 = 0.29525D0
    real(rk)                    :: k20_plus = -0.00087D0
    real(rk)                    :: k21 = 0.2947D0
    real(rk)                    :: K21_plus = -0.00079
    real(rk)                    :: k22 = 0.29801D0
    real(rk)                    :: K22_plus = -0.00057D0
    real(rk)                    :: K30 = 0.093D0
    real(rk)                    :: K31 = 0.093D0
    real(rk)                    :: K32 = 0.093D0

    integer(ik), allocatable    :: segment(:, :)
    integer(ik)                 :: number_segment
    
end module M04