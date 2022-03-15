module fft_mod
    use num_kinds_module
  implicit none
  integer,       parameter :: dpfft=selected_real_kind(15,300)
  real(kind=dpfft), parameter :: pi=3.141592653589793238460_dpfft
contains
 
  ! In place Cooley-Tukey FFT
  recursive subroutine fft(x)
    complex(kind=dpfft), dimension(:), intent(inout)  :: x
    complex(kind=dpfft)                               :: t
    integer(ip)                                        :: N
    integer(ip)                                        :: i
    complex(kind=dpfft), dimension(:), allocatable    :: even, odd
 
    N=size(x)
 
    if(N .le. 1) return
 
    allocate(odd((N+1)/2))
    allocate(even(N/2))
 
    ! divide
    odd =x(1:N:2)
    even=x(2:N:2)
 
    ! conquer
    call fft(odd)
    call fft(even)
 
    ! combine
    do i=1,N/2
       t=exp(cmplx(0.0_dpfft,-2.0_dpfft*pi*real(i-1,dpfft)/real(N,dpfft),kind=dpfft))*even(i)
       x(i)     = odd(i) + t
       x(i+N/2) = odd(i) - t
    end do
 
    deallocate(odd)
    deallocate(even)
 
  end subroutine fft
 
end module fft_mod