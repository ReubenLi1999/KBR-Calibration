module phase_centre_vad
    
    use io_file_module
    use fpl
    use num_kinds_module
    use logger_mod, only: logger_init, logger => master_logger
    use ddeabm_module
    use accl_potl_module
    use xml_module
    use FHDICT
    use random_integer_module
    use math_collection_module
    use FFT_Mod_fcoder
    use bspline_module
    use, intrinsic :: iso_c_binding
    use, intrinsic :: iso_fortran_env, only: output_unit
    !use ogpf
    
    implicit none
    !include 'fftw3.f03'
    ! include 'fftw/fftw3.f'
    
    type, public :: kbr1a_data
        real(kind=wp)                                        :: gpst_intg
        !> the gps time in integer in the KBR1A file product
        real(kind=wp)                                        :: gpst_frac
        !> the gps time in fraction in the KBR1A file product
    end type kbr1a_data
    
    type, public :: khk1a_data
        real(kind=wp)                                        :: gpst_intg
        !> the gps time in integer in the KHK1A file product
        real(kind=wp)                                        :: gpst_frac
        !> the gps time in fraction in the KHK1A file product
    end type khk1a_data
    
    type, public :: khk1b_data
        real(kind=wp)                                        :: gpst_intg
        !> the gps time in integer in the KHK1B file product
        real(kind=wp)                                        :: gpst_frac
        !> the gps time in fraction in the KHK1B file prodct
    end type khk1b_data
    
    type, public :: tha1b_data
        real(kind=wp)                                        :: gpst_intg
        !> the gps time in integer in the THA1B file product
        real(kind=wp)                                        :: gpst_frac
        !> the gps time in fraction in the THA1B file product
        real(kind=wp)                                        :: gpst
        !> equals to gpst_intg + gpst_frac
        real(kind=wp)                                        :: thruster_time(12)
        !> the thrustering time for each thruster (all 12 thrusters), unit = ms, should be 0~1000 ms
        real(kind=wp)                                        :: accum_time
        !> the accumulated time for all the thrusters
    end type tha1b_data
    
    !> all of the following quatities are in the gpts time tag for KBR1B
    type, public :: kbr1b_data 
        real(kind=wp)                                        :: gpst_kbr1b
        !> the gps time in the KBR1B file product
        real(kind=wp)                                        :: range
        !> the biased range in the KBR1B file product
        real(kind=wp)                                        :: range_rate
        !> the biased range rate in the KBR1B file product
        real(kind=wp)                                        :: range_accl
        !> the biased range acceleration in the KBR1B file product
        real(kind=wp)                                        :: tof_range
        !> the time-of-flight correction in the KBR1B file product
        real(kind=wp)                                        :: ant_range
        !> the antenna centre correction in the KBR1B file product
        real(kind=wp)                                        :: tof_rate
        !> the time-of-flight correction rate in the KBR1B file product
        real(kind=wp)                                        :: tof_accl
        !> the time-of-flight correction acceleration in the KBR1B file product
        real(kind=wp)                                        :: iono_corr
        !> the ionospherical correction in the KBR1B file product
        real(kind=wp)                                        :: pos_i_kbr(3)
        !> the position vector for kbr computing in the inertial frame
        real(kind=wp)                                        :: pos_i_lead(3)
        !> the position vector for the leading satellite in the inertial frame
        real(kind=wp)                                        :: pos_i_trac(3)
        !> the position vector for the tracking satellite in the inertial frame
        real(kind=wp)                                        :: pos_e_lead(3)
        !> the position vector for the leading satellite in the earth-fixed frame
        real(kind=wp)                                        :: pos_e_trac(3)
        !> the position vector for the tracking satellite in the earth-fixed frame
        real(kind=wp)                                        :: wp_acc_grav_lead_e(3)
        !> gravitational acceleration for the leading satellite in the earth-fixed frame
        real(kind=wp)                                        :: wp_acc_grav_trac_e(3)
        !> gravitational acceleration for the tracking satellite in the earth-fixed frame
        real(kind=wp)                                        :: range_simu
        !> the simulated range corresponding to the biased range, computed from the forward subroutine
        real(kind=wp)                                        :: range_meas
        !> the range measured by GPS or Beidou
        real(kind=wp)                                        :: range_resi
        !> the residual of the biased range and the simulated range
        real(kind=wp)                                        :: los_l2t(3)
        !> the line-of-sight vector in the inertial frame from the leading satellite to the tracking one
        real(kind=wp)                                        :: los_t2l(3)
        !> the line-of-sighr vector in the inertial frame from the tracking satellite to the leading one
        real(kind=wp)                                        :: quaternion_c(4)
        !> the rotation quaternion from the inertial frame to SRF for the leading satellite
        real(kind=wp)                                        :: quaternion_d(4)
        !> the rotation quaternion from the inertial frame to SRF for the tracking satellite
        real(kind=wp)                                        :: rotm_c_i2s(3, 3)
        !> the rotation matrix from the inertial frame to SRF for the leading satellite
        real(kind=wp)                                        :: rotm_c_s2i(3, 3)
        !> the rotation matrix from SRF to the inertial frame for the leading satellite
        real(kind=wp)                                        :: rotm_d_s2i(3, 3)
        !> the rotation matrix from SRF to the inertial frame for the tracking satellite
        real(kind=wp)                                        :: rotm_d_i2s(3, 3)
        !> the rotation matrix from the inertial frame to SRF for the tracking satellite
        real(kind=wp)                                        :: eq_a(6)
        !> the model matrix for the equation system
        real(kind=wp)                                        :: eq_b
        !> the information vector for the equation system
        real(kind=wp)                                        :: ant_phase_corr_simu
        !> the simulated antenna phase correction
        real(kind=wp)                                        :: range_pod
        !> the inter-satellite range calculated from POD data
        real(kind=wp)                                        :: wp_rotm_c_losf2irf(3, 3)
        !> rotation matrix from line-of-sight frame to inertial frame for the leading satellite
        real(kind=wp)                                        :: wp_rotm_c_irf2losf(3, 3)
        !> rotation matrix from inertial frame to line-of-sight frame for the leading satellite
        real(kind=wp)                                        :: wp_rotm_d_losf2irf(3, 3)
        !> rotation matrix from line-of-sight frame to inertial frame for the tracking satellite
        real(kind=wp)                                        :: wp_rotm_d_irf2losf(3, 3)
        !> rotation matrix from inertial frame to line-of-sight frame for the tracking satellite
        real(kind=wp)                                        :: wp_eul_c_srf2losf(3)
        !> euler angle from srf to losf
        real(kind=wp)                                        :: wp_eul_d_srf2losf(3)
        !> euler angle from losf to srf
        real(kind=wp)                                        :: wp_multipath_error
    end type kbr1b_data

    type, public :: gps1b_data
        real(kind=wp)                                        :: gpst_gps1b
        !> the gps time for the GNI1B file product
        real(kind=wp)                                        :: utct_gps1b
        !> the utc time for the GNI1B file product corresponding to the utct
        real(kind=wp)                                        :: jdtt_gps1b
        !> the julian day tt for the GNI1B file product corresponding to the gpst
        real(kind=wp)                                        :: pos_i(3)
        !> the position vector in the inertial frame
        real(kind=wp)                                        :: vel_i(3)
        !> the velocity vector in the inertial frame
        real(kind=wp)                                        :: pos_e(3)
        !> the position vector in the earth-fixed frame
        real(kind=wp)                                        :: vel_e(3)
        !> the velocity vector in the earth-fixed frame
        real(kind=wp)                                        :: range
        !> pod range
    end type gps1b_data

    type, public :: sca1b_data
        real(kind=wp)                                        :: gpst_sca1b
        !> the gps time for the SCA1B file product
        real(kind=wp)                                        :: quaternion(4)
        !> the rotation quaternion from the inertial frame to SRF
        real(kind=wp)                                        :: rotm_i2s(3, 3)
        !> the rotation matrix from the inertial frame to SRF
        real(kind=wp)                                        :: rotm_s2i(3, 3)
        !> the rotation matrix from SRF to the inertial frame
    end type sca1b_data

    type, public :: acc1b_data
        real(kind=wp)                                        :: gpst_acc1b
        !> the gps time for the ACC1B file product
        real(kind=wp)                                        :: jdtt
        !> the julian day tt for the ACC1B file product
        real(kind=wp)                                        :: non_grav_acc(3)
        !> the non-gravitational acceleration in the inertial frame
    end type acc1b_data

    type, public :: Stocks_coeffs
        real(kind=wp), ALLOCATABLE                           :: c_coeffs(:, :), c_coeffs_1d(:)
        !> the spherical coefficients C
        real(kind=wp), ALLOCATABLE                           :: s_coeffs(:, :), s_coeffs_1d(:)
        !> the spherical coefficients S
    end type Stocks_coeffs

    type, public :: satellite
        type(kbr1b_data), allocatable                        :: kbr1b_both(:), kbr1b_2degdiff(:), kbr1b_3degdiff(:)
        !> kbr1b for both satellites
        type(gps1b_data), allocatable                        :: gps1b_lead(:)
        !> trajectory data in the inertial frame of 1B-level for the leading satellite
        type(gps1b_data), allocatable                        :: gps1b_trac(:)
        !> trajectory data in the inertial frame of 1B-level for the tracking satellite
        type(sca1b_data), allocatable                        :: sca1b_lead(:)
        !> altitude data from GCRS to SRF of 1B-level for the leading satellite
        type(sca1b_data), allocatable                        :: sca1b_trac(:)
        !> altitude data from GCRS to SRF of 1B-level for the tracking satellite
        type(acc1b_data), ALLOCATABLE                        :: acc1b_lead(:)
        !> non-gravitational acceleration of 1B-level in the SRF for the leading satellite
        type(acc1b_data), ALLOCATABLE                        :: acc1b_trac(:)
        !> non-gravitational acceleration of 1B-level in the SRF for the tracking satellite
        type(tha1b_data), ALLOCATABLE                        :: tha1b_lead(:)
        !> thruster events for the leading satellite of 1B-level
        type(tha1b_data), ALLOCATABLE                        :: tha1b_trac(:)
        !> thruster events for the tracking satellite of 1B-level
        type(kbr1a_data), ALLOCATABLE                        :: kbr1a_lead(:)
        !> kbr1a for the leading satellite
        type(kbr1a_data), ALLOCATABLE                        :: kbr1a_trac(:)
        !> kbr1a for the tracking satellite
        type(khk1a_data), ALLOCATABLE                        :: khk1a_lead(:)
        !> kbr house-keeping data of 1A-level for the leading satellite
        type(khk1a_data), ALLOCATABLE                        :: khk1a_trac(:)
        !> kbr house-keeping data of 1A-level for the tracking satellite
        TYPE(khk1b_data), ALLOCATABLE                        :: khk1b_lead(:)
        !> kbr house-keeping data of 1B-level for the leading satellite
        type(khk1b_data), ALLOCATABLE                        :: khk1b_trac(:)
        !> kbr house-keeping data of 1B-level for the tracking satelllite
        type(Stocks_coeffs)                                  :: cs_coeffs
        !> initial stocks coefficients
        real(kind=wp)                                        :: initial_vector(6)
        !> gkb1b file data, indicating the equivalent phase centre vector validated on the ground
        real(kind=wp)                                        :: initial_vector_k(6)
        !> gkb1b file data, indicating the phase centre vector for the K band microwave
        real(kind=wp)                                        :: initial_vector_ka(6)
        !> gkb1b file data, indicating the phase centre vector for the Ka band microwave
        real(kind=wp)                                        :: inverse_vector_expe(6)
        !> the phase centre vector validated in-orbit
        real(kind=wp)                                        :: inverse_vector(4, 6), inverse_vector_n(4, 6)
        !> calibration res using 4 different methods
        real(kind=wp)                                        :: error(2)
        !> error for the inverse problem
        CHARACTER(len=1000)                                  :: dir
        !> reserved, the directory is not used anymore, 2021-11-08
        real(kind=wp)                                        :: bf_lead(3), af_lead(3)
        !> the calibration parametres of the leading satellite
        real(kind=wp)                                        :: bf_trac(3), af_trac(3)
        !> the calibration parametres of the tracking satellite
        real(kind=wp)                                        :: wp_man_period
        !> the actual period of a single sub-manuever
        real(kind=wp)                                        :: wp_amp_freq(2, 4)
        !> amplitude of the fourier component at signal frequency
        real(kind=wp)                                        :: wp_std(6)
        !> standard deviation of the considered calibrated results 
    contains
        procedure, NON_OVERRIDABLE, public                   :: initialise                       => initialise
        !> initialise the class
        procedure, NON_OVERRIDABLE, public                   :: destructor                       => destructor
        !> destruct the class
        ! procedure, NON_OVERRIDABLE, PUBLIC                   :: trajectory_forward               => trajectory_forward
        !> subroutine to perform the forward problem for certain satellite's tracjetory
        procedure, NON_OVERRIDABLE, PUBLIC                   :: create_phase_centre_vad_eq       => create_phase_centre_vad_eq
        !> using SCA1B, GNI1B, KBR1B and some other data to create the equation for solving the antenna offset vector
        procedure, NON_OVERRIDABLE, PUBLIC                   :: solve_phase_centre_vad_eq        => solve_phase_centre_vad_eq
        !> using least-square filtering to solve the problem in the time domain
        ! procedure, NON_OVERRIDABLE, PUBLIC                   :: solve_phase_centre_vad_eq_freq   => solve_phase_centre_vad_eq_freq
        !> using least-square filtering to solve the problem in the frequency domain
        procedure, NON_OVERRIDABLE, PUBLIC                   :: solve_phase_centre_vad_eq_stitch => solve_phase_centre_vad_eq_stitch
        !> using least-square filtering to solve the problem in the time domain with data stitched
        procedure, NON_OVERRIDABLE, PUBLIC                   :: output_data                      => output_data
        !> write the solved data to file
        procedure, NON_OVERRIDABLE, PUBLIC                   :: simu_ant_phase_corr              => simu_ant_phase_corr
        !> simulate the manuever data with the given a prior antenna offset
        procedure, NON_OVERRIDABLE, PUBLIC                   :: solver_gnss                      => solver_gnss
        !> subroutine to divide different manuevers and then solve the estimation problem based on the gnss data
        procedure, NON_OVERRIDABLE, public                   :: amp_identification               => amp_identification
        !> subroutine to identify the period of the manuever from the SCA data
        procedure, NON_OVERRIDABLE, public                   :: output_rpt                       => output_rpt
        !> subroutine to evaluate the validation and write evaluation to a report
        procedure, NON_OVERRIDABLE, PUBLIC                   :: acc_grav                         => acc_grav
        !> subroutine to calculate the gravitational accelaretion
        procedure, NON_OVERRIDABLE, PUBLIC                   :: solver_dynamics                  => solver_dynamics
        !> in the case that the POD data contains periodic signal with the same period as the maneuver signal, the aforementioned solvers
        !> will collapse.
        procedure, NON_OVERRIDABLE, PUBLIC                   :: assert_maneuver                  => assert_maneuver
        !> make sure the maneuver epoches are at the span
        procedure, NON_OVERRIDABLE, PUBLIC                   :: create_eq_dynamics               => create_eq_dynamics
    end type satellite
    
    type, extends(ddeabm_with_event_class) :: spacecraft
        !> spacecraft propagation type.
        !> extends the [[ddeabm_class]] to include data used in the deriv routine
        real(kind=wp)                                        :: mu     = 0.0_wp
        !> central body gravitational parameter (m3/s2)
        integer(kind=ip)                                     :: fevals = 0
        !> number of function evaluations
        integer(kind=ip)                                     :: degree = 60_ip
        logical                                              :: first  = .true.
        !> first point is being exported
        logical                                              :: w_time = .true.
        real(kind=wp)                                        :: date   = 0.0_wp
        !> MJD of 2012.02.01
        real(kind=wp)                                        :: t0     = 0.0_wp
        !> initial time (sec)
        real(kind=wp)                                        :: tf     = 0
        real(kind=wp)                                        :: dt     = 5.0_wp
        !> output step size (sec)
        real(kind=wp)                                        :: final_state(6) = 0.0_wp
        real(kind=wp), ALLOCATABLE                           :: acc_non_grav_srf(:, :)
        !> the non-gravitational acceleration in the SRF
        real(kind=wp), ALLOCATABLE                           :: acc_non_grav_gcrs(:, :)
        !> the non-gravitational acceleration in the GCRS
    end type spacecraft
    
    type(satellite)                                          :: th, jd(4)
    
    type(spacecraft)                                         :: lead, trac

contains

    subroutine create_eq_dynamics(self, i_index_motiv)
        class(satellite)    , INTENT(INOUT)                  :: self
        integer(kind=ip)    , INTENT(IN   ), OPTIONAL        :: i_index_motiv
        type(hashtable)                                      :: q_scac, q_scad, q_gnia, q_gnib, q_gnva, q_gnvb
        integer(kind=ip)                                     :: i, ios, j, istat
        integer(kind=ip)                                     :: tkbr, tscac, tscad, tgnia, tgnib, tgnva, tgnvb
        real(kind=wp), ALLOCATABLE                           :: q_c(:), q_d(:), i_c(:), i_d(:)
        real(kind=wp)                                        :: range_res_ave
        real(kind=wp)                                        :: temp
        real(kind=wp)                                        :: intersatellite_range(size(self%kbr1b_both))
        character(len=14)                                    :: datenow
        character(len=1)                                     :: c_index_motiv
        !type(pyplot)                                         :: plt
        
        
        datenow = yyyymmddhhmmss()

        !> delete
        !open(unit=588, file='..//output//inertial_trajectory_2019-01-01_C_04.txt', iostat=ios, status="old", action="read")
        !if ( ios /= 0 ) stop "Error opening file name"
        !open(unit=899, file='..//output//inertial_trajectory_2019-01-01_D_04.txt', iostat=ios, status="old", action="read")
        !if ( ios /= 0 ) stop "Error opening file name"
        !read(588, *); read(899, *)
        !do i = 1, size(self%kbr1b_both), 1
        !    read(588, *) temp, self%kbr1b_both(i)%pos_i_lead
        !    read(899, *) temp, self%kbr1b_both(i)%pos_i_trac
        !end do
        !
        !close(unit=588, iostat=ios)
        !if ( ios /= 0 ) stop "Error closing file unit 588"
        !close(unit=899, iostat=ios)
        !if ( ios /= 0 ) stop "Error closing file unit 899"
        !> delete
        
        !open(unit=424, file='..//temp//trajectory'//datenow//'.txt', iostat=ios, status="unknown", action="write")
        !if ( ios /= 0 ) stop "Error opening file name"
        !> calculate the simulated inter-satellite range
        ! simu_range: do i = 1, size(self%kbr1b_both), 1
        !     self%kbr1b_both(i)%range_simu = norm2(self%kbr1b_both(i)%pos_i_lead - self%kbr1b_both(i)%pos_i_trac)
        !     !write(424, '(6f30.15)') self%kbr1b_both(i)%pos_i_lead, self%kbr1b_both(i)%pos_i_trac
        ! end do simu_range

        !>------------------------------------------------------------------------------------------
        !> obtain position vector in the inertial frame
        !> using hash table
        call q_gnva%init(nitems=size(self%gps1b_lead))
        call q_gnvb%init(nitems=size(self%gps1b_trac))

        !> check if the nrows of sca_lead and sca_track are the same
        if (size(self%gps1b_lead) /= size(self%gps1b_trac)) then
            call logger%error('phase_centre_vad', 'the number of lines of two SCA files are different')
            call xml_o%xml2file(1, 'the number of lines of two SCA files are different')
            stop
        end if
        
        put_gnv_value: do i = 1, size(self%gps1b_lead), 1
            tgnva = int(self%gps1b_lead(i)%gpst_gps1b)
            tgnvb = int(self%gps1b_trac(i)%gpst_gps1b)
            call q_gnva%put(key=tgnva, rvals=self%gps1b_lead(i)%pos_e)
            call q_gnvb%put(key=tgnvb, rvals=self%gps1b_trac(i)%pos_e)
        end do put_gnv_value

        create_acc: do i = 1, size(self%kbr1b_both), 1
            !> quaternion to rotation matrix section
            tkbr = int(self%kbr1b_both(i)%gpst_kbr1b)
            if (q_gnva%has_key(tkbr)) then
                !> get position vector in the earth-fixed frame from the hash table
                call q_gnva%get(key=tkbr, rvals=i_c)
                !> assign the position vector to the kbr1b_both
                self%kbr1b_both(i)%pos_e_lead = i_c
                !> get the gravitational acceleration of the earth-fixed frame 
                call accxyz(self%kbr1b_both(i)%pos_e_lead, self%kbr1b_both(i)%wp_acc_grav_lead_e, &
                             60_ip, self%cs_coeffs%c_coeffs, self%cs_coeffs%s_coeffs)
            else
                call logger%error('phase_centre_vad', 'time tags of KBR data and KOE data not compatible')
                call xml_o%xml2file(1, 'time tags of KBR data and KOE data not compatible')
                stop
            end if
            if (q_gnvb%has_key(tkbr)) then
                call q_gnvb%get(key=tkbr, rvals=i_d)
                self%kbr1b_both(i)%pos_e_trac = i_d
                !> get the gravitational acceleration of the earth-fixed frame 
                call accxyz(self%kbr1b_both(i)%pos_e_trac, self%kbr1b_both(i)%wp_acc_grav_trac_e, &
                             60_ip, self%cs_coeffs%c_coeffs, self%cs_coeffs%s_coeffs)
            else
                call logger%error('phase_centre_vad', 'time tags of KBR data and KOE data not compatible')
                call xml_o%xml2file(1, "time tags of KBR data and KOE data not compatible")
                stop
            end if
        end do create_acc

        !>------------------------------------------------------------------------------------------
        !> obtain position vector in the inertial frame
        !> using hash table
        call q_gnia%init(nitems=size(self%gps1b_lead))
        call q_gnib%init(nitems=size(self%gps1b_trac))

        !> check if the nrows of sca_lead and sca_track are the same
        if (size(self%gps1b_lead) /= size(self%gps1b_trac)) then
            call logger%error('phase_centre_vad', 'the number of lines of two SCA files are different')
            call xml_o%xml2file(1, 'the number of lines of two SCA files are different')
            stop
        end if
        
        put_gps_value: do i = 1, size(self%gps1b_lead), 1
            tgnia = int(self%gps1b_lead(i)%gpst_gps1b)
            tgnib = int(self%gps1b_trac(i)%gpst_gps1b)
            call q_gnia%put(key=tgnia, rvals=self%gps1b_lead(i)%pos_i)
            call q_gnib%put(key=tgnib, rvals=self%gps1b_trac(i)%pos_i)
        end do put_gps_value

        create_eqb: do i = 1, size(self%kbr1b_both), 1
            !> quaternion to rotation matrix section
            tkbr = int(self%kbr1b_both(i)%gpst_kbr1b)
            if (q_gnia%has_key(tkbr)) then
                call q_gnia%get(key=tkbr, rvals=i_c)
                self%kbr1b_both(i)%pos_i_lead = i_c
            else
                call logger%error('phase_centre_vad', 'time tags of KBR data and KOE data not compatible')
                call xml_o%xml2file(1, 'time tags of KBR data and KOE data not compatible')
                stop
            end if
            if (q_gnib%has_key(tkbr)) then
                call q_gnib%get(key=tkbr, rvals=i_d)
                self%kbr1b_both(i)%pos_i_trac = i_d
            else
                call logger%error('phase_centre_vad', 'time tags of KBR data and KOE data not compatible')
                call xml_o%xml2file(1, "time tags of KBR data and KOE data not compatible")
                stop
            end if
            self%kbr1b_both(i)%range_pod = norm2(self%kbr1b_both(i)%pos_i_lead - self%kbr1b_both(i)%pos_i_trac)
        end do create_eqb

        !close(424)
        !stop

        !> calculate the residual between the biased range from KBR and the simulated inter-satellite range
        ! self%kbr1b_both%range_resi = -self%kbr1b_both%range + self%kbr1b_both%tof_range + self%kbr1b_both%range_simu
        self%kbr1b_both%range_resi = self%kbr1b_both%range + self%kbr1b_both%tof_range - self%kbr1b_both%range_pod
        ! call plt%initialize()
        ! call plt%add_plot(self%kbr1b_both%gpst_kbr1b, self%kbr1b_both%range_simu, label='$\sin(x)$', linestyle='b-o',markersize=5,linewidth=2, istat=istat)
        ! call plt%savefig('sinx.png', pyfile='sinx.py', ismat=.true., istat=istat)
        ! 
        ! call plt%initialize()
        ! call plt%add_plot(self%kbr1b_both%gpst_kbr1b, self%kbr1b_both%range, label='$\sin(x)$', linestyle='b-o',markersize=5,linewidth=2, ! istat=istat)
        ! call plt%savefig('sinx.png', pyfile='sinx.py', ismat=.true., istat=istat)
        ! 
        ! call plt%initialize()
        ! call plt%add_plot(self%kbr1b_both%gpst_kbr1b, self%kbr1b_both%range_resi, label='$\sin(x)$', linestyle='b-o',markersize=5,linewidth=2, istat=istat)
        ! call plt%savefig('sinx.png', pyfile='sinx.py', ismat=.true., istat=istat)
        ! print *, self%kbr1b_both%range_resi
        ! stop

        !> calculate the line-of-sight vector
        self%kbr1b_both%los_l2t(1) =  self%kbr1b_both%pos_i_lead(1) - self%kbr1b_both%pos_i_trac(1)
        self%kbr1b_both%los_l2t(2) =  self%kbr1b_both%pos_i_lead(2) - self%kbr1b_both%pos_i_trac(2)
        self%kbr1b_both%los_l2t(3) =  self%kbr1b_both%pos_i_lead(3) - self%kbr1b_both%pos_i_trac(3)
        self%kbr1b_both%los_t2l(1) = -self%kbr1b_both%los_l2t(1)
        self%kbr1b_both%los_t2l(2) = -self%kbr1b_both%los_l2t(2)
        self%kbr1b_both%los_t2l(3) = -self%kbr1b_both%los_l2t(3)

        !> calculate the inter-satellite range
        cal_inter_range: do i = 1, size(self%kbr1b_both), 1
            intersatellite_range(i) = norm2(self%kbr1b_both(i)%los_l2t)
            do j = 1, 3, 1
                self%kbr1b_both(i)%los_l2t(j) = self%kbr1b_both(i)%los_l2t(j) / intersatellite_range(i)
                self%kbr1b_both(i)%los_t2l(j) = self%kbr1b_both(i)%los_t2l(j) / intersatellite_range(i)
            end do
        end do cal_inter_range

        !> quaternion to rotation matrix
        !> using hash table
        call q_scac%init(nitems=size(self%sca1b_lead))
        call q_scad%init(nitems=size(self%sca1b_trac))

        !> check if the nrows of sca_lead and sca_track are the same
        if (size(self%sca1b_lead) /= size(self%sca1b_trac)) then
            call logger%error('phase_centre_vad', 'the number of lines of two SCA files are different')
            call xml_o%xml2file(1, 'the number of lines of two SCA files are different')
            stop
        end if
        
        put_sca_value: do i = 1, size(self%sca1b_lead), 1
            tscac = int(self%sca1b_lead(i)%gpst_sca1b)
            tscad = int(self%sca1b_trac(i)%gpst_sca1b)
            call q_scac%put(key=tscac, rvals=self%sca1b_lead(i)%quaternion)
            call q_scad%put(key=tscad, rvals=self%sca1b_trac(i)%quaternion)
        end do put_sca_value

        !> -----------------------------------------------------------------------------------------
        ! open(unit=424, file='..//temp//eq_a_'//datenow//'.txt', iostat=ios, status="unknown", position="append")
        ! if ( ios /= 0 ) stop "Error opening file name"
        !> -----------------------------------------------------------------------------------------
        range_res_ave = 0.0_wp
        create_eq: do i = 1, size(self%kbr1b_both), 1
            !> range residual average section
            range_res_ave = range_res_ave + self%kbr1b_both(i)%range_resi

            !> quaternion to rotation matrix section
            tkbr = int(self%kbr1b_both(i)%gpst_kbr1b)
            if (q_scac%has_key(tkbr)) then
                call q_scac%get(key=tkbr, rvals=q_c)
                self%kbr1b_both(i)%quaternion_c = q_c
                self%kbr1b_both(i)%rotm_c_i2s = q2m(self%kbr1b_both(i)%quaternion_c)
                self%kbr1b_both(i)%rotm_c_s2i = transpose(self%kbr1b_both(i)%rotm_c_i2s)
            else
                call logger%error('phase_centre_vad', 'time tags of KBR data and SCA data not compatible')
                call xml_o%xml2file(1, 'time tags of KBR data and SCA data not compatible')
                stop
            end if
            if (q_scad%has_key(tkbr)) then
                call q_scad%get(key=tkbr, rvals=q_d)
                self%kbr1b_both(i)%quaternion_d = q_d
                self%kbr1b_both(i)%rotm_d_i2s = q2m(self%kbr1b_both(i)%quaternion_d)
                self%kbr1b_both(i)%rotm_d_s2i = transpose(self%kbr1b_both(i)%rotm_d_i2s)
            else
                call logger%error('phase_centre_vad', 'time tags of KBR data and SCA data not compatible')
                call xml_o%xml2file(1, "time tags of KBR data and SCA data not compatible")
                stop
            end if

            !> assign matrix A in Ax=b
            self%kbr1b_both(i)%eq_a(1: 3) = matmul(self%kbr1b_both(i)%los_l2t, self%kbr1b_both(i)%rotm_c_s2i)
            self%kbr1b_both(i)%eq_a(4: 6) = matmul(self%kbr1b_both(i)%los_t2l, self%kbr1b_both(i)%rotm_d_s2i)
            ! print *, self%kbr1b_both(i)%eq_a(1: 3)

            ! write(424, '(10f40.20)') self%kbr1b_both(i)%eq_a
        end do create_eq
        !> -----------------------------------------------------------------------------------------
        ! close(unit=424, iostat=ios)
        ! if ( ios /= 0 ) stop "Error closing file unit 424"
        !> -----------------------------------------------------------------------------------------

        !> simulate the antenna phase correction
        ! call self%simu_ant_phase_corr()

        !> assign array b in Ax=b
        self%kbr1b_both%eq_b = self%kbr1b_both%range_resi


        !>------------------------------------------------------------------------------------------
        !> output eq_b
        ! open(unit=424, file='..//temp//eq_b_'//datenow//'.txt', iostat=ios, status="unknown", position="append")
        ! if ( ios /= 0 ) stop "Error opening file name"
        ! write2file_eq_b: do i = 1, size(self%kbr1b_both), 1
        !     write(424, *) self%kbr1b_both(i)%eq_b
        ! end do write2file_eq_b
        ! close(424)

        !> create equation for 2-degree diff and 3-degree diff
        create_2degdiff_loop: do i = 1, size(self%kbr1b_2degdiff), 1
            self%kbr1b_2degdiff(i)%eq_a = self%kbr1b_both(i + 2)%eq_a + self%kbr1b_both(i)%eq_a - 2.0_wp * self%kbr1b_both(i + 1)%eq_a
            self%kbr1b_2degdiff(i)%eq_b = self%kbr1b_both(i + 2)%eq_b + self%kbr1b_both(i)%eq_b - 2.0_wp * self%kbr1b_both(i + 1)%eq_b
        end do create_2degdiff_loop
        
        create_3degdiff_loop: do i = 1, size(self%kbr1b_3degdiff), 1
            self%kbr1b_3degdiff(i)%eq_a = self%kbr1b_2degdiff(i + 1)%eq_a + self%kbr1b_2degdiff(i)%eq_a
            self%kbr1b_3degdiff(i)%eq_b = self%kbr1b_2degdiff(i + 1)%eq_b + self%kbr1b_2degdiff(i)%eq_b
        end do create_3degdiff_loop
        
        write(c_index_motiv, "(i1)") i_index_motiv
        call logger%info('phase_centre_vad', 'create the equations to be solved successfully of Motivation '//c_index_motiv)

    end subroutine create_eq_dynamics

    subroutine assert_maneuver(self, i_maneuver_time)
        class(satellite)                , intent(inout)      :: self

        integer(kind=ip), DIMENSION(:)  , INTENT(INout)      :: i_maneuver_time

        type(hashtable)                                      :: q_scac
        integer(kind=ip)                                     :: i
        integer(kind=ip)                                     :: tscac, tman, i_mantime(8)
        real(kind=wp), ALLOCATABLE                           :: wp_man(:)

        if (any(i_maneuver_time < self%sca1b_lead(1)%gpst_sca1b)) then
            call logger%error('phase_centre_vad', "Maneuver time error")
            call xml_o%xml2file(1, "Maneuver time error")
            stop
        end if

        if (any(i_maneuver_time > self%sca1b_lead(size(self%sca1b_lead))%gpst_sca1b)) then
            call logger%error('phase_centre_vad', "Maneuver time error")
            call xml_o%xml2file(1, "Maneuver time error")
            stop
        end if

        !> assign maneuver index
        call q_scac%init(nitems=size(self%sca1b_lead))
        
        put_sca_value: do i = 1, size(self%sca1b_lead), 1
            tscac = int(self%sca1b_lead(i)%gpst_sca1b)
            call q_scac%put(key=tscac, rvals=[real(i, wp)])
        end do put_sca_value

        assign_man: do i = 1, size(i_maneuver_time), 1

            !> quaternion to rotation matrix section
            tman = i_maneuver_time(i)
            if (q_scac%has_key(tman)) then
                call q_scac%get(key=tman, rvals=wp_man)
                i_mantime(i) = ceiling(wp_man(1))
            else
                call logger%error('phase_centre_vad', 'Maneuver time error')
                call xml_o%xml2file(1, 'Maneuver time error')
                stop
            end if
        end do assign_man
        i_maneuver_time = i_mantime

    end subroutine assert_maneuver

    subroutine solver_dynamics(self, jd_i, i_index_span, i_mirror)
        class(satellite)     , intent(inout)              :: self
        type(satellite)      , INTENT(INOUT)              :: jd_i(:)
        integer(kind=ip)     , INTENT(IN   )              :: i_index_span(:, :)
        integer(kind=ip)     , intent(in   )              :: i_mirror !< flag indicating mirror maneuver
                                                                      !< if [i_mirror=1] the equation of frequency domain will change its sign

        !> local args
        integer(kind=ip)                                  :: i, err, length_span, j, fplerror, length_span_5
        integer(kind=ip)                                  :: index_span_5(size(i_index_span, 1), size(i_index_span, 2))
        integer(kind=ip)                                  :: index_span(size(i_index_span, 1), size(i_index_span, 2))
        integer(kind=ip)                                  :: nfilter
        CHARACTER(len=:), allocatable                     :: config_path
        real(kind=wp)                                     :: wp_normal_eqa_diff2(4, 4), wp_normal_eqb_diff2(4)
        real(kind=wp)                                     :: wp_normal_eqa_diff3(4, 4), wp_normal_eqb_diff3(4)
        real(kind=wp)                                     :: wp_x(4) = 0.0_wp, wp_temp(2)
        real(kind=wp), allocatable                        :: wp_eqa_diff2(:, :), wp_eqa_diff3(:, :)
        real(kind=wp), allocatable                        :: wp_eqb_diff2(:), wp_eqb_diff3(:)
        real(kind=wp)                                     :: wp_nonsens_eqa(2, 4) !< the amplitude of four non-sensitive axises at
                                                                                  !< signal frequency and doubled signal frequency
        !type(pyplot)                                      :: plt

        !> normal matrix for four algorithms
        wp_normal_eqa_diff2 = 0.0_wp
        wp_normal_eqb_diff2 = 0.0_wp
        wp_normal_eqa_diff3 = 0.0_wp
        wp_normal_eqb_diff3 = 0.0_wp
        !> for jd-1: the motivation is along the yaw direction of the leading satellite (x, y)12
        !> for jd-2: the motivation is along the pitch direction of the leading satellite (x, z)13
        !> for jd-3: the motivation is along the yaw direction of the tracking satellite (x, y)45
        !> for jd-4: the motivation is along the pitch direction of the tracking satellite (x, z)46
        assign_jd_loop: do i = 1, 4, 1
            length_span = index_span(i, 2) - index_span(i, 1) + 1
            length_span_5 = index_span_5(i, 2) - index_span_5(i, 1) + 1
            allocate(jd_i(i)%acc1b_lead(1: length_span_5), stat=err)
            if (err /= 0) print *, "jd(i)%acc1b_lead: Allocation request denied"
            allocate(jd_i(i)%acc1b_trac(1: length_span_5), stat=err)
            if (err /= 0) print *, "jd(i)%acc1b_trac: Allocation request denied"
            allocate(jd_i(i)%kbr1b_both(1: length_span_5), stat=err)
            if (err /= 0) print *, "jd(i)%kbr1b_both: Allocation request denied"
            allocate(jd_i(i)%kbr1b_2degdiff(1: length_span_5 - 2), stat=err)
            if (err /= 0) print *, "jd(i)%kbr1b_2defdiff: Allocation request denied"
            allocate(jd_i(i)%kbr1b_3degdiff(1: length_span_5 - 3), stat=err)
            if (err /= 0) print *, "jd(i)%kbr1b_3degediff: Allocation request denied"
            allocate(jd_i(i)%sca1b_lead(1: length_span), stat=err)
            if (err /= 0) print *, "jd(i)%sca1b_lead: Allocation request denied"
            allocate(jd_i(i)%sca1b_trac(1: length_span), stat=err)
            if (err /= 0) print *, "jd(i)%sca1b_trac: Allocation request denied"
            allocate(jd_i(i)%gps1b_lead(1: length_span), stat=err)
            if (err /= 0) print *, "jd(i)%gps1b_lead: Allocation request denied"
            allocate(jd_i(i)%gps1b_trac(1: length_span), stat=err)
            if (err /= 0) print *, "jd(i)%gps1b_trac: Allocation request denied"

            !> necesary assignment
            jd_i(i)%kbr1b_both = self%kbr1b_both(index_span_5(i, 1): index_span_5(i, 2))
            jd_i(i)%acc1b_lead = self%acc1b_lead(index_span_5(i, 1): index_span_5(i, 2))
            jd_i(i)%acc1b_trac = self%acc1b_trac(index_span_5(i, 1): index_span_5(i, 2))
            jd_i(i)%gps1b_lead = self%gps1b_lead(index_span(i, 1): index_span(i, 2))
            jd_i(i)%gps1b_trac = self%gps1b_trac(index_span(i, 1): index_span(i, 2))
            jd_i(i)%sca1b_lead = self%sca1b_lead(index_span(i, 1): index_span(i, 2))
            jd_i(i)%sca1b_trac = self%sca1b_trac(index_span(i, 1): index_span(i, 2))

            !>--------------------------------------------------------------------------------------
            !> create equations
            call jd_i(i)%create_phase_centre_vad_eq(i)

            allocate(wp_eqa_diff2(size(jd_i(i)%kbr1b_2degdiff), 6), stat=err)
            if (err /= 0) print *, "wp_eqa_diff2: Allocation request denied"
            allocate(wp_eqa_diff3(size(jd_i(i)%kbr1b_3degdiff), 6), stat=err)
            if (err /= 0) print *, "wp_eqa_diff3: Allocation request denied"
            allocate(wp_eqb_diff2(size(jd_i(i)%kbr1b_2degdiff)), stat=err)
            if (err /= 0) print *, "wp_eqb_diff2: Allocation request denied"
            allocate(wp_eqb_diff3(size(jd_i(i)%kbr1b_3degdiff)), stat=err)
            if (err /= 0) print *, "wp_eqb_diff3: Allocation request denied"

            !> high pass filter for the data vector
            !> diff-2
            wp_eqb_diff2 = jd_i(i)%kbr1b_2degdiff%eq_b
            call fir_filter(trim(config_path)//'coeff_band_pass_0.001_0.009_ls.fcf', wp_eqb_diff2, 0.2_wp, &
                            jd_i(i)%kbr1b_2degdiff%eq_b, nfilter)
            !> diff-3
            wp_eqb_diff3 = jd_i(i)%kbr1b_3degdiff%eq_b
            call fir_filter(trim(config_path)//'coeff_band_pass_0.001_0.009_ls.fcf', wp_eqb_diff3, 0.2_wp, &
                            jd_i(i)%kbr1b_3degdiff%eq_b, nfilter)

            !> high pass filter for the model matrix separately
            do j = 1, 6, 1
                !> diff-2
                wp_eqb_diff2 = jd_i(i)%kbr1b_2degdiff%eq_a(j)
                call fir_filter(trim(config_path)//'coeff_band_pass_0.001_0.009_ls.fcf', wp_eqb_diff2, 0.2_wp, &
                           wp_eqa_diff2(:, j), nfilter)
                !> diff-3
                wp_eqb_diff3 = jd_i(i)%kbr1b_3degdiff%eq_a(j)
                call fir_filter(trim(config_path)//'coeff_band_pass_0.001_0.009_ls.fcf', wp_eqb_diff3, 0.2_wp, &
                           wp_eqa_diff3(:, j), nfilter)
            end do

            !> identify the amplitude of the Fourier component at the maneuver frequency and doubled maneuver frequency
            call jd_i(i)%amp_identification(jd_i(i)%kbr1b_2degdiff(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b))%eq_b, &
                                            jd_i(i)%wp_amp_freq(:, 3))
            call jd_i(i)%amp_identification(jd_i(i)%kbr1b_3degdiff(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b))%eq_b, &
                                            jd_i(i)%wp_amp_freq(:, 4))
            
            !> solver for the two algorithms separately in the frequency domain
            select case (i)
                case (1)
                    !> diff2
                    call jd_i(i)%amp_identification(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 1), &
                                                    jd_i(i)%wp_amp_freq(:, 1))
                    call jd_i(i)%amp_identification(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 2), &
                                                    jd_i(i)%wp_amp_freq(:, 2))
                    !> mirror maneuver
                    !> ref: estimation theory for KBR on-board calibration
                    if (i_mirror == 0) then
                        jd_i(i)%wp_amp_freq(2, 2) = -jd_i(i)%wp_amp_freq(2, 2)
                    else
                        jd_i(i)%wp_amp_freq(1, 2) = -jd_i(i)%wp_amp_freq(1, 2)
                    end if!> solve the linear equation system
                    self%inverse_vector(1, 2) = (jd_i(i)%wp_amp_freq(1, 3) - self%initial_vector(1) * jd_i(i)%wp_amp_freq(1, 1)) / jd_i(i)%wp_amp_freq(1, 2)
                    
                    !> diff3
                    call jd_i(i)%amp_identification(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 1), &
                                                    jd_i(i)%wp_amp_freq(:, 1))
                    call jd_i(i)%amp_identification(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 2), &
                                                    jd_i(i)%wp_amp_freq(:, 2))
                    !> mirror maneuver
                    !> ref: estimation theory for KBR on-board calibration
                    if (i_mirror == 0) then
                        jd_i(i)%wp_amp_freq(2, 2) = -jd_i(i)%wp_amp_freq(2, 2)
                    else
                        jd_i(i)%wp_amp_freq(1, 2) = -jd_i(i)%wp_amp_freq(1, 2)
                    end if
                    !> solve the linear equation system
                    self%inverse_vector(2, 2) = (jd_i(i)%wp_amp_freq(1, 4) - self%initial_vector(1) * jd_i(i)%wp_amp_freq(1, 1)) / jd_i(i)%wp_amp_freq(1, 2)
                case (2)
                    !> diff2
                    call jd_i(i)%amp_identification(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 1), &
                                                    jd_i(i)%wp_amp_freq(:, 1))
                    call jd_i(i)%amp_identification(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 3), &
                                                    jd_i(i)%wp_amp_freq(:, 2))
                    !> mirror maneuver
                    !> ref: estimation theory for KBR on-board calibration
                    if (i_mirror == 0) then
                        jd_i(i)%wp_amp_freq(2, 2) = -jd_i(i)%wp_amp_freq(2, 2)
                    else
                        jd_i(i)%wp_amp_freq(1, 2) = -jd_i(i)%wp_amp_freq(1, 2)
                    end if
                    !> solve the linear equation system
                    self%inverse_vector(1, 3) = (jd_i(i)%wp_amp_freq(1, 3) - self%initial_vector(1) * jd_i(i)%wp_amp_freq(1, 1)) / jd_i(i)%wp_amp_freq(1, 2)
                    self%inverse_vector(1, 1) = self%initial_vector(1)
                    !> diff3
                    call jd_i(i)%amp_identification(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 1), &
                                                    jd_i(i)%wp_amp_freq(:, 1))
                    call jd_i(i)%amp_identification(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 3), &
                                                    jd_i(i)%wp_amp_freq(:, 2))
                    !> mirror maneuver
                    !> ref: estimation theory for KBR on-board calibration
                    if (i_mirror == 0) then
                        jd_i(i)%wp_amp_freq(2, 2) = -jd_i(i)%wp_amp_freq(2, 2)
                    else
                        jd_i(i)%wp_amp_freq(1, 2) = -jd_i(i)%wp_amp_freq(1, 2)
                    end if
                    !> solve the linear equation system
                    self%inverse_vector(2, 3) = (jd_i(i)%wp_amp_freq(1, 4) - self%initial_vector(1) * jd_i(i)%wp_amp_freq(1, 1)) / jd_i(i)%wp_amp_freq(1, 2)
                    self%inverse_vector(2, 1) = self%initial_vector(1)
                case (3)
                    !> diff2
                    call jd_i(i)%amp_identification(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 4), &
                                                    jd_i(i)%wp_amp_freq(:, 1))
                    call jd_i(i)%amp_identification(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 5), &
                                                    jd_i(i)%wp_amp_freq(:, 2))
                    !> mirror maneuver
                    !> ref: estimation theory for KBR on-board calibration
                    if (i_mirror == 0) then
                        jd_i(i)%wp_amp_freq(2, 2) = -jd_i(i)%wp_amp_freq(2, 2)
                    else
                        jd_i(i)%wp_amp_freq(1, 2) = -jd_i(i)%wp_amp_freq(1, 2)
                    end if
                    !> solve the linear equation system
                    self%inverse_vector(1, 5) = (jd_i(i)%wp_amp_freq(1, 3) - self%initial_vector(4) * jd_i(i)%wp_amp_freq(1, 1)) / jd_i(i)%wp_amp_freq(1, 2)
                    !> diff3
                    call jd_i(i)%amp_identification(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 4), &
                                                    jd_i(i)%wp_amp_freq(:, 1))
                    call jd_i(i)%amp_identification(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 5), &
                                                    jd_i(i)%wp_amp_freq(:, 2))
                    !> mirror maneuver
                    !> ref: estimation theory for KBR on-board calibration
                    if (i_mirror == 0) then
                        jd_i(i)%wp_amp_freq(2, 2) = -jd_i(i)%wp_amp_freq(2, 2)
                    else
                        jd_i(i)%wp_amp_freq(1, 2) = -jd_i(i)%wp_amp_freq(1, 2)
                    end if
                    !> solve the linear equation system
                    self%inverse_vector(2, 5) = (jd_i(i)%wp_amp_freq(1, 4) - self%initial_vector(4) * jd_i(i)%wp_amp_freq(1, 1)) / jd_i(i)%wp_amp_freq(1, 2)
                case (4)
                    !> diff2
                    call jd_i(i)%amp_identification(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 4), &
                                                    jd_i(i)%wp_amp_freq(:, 1))
                    call jd_i(i)%amp_identification(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 6), &
                                                    jd_i(i)%wp_amp_freq(:, 2))
                    !> mirror maneuver
                    !> ref: estimation theory for KBR on-board calibration
                    if (i_mirror == 0) then
                        jd_i(i)%wp_amp_freq(2, 2) = -jd_i(i)%wp_amp_freq(2, 2)
                    else
                        jd_i(i)%wp_amp_freq(1, 2) = -jd_i(i)%wp_amp_freq(1, 2)
                    end if
                    !> solve the linear equation system
                    self%inverse_vector(1, 6) = (jd_i(i)%wp_amp_freq(1, 3) - self%initial_vector(4) * jd_i(i)%wp_amp_freq(1, 1)) / jd_i(i)%wp_amp_freq(1, 2)
                    self%inverse_vector(1, 4) = self%initial_vector(4)
                    !> diff3
                    call jd_i(i)%amp_identification(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 4), &
                                                    jd_i(i)%wp_amp_freq(:, 1))
                    call jd_i(i)%amp_identification(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 6), &
                                                    jd_i(i)%wp_amp_freq(:, 2))
                    !> mirror maneuver
                    !> ref: estimation theory for KBR on-board calibration
                    if (i_mirror == 0) then
                        jd_i(i)%wp_amp_freq(2, 2) = -jd_i(i)%wp_amp_freq(2, 2)
                    else
                        jd_i(i)%wp_amp_freq(1, 2) = -jd_i(i)%wp_amp_freq(1, 2)
                    end if
                    !> solve the linear equation system
                    self%inverse_vector(2, 6) = (jd_i(i)%wp_amp_freq(1, 4) - self%initial_vector(4) * jd_i(i)%wp_amp_freq(1, 1)) / jd_i(i)%wp_amp_freq(1, 2)
                    self%inverse_vector(2, 4) = self%initial_vector(4)
                case default
            end select

            if (allocated(wp_eqa_diff2)) deallocate(wp_eqa_diff2, stat=err)
            if (err /= 0) print *, "wp_eqa_diff2: Deallocation request denied"
            if (allocated(wp_eqa_diff3)) deallocate(wp_eqa_diff3, stat=err)
            if (err /= 0) print *, "wp_eqa_diff3: Deallocation request denied"
            if (allocated(wp_eqb_diff2)) deallocate(wp_eqb_diff2, stat=err)
            if (err /= 0) print *, "wp_eqb_diff2: Deallocation request denied"
            if (allocated(wp_eqb_diff3)) deallocate(wp_eqb_diff3, stat=err)
            if (err /= 0) print *, "wp_eqb_diff3: Deallocation request denied"
            
            !> deallocate
            if (allocated(jd_i(i)%gps1b_trac)) deallocate(jd_i(i)%gps1b_trac, stat=err)
            if (err /= 0) print *, "jd(i)%gps1b_trac: Deallocation request denied"
            if (allocated(jd_i(i)%gps1b_lead)) deallocate(jd_i(i)%gps1b_lead, stat=err)
            if (err /= 0) print *, "jd(i)%gps1b_lead: Deallocation request denied"
            if (allocated(jd_i(i)%sca1b_trac)) deallocate(jd_i(i)%sca1b_trac, stat=err)
            if (err /= 0) print *, "jd(i)%sca1b_trac: Deallocation request denied"
            if (allocated(jd_i(i)%sca1b_lead)) deallocate(jd_i(i)%sca1b_lead, stat=err)
            if (err /= 0) print *, "jd(i)%sca1b_lead: Deallocation request denied"
            if (allocated(jd_i(i)%kbr1b_both)) deallocate(jd_i(i)%kbr1b_both, stat=err)
            if (err /= 0) print *, "jd(i)%kbr1b_both: Deallocation request denied"
            if (allocated(jd_i(i)%acc1b_lead)) deallocate(jd_i(i)%acc1b_lead, stat=err)
            if (err /= 0) print *, "jd(i)%acc1b_lead: Deallocation request denied"
            if (allocated(jd_i(i)%acc1b_trac)) deallocate(jd_i(i)%acc1b_trac, stat=err)
            if (err /= 0) print *, "jd(i)%acc1b_trac: Deallocation request denied"
            if (allocated(jd_i(i)%kbr1b_2degdiff)) deallocate(jd_i(i)%kbr1b_2degdiff, stat=err)
            if (err /= 0) print *, "jd(i)%kbr1b_2degdiff: Deallocation request denied"
            if (allocated(jd_i(i)%kbr1b_3degdiff)) deallocate(jd_i(i)%kbr1b_3degdiff, stat=err)
            if (err /= 0) print *, "jd(i)%kbr1b_3degdiff: Deallocation request denied"
        end do assign_jd_loop

    end subroutine solver_dynamics

    subroutine acc_grav(self)
        class(satellite)                , INTENT(INOUT)      :: self
        
        integer(kind=ip)                                     :: i, ios
        real(kind=wp)                                        :: wp_acc_lead(3), wp_acc_trac(3)
        
        open(unit=output_unit, file='..//temp//acc_grav_calibra2.txt', iostat=ios, status="unknown", action="write")
        if ( ios /= 0 ) stop "Error opening file name"
        
        do i = 1, size(self%gps1b_lead), 1
            ! call geopot(self%gps1b_lead(i)%pos_e(1), &
            !             self%gps1b_lead(i)%pos_e(2), &
            !             self%gps1b_lead(i)%pos_e(3), &
            !             60_ip, &
            !             60_ip, &
            !             6.3781363E+3_wp, &
            !             398600.4415_wp, &
            !             self%cs_coeffs%c_coeffs_1d, &
            !             self%cs_coeffs%s_coeffs_1d, &
            !             wp_acc_lead(1), &
            !             wp_acc_lead(2), &
            !             wp_acc_lead(3))
            ! call geopot(self%gps1b_trac(i)%pos_e(1), &
            !             self%gps1b_trac(i)%pos_e(2), &
            !             self%gps1b_trac(i)%pos_e(3), &
            !             60_ip, &
            !             60_ip, &
            !             6.3781363E+3_wp, &
            !             398600.4415_wp, &
            !             self%cs_coeffs%c_coeffs_1d, &
            !             self%cs_coeffs%s_coeffs_1d, &
            !             wp_acc_trac(1), &
            !             wp_acc_trac(2), &
            !             wp_acc_trac(3))
            call accxyz(self%gps1b_lead(i)%pos_e, wp_acc_lead, 60_ip, self%cs_coeffs%c_coeffs, self%cs_coeffs%s_coeffs)
            call accxyz(self%gps1b_trac(i)%pos_e, wp_acc_trac, 60_ip, self%cs_coeffs%c_coeffs, self%cs_coeffs%s_coeffs)
            
            write(output_unit, "(6f30.15)") wp_acc_lead, wp_acc_trac
        end do
        
        close(unit=output_unit, iostat=ios)
        if ( ios /= 0 ) stop "Error closing file unit output_unit"
        
    end subroutine acc_grav

    subroutine amp_identification(self, estimated, wp_amp_temp)
        !> -----------------------------------------------------------------------------------------
        !> Description: This subroutine identifies the period of a single sub-manuever. It's common 
        !>              in reality that the length of FFT's input 
        !>              is not the integer multiple of the period, in that case, the FFT spectram of
        !>              a single sub-manuever leaks, in other words, FFT cannot reflect the period
        !>              accurately.
        !>              Provided that a single sub-manuever is a linear time-invariant system, the 
        !>              period does not change during the sub-manuever.
        !> Input: SCA
        !> Output: period of a single sub-manuever
        class(satellite)     , INTENT(INOUT)            :: self
        real(kind=wp)        , INTENT(IN   )            :: estimated(:)
        real(kind=wp)        , intent(  out)            :: wp_amp_temp(:)
        
        !> parameter
        integer(kind=ip), PARAMETER                     :: ip_estimate_freq_len = 101_ip
        real(kind=wp)   , PARAMETER                     :: wp_f_man_nominal = 4.0e-3_wp
        real(kind=wp)   , PARAMETER                     :: wp_f_man_nominal_std = 1.0_wp/250.0_wp - 1.0_wp/255.0_wp
        
        !> temp
        integer(kind=ip)                                :: i, j
        real(kind=wp)                                   :: wp_cs(size(estimated), 4)
        real(kind=wp)                                   :: wp_f_man
        real(kind=wp)                                   :: wp_time(size(estimated))
        real(kind=wp)                                   :: wp_res(6_ip, ip_estimate_freq_len)
        
        
        !> sampling interval is 5 second
        wp_time = arange(1.0_wp, real(size(estimated), wp)*5.0_wp, 5.0_wp)
        wp_res(1, :) = linspace(wp_f_man_nominal - wp_f_man_nominal_std, wp_f_man_nominal + wp_f_man_nominal_std, ip_estimate_freq_len)
        
        !> assign [wp_cs]
        !> the 1st column for sin(2 pi f t)
        !> the 2nd column for cos(2 pi f t)
        !> the 3rd column for sin(4 pi f t)
        !> the 4th column for cos(4 pi f t)
        estimate_amp_at_maneuver_freq_loop: do i = 1, ip_estimate_freq_len, 1
            wp_f_man = wp_res(1, i)
            assign_wo_cs_loop: do j = 1, size(estimated), 1
                wp_cs(j, 1) = sin(2 * pi * wp_f_man * wp_time(j))
                wp_cs(j, 2) = cos(2 * pi * wp_f_man * wp_time(j))
                wp_cs(j, 3) = sin(4 * pi * wp_f_man * wp_time(j))
                wp_cs(j, 4) = cos(4 * pi * wp_f_man * wp_time(j))
            end do assign_wo_cs_loop

            wp_res(2: 5, i) = ls_solver(wp_cs, estimated)
            wp_res(6, i) = norm2(MATMUL(wp_cs, wp_res(2: 5, i)) - estimated)
            
        end do estimate_amp_at_maneuver_freq_loop
        
        !> amplitude from sine component and cosine component
        wp_amp_temp(1) = norm2(wp_res(2: 3, minloc(wp_res(6, :), mask=wp_res(6, :)>0)))
        wp_amp_temp(2) = norm2(wp_res(4: 5, minloc(wp_res(6, :), mask=wp_res(6, :)>0)))
        
    end subroutine amp_identification

    subroutine solve_phase_centre_vad_eq_stitch(self, motor_id)
        class(satellite)     , INTENT(INOUT)            :: self
            
        !> input argument 
        integer(kind=ip)     , intent(in   )            :: motor_id
            
        !> temp 
        !type(pyplot)                                    :: plt
        real(kind=wp), allocatable                      :: a_valid(:, :)
        real(kind=wp), allocatable                      :: b_valid(:)
        real(kind=wp), allocatable                      :: a_valid_5times(:, :)
        real(kind=wp), allocatable                      :: b_valid_5times(:)
        real(kind=wp), allocatable                      :: eqa_stitched(:, :)
        real(kind=wp), allocatable                      :: eqb_stitched(:)
        real(kind=wp)                                   :: wp_temp1(size(self%kbr1b_both))
        real(kind=wp)                                   :: wp_temp2(size(self%kbr1b_both))
        real(kind=wp)                                   :: wp_temp
        integer(kind=ip)                                :: ip_start_epoches(4)
        integer(kind=ip)                                :: fplerror, err, ios, i, j, sflag
        integer(kind=ip)                                :: nfilter
        CHARACTER(len=:), allocatable                   :: config_path
        type(bspline_1d)                                :: s
        
        !>------------------------------------------------------------------------------------------
        !> raw filter
        !> band pass filter filename
        fplerror = xml_i%urlpaths%GetAsString(key='ConfigPath', string=config_path)
        !> band pass filter
        wp_temp1 = self%kbr1b_both%eq_b
        call fir_filter(trim(config_path)//'coeff_band_pass_0.001_0.009_ls.fcf', wp_temp1, 0.2_wp, &
                        wp_temp2, nfilter)
        self%kbr1b_both%eq_b = wp_temp2
        
        !> assign the valid values of a and b
        allocate(a_valid(1: -nfilter + size(self%kbr1b_both%eq_b), 6), stat=err)
        if (err /= 0) print *, "a_valid: Allocation request denied"
        allocate(b_valid(1: -nfilter + size(self%kbr1b_both%eq_b)), stat=err)
        if (err /= 0) print *, "b_valid: Allocation request denied"
        allocate(a_valid_5times(1: (1 - nfilter + size(self%kbr1b_both%eq_b)) * 5, 6), stat=err)
        if (err /= 0) print *, "a_valid_5times: Allocation request denied"
        allocate(b_valid_5times(1: (1 - nfilter + size(self%kbr1b_both%eq_b)) * 5), stat=err)
        if (err /= 0) print *, "b_valid_5times: Allocation request denied"
        
        !> extract the useful information from filtered time series, in other words, to uretract the
        !> phase delay
        do i = 1, 6, 1
            a_valid(:, i) = self%kbr1b_both(floor((nfilter + 1.0_wp) / 2.0_wp): &
                            size(self%kbr1b_both%eq_b) - floor((nfilter - 1.0_wp) / 2.0_wp))%eq_a(i)
        end do 
        b_valid = self%kbr1b_both(nfilter: size(self%kbr1b_both%eq_b))%eq_b & 
                + sum(self%kbr1b_both(nfilter: size(self%kbr1b_both%eq_b))%ant_phase_corr_simu - &
                    self%kbr1b_both(nfilter: size(self%kbr1b_both%eq_b))%eq_b) &
                    / size(self%kbr1b_both(nfilter: size(self%kbr1b_both%eq_b)))
        
        !> copy the model matrix and the data vector 5 times
        repmat_a_valid_5times_loop: do i = 1, 6, 1
            a_valid_5times(:, i) = [a_valid(:, i), a_valid(:, i), a_valid(:, i), a_valid(:, i), a_valid(:, i)]
        end do repmat_a_valid_5times_loop
        b_valid_5times = [b_valid, b_valid, b_valid, b_valid, b_valid]
        !>------------------------------------------------------------------------------------------
        
        !>------------------------------------------------------------------------------------------
        !> indicate the epoches that should be corrected
        ip_start_epoches = [(i * size(b_valid) + 1, i = 1, size(ip_start_epoches), 1)]
        !> stitch the data
        stitch_loop: do i = 1, size(ip_start_epoches), 1
            !> extrapolate using bspline interpolation method
            call s%initialize([(real(j, wp), j = 1, ip_start_epoches(i) - 1, 1)], &
                              b_valid_5times(1: ip_start_epoches(i) - 1), &
                              4, &
                              sflag, &
                              .true.)
            call s%evaluate(real(ip_start_epoches(i), wp), 0, wp_temp, sflag)
            call s%destroy()
            
            b_valid_5times(ip_start_epoches(i): size(b_valid_5times)) = b_valid_5times(ip_start_epoches(i): size(b_valid_5times)) &
                                                                        - b_valid_5times(ip_start_epoches(i)) + wp_temp
        end do stitch_loop
        !>------------------------------------------------------------------------------------------
        
        if (allocated(b_valid)) deallocate(b_valid, stat=err)
        if (err /= 0) print *, "b_valid: Deallocation request denied"
        if (allocated(a_valid)) deallocate(a_valid, stat=err)
        if (err /= 0) print *, "a_valid: Deallocation request denied"
        
        !>------------------------------------------------------------------------------------------
        !> second filter
        allocate(eqa_stitched(1: (1 - nfilter + size(self%kbr1b_both%eq_b)) * 5, 6), stat=err)
        if (err /= 0) print *, "eqa_stitched: Allocation request denied"
        allocate(eqb_stitched(1: (1 - nfilter + size(self%kbr1b_both%eq_b)) * 5), stat=err)
        if (err /= 0) print *, "eqb_stitched: Allocation request denied"
        
        wp_temp1 = b_valid_5times
        call fir_filter(trim(config_path)//'coeff_band_pass_0.001_0.009_ls.fcf', wp_temp1, 0.2_wp, &
                        wp_temp2, nfilter)
        
        !> extract the useful information from filtered time series, in other words, to uretract the
        !> phase delay
        do i = 1, 6, 1
            eqa_stitched(:, i) = a_valid_5times(floor((nfilter + 1.0_wp) / 2.0_wp): &
                            size(self%kbr1b_both%eq_b) - floor((nfilter - 1.0_wp) / 2.0_wp), i)
        end do 
        eqb_stitched = b_valid_5times(nfilter: size(b_valid_5times)) & 
                + sum(self%kbr1b_both(nfilter: size(self%kbr1b_both%eq_b))%ant_phase_corr_simu - &
                    b_valid_5times(nfilter: size(self%kbr1b_both%eq_b))) &
                    / size(b_valid_5times(nfilter: size(b_valid_5times)))
        
        if (allocated(b_valid_5times)) deallocate(b_valid_5times, stat=err)
        if (err /= 0) print *, "b_valid_5times: Deallocation request denied"
        if (allocated(a_valid_5times)) deallocate(a_valid_5times, stat=err)
        if (err /= 0) print *, "a_valid_5times: Deallocation request denied"
        
        if (allocated(eqb_stitched)) deallocate(eqb_stitched, stat=err)
        if (err /= 0) print *, "eqb_stitched: Deallocation request denied"
        if (allocated(eqa_stitched)) deallocate(eqa_stitched, stat=err)
        if (err /= 0) print *, "eqa_stitched: Deallocation request denied"
        stop
        
        
    end subroutine solve_phase_centre_vad_eq_stitch

    ! subroutine solve_phase_centre_vad_eq_freq(self, motor_id)
    !     class(satellite)     , INTENT(INOUT)              :: self
    !     !> input variables
    !     integer(kind=ip)          , INTENT(IN   )         :: motor_id
    !     
    !     complex(dpfft)                                    :: data_fft(size(self%kbr1b_both)), temp(8)
    !     real(kind=wp)                                     :: amp(size(self%kbr1b_both) / 2), freq(size! (self%kbr1b_both) / 2)
    !     real(kind=wp)                                     :: model_matrix(2, 2), data_vector(2), res(2)
    !     integer(kind=ip)                                  :: i, fwd = 0, index_f1, index_f2, istat
    !     type(c_ptr)                                       :: plan
    !     real(c_double)                                    :: input(size(self%kbr1b_both))
    !     COMPLEX(c_double_complex)                         :: output(size(self%kbr1b_both) / 2 + 1)
    !     type(pyplot)                                      :: plt
    !     
    !     
    !     !> fft the data vector
    !     data_fft = self%kbr1b_both%eq_b
    !     !> fft using FFTW
    !     !> the result in the frequency domain is the same as one calculated from MATLAB,
    !     !>------------------------------------------------------------------------------------------
    !     !> Note that result calculated by codes from Google and Gitub can not reach the standard or
    !     !> they are false to be frank. Because the algorithm they use is radix-2 one, and the
    !     !> algorithm used by FFTW can handle the arbitary-length input data problem.
    !     !>------------------------------------------------------------------------------------------
    !     temp = [1.0_dpfft, 1.0_dpfft, 1.0_dpfft, 1.0_dpfft, 0.0_dpfft, 0.0_dpfft, 0.0_dpfft, 0.0_dpfft]
    !     !> linux fftw using MKL
    !     !plan = fftw_plan_dft_r2c_1d(size(input), input, output, FFTW_ESTIMATE+FFTW_UNALIGNED)
    !     !call fftw_execute_dft_r2c(plan, input, output)
    ! 
    !     !> windows fftw using MKL
    !     !call dfftw_plan_dft_1d(fwd, size(data_fft), data_fft, data_fft, FFT_Forward, FFTW_ESTIMATE)
    !     !call dfftw_execute(fwd)
    !     !call dfftw_plan_dft_1d(fwd, 8, temp, temp, FFT_Forward, FFTW_ESTIMATE)
    !     !call dfftw_execute(fwd)
    !     
    !     !print *, temp
    !     !stop
    !     amp = 2.0_wp / real(size(data_fft), wp) * abs(data_fft)
    !     freq = 0.2_wp / real(size(self%kbr1b_both), wp) * arange(0.0_wp, 100.0_wp, 1.0_wp)
    !     call plt%initialize()
    !     call plt%add_plot(freq, amp, xscale='log', yscale='log', label='$\sin(x)$',istat=istat, ! linestyle='-')
    !     call plt%savefig('sinx.png', pyfile='sinx.py', ismat=.true., istat=istat)
    !     !> search the frequency domain to extract the amplitude at 1/250Hz and 2/250Hz
    !     index_f1 = 1_ip
    !     index_f2 = 1_ip
    !     extract_amp_loop: do i = 1, size(amp), 1
    !         if (isequal(freq(i), 1.0_wp / 250.0_wp)) index_f1 = i
    !         if (isequal(freq(i), 2.0_wp / 250.0_wp)) index_f2 = i
    !     end do extract_amp_loop
    ! 
    !     !> plot session
    !     ! call plt%title("The 1st motivation (1: 200)")
    !     ! call plt%xlabel("Frequency [Hz]")
    !     ! call plt%ylabel("Amplitude [m]")
    !     ! call plt%options("set grid xtics ytics mxtics")
    !     ! call plt%loglog(freq, amp)
    !     
    !     !> Semi-analytic method to solve the linear inverse problem Ax=b
    !     !> 1st step: create the model matrix A
    !     model_matrix(1, 1) =  (deg2rad(1.0_wp)**2 / 4.0_wp * cos(deg2rad(2.0_wp)))
    !     model_matrix(1, 2) = -(deg2rad(1.0_wp)**2 / 4.0_wp * sin(deg2rad(2.0_wp)))
    !     model_matrix(2, 1) =   deg2rad(1.0_wp) * sin(deg2rad(2.0_wp))
    !     model_matrix(2, 2) =   deg2rad(1.0_wp) * cos(deg2rad(2.0_wp))
    !     
    !     !> 2nd step: create the date vector
    !     data_vector(1) = amp(index_f2); data_vector(2) = amp(index_f1)
    !     
    !     !> 3rd step: solve this linear inverse problem
    !     res = ls_solver(model_matrix, data_vector)
    !     !print *, data_vector
    !     !print *, "--"
    !     !print *, model_matrix
    !     !print *, "--"
    !     print *, res
    !     !stop
    !     
    ! end subroutine solve_phase_centre_vad_eq_freq

    subroutine solver_gnss(self, jd_i, i_index_span, i_mirror)
        class(satellite)     , intent(inout)              :: self
        type(satellite)      , INTENT(INOUT)              :: jd_i(:)
        integer(kind=ip)     , INTENT(IN   )              :: i_index_span(:, :)
        integer(kind=ip)     , intent(in   )              :: i_mirror !< flag indicating mirror maneuver
                                                                      !< if [i_mirror=1] the equation of frequency domain will change its sign
        
        !> reg
        integer(kind=ip)                                  :: i, err, length_span, j, fplerror, length_span_5
        integer(kind=ip)                                  :: index_span_5(size(i_index_span, 1), size(i_index_span, 2))
        integer(kind=ip)                                  :: index_span(size(i_index_span, 1), size(i_index_span, 2))
        integer(kind=ip)                                  :: nfilter
        CHARACTER(len=:), allocatable                     :: config_path
        real(kind=wp)                                     :: wp_normal_eqa_diff2(4, 4), wp_normal_eqb_diff2(4)
        real(kind=wp)                                     :: wp_normal_eqa_diff3(4, 4), wp_normal_eqb_diff3(4)
        real(kind=wp)                                     :: wp_x(4) = 0.0_wp, wp_temp(2)
        real(kind=wp), allocatable                        :: wp_eqa_diff2(:, :), wp_eqa_diff3(:, :)
        real(kind=wp), allocatable                        :: wp_eqb_diff2(:), wp_eqb_diff3(:)
        real(kind=wp)                                     :: wp_nonsens_eqa(2, 4) !< the amplitude of four non-sensitive axises at
                                                                                  !< signal frequency and doubled signal frequency
        
        !type(pyplot)                                      :: plt
        
        !>------------------------------------------------------------------------------------------
        !> raw filter
        !> high pass filter filename
        fplerror = xml_i%urlpaths%GetAsString(key='ConfigPath', string=config_path)

        !> consider the uncertainty of the initial period
        index_span = i_index_span
        index_span(1, 1) = index_span(1, 1) + 151_ip
        index_span(2, 1) = index_span(2, 1) + 151_ip
        index_span(3, 1) = index_span(3, 1) + 151_ip
        index_span(4, 1) = index_span(4, 1) + 151_ip

        !> time epoch to index
        assign_index_span_5: do i = 1, size(index_span, 1), 1
            do j = 1, size(index_span, 2), 1
                if (mod(j, 2) == 0) then
                    index_span_5(i, j) = floor(real(index_span(i, j), 16) / 5.0_wp) - 1_ip
                else
                    index_span_5(i, j) = ceiling(real(index_span(i, j), 16) / 5.0_wp) + 1_ip
                end if
            end do
        end do assign_index_span_5
        
        !> first time-------------------------------------------------------------------------------
        !> normal matrix for four algorithms
        wp_normal_eqa_diff2 = 0.0_wp
        wp_normal_eqb_diff2 = 0.0_wp
        wp_normal_eqa_diff3 = 0.0_wp
        wp_normal_eqb_diff3 = 0.0_wp
        !> for jd-1: the motivation is along the yaw direction of the leading satellite (x, y)12
        !> for jd-2: the motivation is along the pitch direction of the leading satellite (x, z)13
        !> for jd-3: the motivation is along the yaw direction of the tracking satellite (x, y)45
        !> for jd-4: the motivation is along the pitch direction of the tracking satellite (x, z)46
        assign_jd_loop: do i = 1, 4, 1
            length_span = index_span(i, 2) - index_span(i, 1) + 1
            length_span_5 = index_span_5(i, 2) - index_span_5(i, 1) + 1
            allocate(jd_i(i)%acc1b_lead(1: length_span_5), stat=err)
            if (err /= 0) print *, "jd(i)%acc1b_lead: Allocation request denied"
            allocate(jd_i(i)%acc1b_trac(1: length_span_5), stat=err)
            if (err /= 0) print *, "jd(i)%acc1b_trac: Allocation request denied"
            allocate(jd_i(i)%kbr1b_both(1: length_span_5), stat=err)
            if (err /= 0) print *, "jd(i)%kbr1b_both: Allocation request denied"
            allocate(jd_i(i)%kbr1b_2degdiff(1: length_span_5 - 2), stat=err)
            if (err /= 0) print *, "jd(i)%kbr1b_2defdiff: Allocation request denied"
            allocate(jd_i(i)%kbr1b_3degdiff(1: length_span_5 - 3), stat=err)
            if (err /= 0) print *, "jd(i)%kbr1b_3degediff: Allocation request denied"
            allocate(jd_i(i)%sca1b_lead(1: length_span), stat=err)
            if (err /= 0) print *, "jd(i)%sca1b_lead: Allocation request denied"
            allocate(jd_i(i)%sca1b_trac(1: length_span), stat=err)
            if (err /= 0) print *, "jd(i)%sca1b_trac: Allocation request denied"
            allocate(jd_i(i)%gps1b_lead(1: length_span), stat=err)
            if (err /= 0) print *, "jd(i)%gps1b_lead: Allocation request denied"
            allocate(jd_i(i)%gps1b_trac(1: length_span), stat=err)
            if (err /= 0) print *, "jd(i)%gps1b_trac: Allocation request denied"
            
            !> necesary assignment
            jd_i(i)%kbr1b_both = self%kbr1b_both(index_span_5(i, 1): index_span_5(i, 2))
            jd_i(i)%acc1b_lead = self%acc1b_lead(index_span_5(i, 1): index_span_5(i, 2))
            jd_i(i)%acc1b_trac = self%acc1b_trac(index_span_5(i, 1): index_span_5(i, 2))
            jd_i(i)%gps1b_lead = self%gps1b_lead(index_span(i, 1): index_span(i, 2))
            jd_i(i)%gps1b_trac = self%gps1b_trac(index_span(i, 1): index_span(i, 2))
            jd_i(i)%sca1b_lead = self%sca1b_lead(index_span(i, 1): index_span(i, 2))
            jd_i(i)%sca1b_trac = self%sca1b_trac(index_span(i, 1): index_span(i, 2))
            
            !>--------------------------------------------------------------------------------------
            !> create equations
            call jd_i(i)%create_phase_centre_vad_eq(i)
            
            allocate(wp_eqa_diff2(size(jd_i(i)%kbr1b_2degdiff), 6), stat=err)
            if (err /= 0) print *, "wp_eqa_diff2: Allocation request denied"
            allocate(wp_eqa_diff3(size(jd_i(i)%kbr1b_3degdiff), 6), stat=err)
            if (err /= 0) print *, "wp_eqa_diff3: Allocation request denied"
            allocate(wp_eqb_diff2(size(jd_i(i)%kbr1b_2degdiff)), stat=err)
            if (err /= 0) print *, "wp_eqb_diff2: Allocation request denied"
            allocate(wp_eqb_diff3(size(jd_i(i)%kbr1b_3degdiff)), stat=err)
            if (err /= 0) print *, "wp_eqb_diff3: Allocation request denied"
            
            !> high pass filter for the data vector
            !> diff-2
            wp_eqb_diff2 = jd_i(i)%kbr1b_2degdiff%eq_b
            call fir_filter(trim(config_path)//'coeff_band_pass_0.001_0.009_ls.fcf', wp_eqb_diff2, 0.2_wp, &
                           jd_i(i)%kbr1b_2degdiff%eq_b, nfilter)

            !> diff-3
            wp_eqb_diff3 = jd_i(i)%kbr1b_3degdiff%eq_b
            call fir_filter(trim(config_path)//'coeff_band_pass_0.001_0.009_ls.fcf', wp_eqb_diff3, 0.2_wp, &
                           jd_i(i)%kbr1b_3degdiff%eq_b, nfilter)

            !> high pass filter for the model matrix separately
            do j = 1, 6, 1
                !> diff-2
                wp_eqb_diff2 = jd_i(i)%kbr1b_2degdiff%eq_a(j)
                call fir_filter(trim(config_path)//'coeff_band_pass_0.001_0.009_ls.fcf', wp_eqb_diff2, 0.2_wp, &
                           wp_eqa_diff2(:, j), nfilter)
                !> diff-3
                wp_eqb_diff3 = jd_i(i)%kbr1b_3degdiff%eq_a(j)
                call fir_filter(trim(config_path)//'coeff_band_pass_0.001_0.009_ls.fcf', wp_eqb_diff3, 0.2_wp, &
                           wp_eqa_diff3(:, j), nfilter)
            end do

            !> normal equation for diff-2 and diff-3 in the time domain
            wp_normal_eqa_diff2 = wp_normal_eqa_diff2 + matmul(TRANSPOSE(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), [2, 3, 5, 6])), &
                                                                         wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), [2, 3, 5, 6]))
            wp_normal_eqb_diff2 = wp_normal_eqb_diff2 + matmul(TRANSPOSE(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), [2, 3, 5, 6])), &
                                                               jd_i(i)%kbr1b_2degdiff(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b))%eq_b  &
                                                      -           MATMUL(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), [1, 4]), [self%initial_vector(1), self%initial_vector(4)]))
            wp_normal_eqa_diff3 = wp_normal_eqa_diff3 + matmul(TRANSPOSE(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), [2, 3, 5, 6])), &
                                                                         wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), [2, 3, 5, 6]))
            wp_normal_eqb_diff3 = wp_normal_eqb_diff3 + matmul(TRANSPOSE(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), [2, 3, 5, 6])), &
                                                               jd_i(i)%kbr1b_3degdiff(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b))%eq_b &
                                                      -           MATMUL(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), [1, 4]), [self%initial_vector(1), self%initial_vector(4)]))
            
            !> identify the amplitude of the Fourier component at the maneuver frequency and doubled maneuver frequency
            call jd_i(i)%amp_identification(jd_i(i)%kbr1b_2degdiff(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b))%eq_b, &
                                            jd_i(i)%wp_amp_freq(:, 3))
            call jd_i(i)%amp_identification(jd_i(i)%kbr1b_3degdiff(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b))%eq_b, &
                                            jd_i(i)%wp_amp_freq(:, 4))
            
            !> solver for the two algorithms separately in the frequency domain
            select case (i)
                case (1)
                    !> diff2
                    call jd_i(i)%amp_identification(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 1), &
                                                    jd_i(i)%wp_amp_freq(:, 1))
                    call jd_i(i)%amp_identification(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 2), &
                                                    jd_i(i)%wp_amp_freq(:, 2))
                    !> mirror maneuver
                    !> ref: estimation theory for KBR on-board calibration
                    if (i_mirror == 0) then
                        jd_i(i)%wp_amp_freq(2, 2) = -jd_i(i)%wp_amp_freq(2, 2)
                    else
                        jd_i(i)%wp_amp_freq(1, 2) = -jd_i(i)%wp_amp_freq(1, 2)
                    end if!> solve the linear equation system
                    self%inverse_vector(1, 2) = (jd_i(i)%wp_amp_freq(1, 3) - self%initial_vector(1) * jd_i(i)%wp_amp_freq(1, 1)) / jd_i(i)%wp_amp_freq(1, 2)
                    
                    !> diff3
                    call jd_i(i)%amp_identification(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 1), &
                                                    jd_i(i)%wp_amp_freq(:, 1))
                    call jd_i(i)%amp_identification(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 2), &
                                                    jd_i(i)%wp_amp_freq(:, 2))
                    !> mirror maneuver
                    !> ref: estimation theory for KBR on-board calibration
                    if (i_mirror == 0) then
                        jd_i(i)%wp_amp_freq(2, 2) = -jd_i(i)%wp_amp_freq(2, 2)
                    else
                        jd_i(i)%wp_amp_freq(1, 2) = -jd_i(i)%wp_amp_freq(1, 2)
                    end if
                    !> solve the linear equation system
                    self%inverse_vector(2, 2) = (jd_i(i)%wp_amp_freq(1, 4) - self%initial_vector(1) * jd_i(i)%wp_amp_freq(1, 1)) / jd_i(i)%wp_amp_freq(1, 2)
                case (2)
                    !> diff2
                    call jd_i(i)%amp_identification(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 1), &
                                                    jd_i(i)%wp_amp_freq(:, 1))
                    call jd_i(i)%amp_identification(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 3), &
                                                    jd_i(i)%wp_amp_freq(:, 2))
                    !> mirror maneuver
                    !> ref: estimation theory for KBR on-board calibration
                    if (i_mirror == 0) then
                        jd_i(i)%wp_amp_freq(2, 2) = -jd_i(i)%wp_amp_freq(2, 2)
                    else
                        jd_i(i)%wp_amp_freq(1, 2) = -jd_i(i)%wp_amp_freq(1, 2)
                    end if
                    !> solve the linear equation system
                    self%inverse_vector(1, 3) = (jd_i(i)%wp_amp_freq(1, 3) - self%initial_vector(1) * jd_i(i)%wp_amp_freq(1, 1)) / jd_i(i)%wp_amp_freq(1, 2)
                    self%inverse_vector(1, 1) = self%initial_vector(1)
                    !> diff3
                    call jd_i(i)%amp_identification(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 1), &
                                                    jd_i(i)%wp_amp_freq(:, 1))
                    call jd_i(i)%amp_identification(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 3), &
                                                    jd_i(i)%wp_amp_freq(:, 2))
                    !> mirror maneuver
                    !> ref: estimation theory for KBR on-board calibration
                    if (i_mirror == 0) then
                        jd_i(i)%wp_amp_freq(2, 2) = -jd_i(i)%wp_amp_freq(2, 2)
                    else
                        jd_i(i)%wp_amp_freq(1, 2) = -jd_i(i)%wp_amp_freq(1, 2)
                    end if
                    !> solve the linear equation system
                    self%inverse_vector(2, 3) = (jd_i(i)%wp_amp_freq(1, 4) - self%initial_vector(1) * jd_i(i)%wp_amp_freq(1, 1)) / jd_i(i)%wp_amp_freq(1, 2)
                    self%inverse_vector(2, 1) = self%initial_vector(1)
                case (3)
                    !> diff2
                    call jd_i(i)%amp_identification(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 4), &
                                                    jd_i(i)%wp_amp_freq(:, 1))
                    call jd_i(i)%amp_identification(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 5), &
                                                    jd_i(i)%wp_amp_freq(:, 2))
                    !> mirror maneuver
                    !> ref: estimation theory for KBR on-board calibration
                    if (i_mirror == 0) then
                        jd_i(i)%wp_amp_freq(2, 2) = -jd_i(i)%wp_amp_freq(2, 2)
                    else
                        jd_i(i)%wp_amp_freq(1, 2) = -jd_i(i)%wp_amp_freq(1, 2)
                    end if
                    !> solve the linear equation system
                    self%inverse_vector(1, 5) = (jd_i(i)%wp_amp_freq(1, 3) - self%initial_vector(4) * jd_i(i)%wp_amp_freq(1, 1)) / jd_i(i)%wp_amp_freq(1, 2)
                    !> diff3
                    call jd_i(i)%amp_identification(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 4), &
                                                    jd_i(i)%wp_amp_freq(:, 1))
                    call jd_i(i)%amp_identification(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 5), &
                                                    jd_i(i)%wp_amp_freq(:, 2))
                    !> mirror maneuver
                    !> ref: estimation theory for KBR on-board calibration
                    if (i_mirror == 0) then
                        jd_i(i)%wp_amp_freq(2, 2) = -jd_i(i)%wp_amp_freq(2, 2)
                    else
                        jd_i(i)%wp_amp_freq(1, 2) = -jd_i(i)%wp_amp_freq(1, 2)
                    end if
                    !> solve the linear equation system
                    self%inverse_vector(2, 5) = (jd_i(i)%wp_amp_freq(1, 4) - self%initial_vector(4) * jd_i(i)%wp_amp_freq(1, 1)) / jd_i(i)%wp_amp_freq(1, 2)
                case (4)
                    !> diff2
                    call jd_i(i)%amp_identification(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 4), &
                                                    jd_i(i)%wp_amp_freq(:, 1))
                    call jd_i(i)%amp_identification(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 6), &
                                                    jd_i(i)%wp_amp_freq(:, 2))
                    !> mirror maneuver
                    !> ref: estimation theory for KBR on-board calibration
                    if (i_mirror == 0) then
                        jd_i(i)%wp_amp_freq(2, 2) = -jd_i(i)%wp_amp_freq(2, 2)
                    else
                        jd_i(i)%wp_amp_freq(1, 2) = -jd_i(i)%wp_amp_freq(1, 2)
                    end if
                    !> solve the linear equation system
                    self%inverse_vector(1, 6) = (jd_i(i)%wp_amp_freq(1, 3) - self%initial_vector(4) * jd_i(i)%wp_amp_freq(1, 1)) / jd_i(i)%wp_amp_freq(1, 2)
                    self%inverse_vector(1, 4) = self%initial_vector(4)
                    !> diff3
                    call jd_i(i)%amp_identification(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 4), &
                                                    jd_i(i)%wp_amp_freq(:, 1))
                    call jd_i(i)%amp_identification(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 6), &
                                                    jd_i(i)%wp_amp_freq(:, 2))
                    !> mirror maneuver
                    !> ref: estimation theory for KBR on-board calibration
                    if (i_mirror == 0) then
                        jd_i(i)%wp_amp_freq(2, 2) = -jd_i(i)%wp_amp_freq(2, 2)
                    else
                        jd_i(i)%wp_amp_freq(1, 2) = -jd_i(i)%wp_amp_freq(1, 2)
                    end if
                    !> solve the linear equation system
                    self%inverse_vector(2, 6) = (jd_i(i)%wp_amp_freq(1, 4) - self%initial_vector(4) * jd_i(i)%wp_amp_freq(1, 1)) / jd_i(i)%wp_amp_freq(1, 2)
                    self%inverse_vector(2, 4) = self%initial_vector(4)
                case default
            end select

            if (allocated(wp_eqa_diff2)) deallocate(wp_eqa_diff2, stat=err)
            if (err /= 0) print *, "wp_eqa_diff2: Deallocation request denied"
            if (allocated(wp_eqa_diff3)) deallocate(wp_eqa_diff3, stat=err)
            if (err /= 0) print *, "wp_eqa_diff3: Deallocation request denied"
            if (allocated(wp_eqb_diff2)) deallocate(wp_eqb_diff2, stat=err)
            if (err /= 0) print *, "wp_eqb_diff2: Deallocation request denied"
            if (allocated(wp_eqb_diff3)) deallocate(wp_eqb_diff3, stat=err)
            if (err /= 0) print *, "wp_eqb_diff3: Deallocation request denied"
            
            !> deallocate
            if (allocated(jd_i(i)%gps1b_trac)) deallocate(jd_i(i)%gps1b_trac, stat=err)
            if (err /= 0) print *, "jd(i)%gps1b_trac: Deallocation request denied"
            if (allocated(jd_i(i)%gps1b_lead)) deallocate(jd_i(i)%gps1b_lead, stat=err)
            if (err /= 0) print *, "jd(i)%gps1b_lead: Deallocation request denied"
            if (allocated(jd_i(i)%sca1b_trac)) deallocate(jd_i(i)%sca1b_trac, stat=err)
            if (err /= 0) print *, "jd(i)%sca1b_trac: Deallocation request denied"
            if (allocated(jd_i(i)%sca1b_lead)) deallocate(jd_i(i)%sca1b_lead, stat=err)
            if (err /= 0) print *, "jd(i)%sca1b_lead: Deallocation request denied"
            if (allocated(jd_i(i)%kbr1b_both)) deallocate(jd_i(i)%kbr1b_both, stat=err)
            if (err /= 0) print *, "jd(i)%kbr1b_both: Deallocation request denied"
            if (allocated(jd_i(i)%acc1b_lead)) deallocate(jd_i(i)%acc1b_lead, stat=err)
            if (err /= 0) print *, "jd(i)%acc1b_lead: Deallocation request denied"
            if (allocated(jd_i(i)%acc1b_trac)) deallocate(jd_i(i)%acc1b_trac, stat=err)
            if (err /= 0) print *, "jd(i)%acc1b_trac: Deallocation request denied"
            if (allocated(jd_i(i)%kbr1b_2degdiff)) deallocate(jd_i(i)%kbr1b_2degdiff, stat=err)
            if (err /= 0) print *, "jd(i)%kbr1b_2degdiff: Deallocation request denied"
            if (allocated(jd_i(i)%kbr1b_3degdiff)) deallocate(jd_i(i)%kbr1b_3degdiff, stat=err)
            if (err /= 0) print *, "jd(i)%kbr1b_3degdiff: Deallocation request denied"
        end do assign_jd_loop
        
        !> solve the normal equation combined with 4 maneuvers in the time domain
        self%inverse_vector(3, 1) = self%initial_vector(1)
        self%inverse_vector(3, 4) = self%initial_vector(4)
        self%inverse_vector(4, 1) = self%initial_vector(1)
        self%inverse_vector(4, 4) = self%initial_vector(4)
        self%inverse_vector(3, [2, 3, 5, 6]) = ls_solver(wp_normal_eqa_diff2, wp_normal_eqb_diff2)
        self%inverse_vector(4, [2, 3, 5, 6]) = ls_solver(wp_normal_eqa_diff3, wp_normal_eqb_diff3)
        self%inverse_vector_n = self%inverse_vector
        !>------------------------------------------------------------------------------------------

        !> second time -----------------------------------------------------------------------------
        !> normal matrix for four algorithms
        wp_normal_eqa_diff2 = 0.0_wp
        wp_normal_eqb_diff2 = 0.0_wp
        wp_normal_eqa_diff3 = 0.0_wp
        wp_normal_eqb_diff3 = 0.0_wp
        !> for jd-1: the motivation is along the yaw direction of the leading satellite (x, y)12
        !> for jd-2: the motivation is along the pitch direction of the leading satellite (x, z)13
        !> for jd-3: the motivation is along the yaw direction of the tracking satellite (x, y)45
        !> for jd-4: the motivation is along the pitch direction of the tracking satellite (x, z)46
        assign_jd_loop_n: do i = 1, 4, 1
            length_span = index_span(i, 2) - index_span(i, 1) + 1
            length_span_5 = index_span_5(i, 2) - index_span_5(i, 1) + 1
            allocate(jd_i(i)%acc1b_lead(1: length_span_5), stat=err)
            if (err /= 0) print *, "jd(i)%acc1b_lead: Allocation request denied"
            allocate(jd_i(i)%acc1b_trac(1: length_span_5), stat=err)
            if (err /= 0) print *, "jd(i)%acc1b_trac: Allocation request denied"
            allocate(jd_i(i)%kbr1b_both(1: length_span_5), stat=err)
            if (err /= 0) print *, "jd(i)%kbr1b_both: Allocation request denied"
            allocate(jd_i(i)%kbr1b_2degdiff(1: length_span_5 - 2), stat=err)
            if (err /= 0) print *, "jd(i)%kbr1b_2defdiff: Allocation request denied"
            allocate(jd_i(i)%kbr1b_3degdiff(1: length_span_5 - 3), stat=err)
            if (err /= 0) print *, "jd(i)%kbr1b_3degediff: Allocation request denied"
            allocate(jd_i(i)%sca1b_lead(1: length_span), stat=err)
            if (err /= 0) print *, "jd(i)%sca1b_lead: Allocation request denied"
            allocate(jd_i(i)%sca1b_trac(1: length_span), stat=err)
            if (err /= 0) print *, "jd(i)%sca1b_trac: Allocation request denied"
            allocate(jd_i(i)%gps1b_lead(1: length_span), stat=err)
            if (err /= 0) print *, "jd(i)%gps1b_lead: Allocation request denied"
            allocate(jd_i(i)%gps1b_trac(1: length_span), stat=err)
            if (err /= 0) print *, "jd(i)%gps1b_trac: Allocation request denied"
            
            !> necesary assignment
            jd_i(i)%kbr1b_both = self%kbr1b_both(index_span_5(i, 1): index_span_5(i, 2))
            jd_i(i)%acc1b_lead = self%acc1b_lead(index_span_5(i, 1): index_span_5(i, 2))
            jd_i(i)%acc1b_trac = self%acc1b_trac(index_span_5(i, 1): index_span_5(i, 2))
            jd_i(i)%gps1b_lead = self%gps1b_lead(index_span(i, 1): index_span(i, 2))
            jd_i(i)%gps1b_trac = self%gps1b_trac(index_span(i, 1): index_span(i, 2))
            jd_i(i)%sca1b_lead = self%sca1b_lead(index_span(i, 1): index_span(i, 2))
            jd_i(i)%sca1b_trac = self%sca1b_trac(index_span(i, 1): index_span(i, 2))
            
            !>--------------------------------------------------------------------------------------
            !> create equations
            call jd_i(i)%create_phase_centre_vad_eq(i)
            
            allocate(wp_eqa_diff2(size(jd_i(i)%kbr1b_2degdiff), 6), stat=err)
            if (err /= 0) print *, "wp_eqa_diff2: Allocation request denied"
            allocate(wp_eqa_diff3(size(jd_i(i)%kbr1b_3degdiff), 6), stat=err)
            if (err /= 0) print *, "wp_eqa_diff3: Allocation request denied"
            allocate(wp_eqb_diff2(size(jd_i(i)%kbr1b_2degdiff)), stat=err)
            if (err /= 0) print *, "wp_eqb_diff2: Allocation request denied"
            allocate(wp_eqb_diff3(size(jd_i(i)%kbr1b_3degdiff)), stat=err)
            if (err /= 0) print *, "wp_eqb_diff3: Allocation request denied"
            
            !> high pass filter for the data vector
            !> diff-2
            wp_eqb_diff2 = jd_i(i)%kbr1b_2degdiff%eq_b
            call fir_filter(trim(config_path)//'coeff_band_pass_0.001_0.009_ls.fcf', wp_eqb_diff2, 0.2_wp, &
                           jd_i(i)%kbr1b_2degdiff%eq_b, nfilter)

            !> diff-3
            wp_eqb_diff3 = jd_i(i)%kbr1b_3degdiff%eq_b
            call fir_filter(trim(config_path)//'coeff_band_pass_0.001_0.009_ls.fcf', wp_eqb_diff3, 0.2_wp, &
                           jd_i(i)%kbr1b_3degdiff%eq_b, nfilter)

            !> high pass filter for the model matrix separately
            do j = 1, 6, 1
                !> diff-2
                wp_eqb_diff2 = jd_i(i)%kbr1b_2degdiff%eq_a(j)
                call fir_filter(trim(config_path)//'coeff_band_pass_0.001_0.009_ls.fcf', wp_eqb_diff2, 0.2_wp, &
                           wp_eqa_diff2(:, j), nfilter)
                !> diff-3
                wp_eqb_diff3 = jd_i(i)%kbr1b_3degdiff%eq_a(j)
                call fir_filter(trim(config_path)//'coeff_band_pass_0.001_0.009_ls.fcf', wp_eqb_diff3, 0.2_wp, &
                           wp_eqa_diff3(:, j), nfilter)
            end do

            !> identify the amplitude of the Fourier component at the maneuver frequency and doubled maneuver frequency
            call jd_i(i)%amp_identification(jd_i(i)%kbr1b_2degdiff(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b))%eq_b, &
                                            jd_i(i)%wp_amp_freq(:, 3))
            call jd_i(i)%amp_identification(jd_i(i)%kbr1b_3degdiff(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b))%eq_b, &
                                            jd_i(i)%wp_amp_freq(:, 4))
            
            !> solver for the two algorithms separately in the frequency domain
            select case (i)
                case (1)
                    !> diff2
                    !> freq
                    !> sensitive axis
                    call jd_i(i)%amp_identification(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 1), &
                                                    jd_i(i)%wp_amp_freq(:, 1))
                    call jd_i(i)%amp_identification(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 2), &
                                                    jd_i(i)%wp_amp_freq(:, 2))
                    !> nonsensitive axis
                    call jd_i(i)%amp_identification(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 3), &
                                                    wp_nonsens_eqa(:, 1))
                    call jd_i(i)%amp_identification(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 4), &
                                                    wp_nonsens_eqa(:, 2))
                    call jd_i(i)%amp_identification(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 5), &
                                                    wp_nonsens_eqa(:, 3))
                    call jd_i(i)%amp_identification(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 6), &
                                                    wp_nonsens_eqa(:, 4))
                    !> mirror maneuver
                    !> ref: estimation theory for KBR on-board calibration
                    if (i_mirror == 0) then
                        jd_i(i)%wp_amp_freq(2, 2) = -jd_i(i)%wp_amp_freq(2, 2)
                    else
                        jd_i(i)%wp_amp_freq(1, 2) = -jd_i(i)%wp_amp_freq(1, 2)
                    end if
                    !> solve the linear equation system
                    self%inverse_vector(1, 2) = (jd_i(i)%wp_amp_freq(1, 3) &
                                                - self%initial_vector(1) * jd_i(i)%wp_amp_freq(1, 1) &
                                                - self%inverse_vector_n(1, 3) * wp_nonsens_eqa(1, 1) &
                                                - self%inverse_vector_n(1, 4) * wp_nonsens_eqa(1, 2) &
                                                - self%inverse_vector_n(1, 5) * wp_nonsens_eqa(1, 3) &
                                                - self%inverse_vector_n(1, 6) * wp_nonsens_eqa(1, 4))&
                                                / jd_i(i)%wp_amp_freq(1, 2)

                    !> time domain
                    ! self%inverse_vector(3, 2) = dot_product(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 2), &
                    !                                         jd_i(i)%kbr1b_2degdiff(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b))%eq_b  &
                    !                                         - MATMUL(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), [1, 3, 4, 5, 6]), &
                    !                                                  self%inverse_vector_n(3, [1, 3, 4, 5, 6]))) &
                    !                             / dot_product(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 2), &
                    !                                           wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 2))

                    !> diff3
                    !> sensitive axis
                    call jd_i(i)%amp_identification(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 1), &
                                                    jd_i(i)%wp_amp_freq(:, 1))
                    call jd_i(i)%amp_identification(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 2), &
                                                    jd_i(i)%wp_amp_freq(:, 2))
                    !> nonsensitive axis
                    call jd_i(i)%amp_identification(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 3), &
                                                    wp_nonsens_eqa(:, 1))
                    call jd_i(i)%amp_identification(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 4), &
                                                    wp_nonsens_eqa(:, 2))
                    call jd_i(i)%amp_identification(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 5), &
                                                    wp_nonsens_eqa(:, 3))
                    call jd_i(i)%amp_identification(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 6), &
                                                    wp_nonsens_eqa(:, 4))
                    !> mirror maneuver
                    !> ref: estimation theory for KBR on-board calibration
                    if (i_mirror == 0) then
                        jd_i(i)%wp_amp_freq(2, 2) = -jd_i(i)%wp_amp_freq(2, 2)
                    else
                        jd_i(i)%wp_amp_freq(1, 2) = -jd_i(i)%wp_amp_freq(1, 2)
                    end if
                    !> solve the linear equation system
                    self%inverse_vector(2, 2) = (jd_i(i)%wp_amp_freq(1, 4) &
                                                - self%initial_vector(1) * jd_i(i)%wp_amp_freq(1, 1) &
                                                - self%inverse_vector_n(2, 3) * wp_nonsens_eqa(1, 1) &
                                                - self%inverse_vector_n(2, 4) * wp_nonsens_eqa(1, 2) &
                                                - self%inverse_vector_n(2, 5) * wp_nonsens_eqa(1, 3) &
                                                - self%inverse_vector_n(2, 6) * wp_nonsens_eqa(1, 4))&
                                                / jd_i(i)%wp_amp_freq(1, 2)
                    !> time domain
                    ! elf%inverse_vector(4, 2) = dot_product(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 2), &
                    !                                        jd_i(i)%kbr1b_3degdiff(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b))%eq_b  &
                    !                                        - MATMUL(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), [1, 3, 4, 5, 6]), &
                    !                                                 self%inverse_vector_n(4, [1, 3, 4, 5, 6]))) &
                    !                            / dot_product(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 2), &
                    !                                          wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 2))

                case (2)
                    !> diff2
                    !> frequency domain
                    !> sensitive axis
                    call jd_i(i)%amp_identification(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 1), &
                                                    jd_i(i)%wp_amp_freq(:, 1))
                    call jd_i(i)%amp_identification(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 3), &
                                                    jd_i(i)%wp_amp_freq(:, 2))
                    !> nonsensitive axis
                    call jd_i(i)%amp_identification(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 2), &
                                                    wp_nonsens_eqa(:, 1))
                    call jd_i(i)%amp_identification(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 4), &
                                                    wp_nonsens_eqa(:, 2))
                    call jd_i(i)%amp_identification(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 5), &
                                                    wp_nonsens_eqa(:, 3))
                    call jd_i(i)%amp_identification(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 6), &
                                                    wp_nonsens_eqa(:, 4))
                    !> mirror maneuver
                    !> ref: estimation theory for KBR on-board calibration
                    if (i_mirror == 0) then
                        jd_i(i)%wp_amp_freq(2, 2) = -jd_i(i)%wp_amp_freq(2, 2)
                    else
                        jd_i(i)%wp_amp_freq(1, 2) = -jd_i(i)%wp_amp_freq(1, 2)
                    end if
                    !> solve the linear equation system
                    self%inverse_vector(1, 3) = (jd_i(i)%wp_amp_freq(1, 3) &
                                                - self%initial_vector(1) * jd_i(i)%wp_amp_freq(1, 1) &
                                                - self%inverse_vector_n(1, 2) * wp_nonsens_eqa(1, 1) &
                                                - self%inverse_vector_n(1, 4) * wp_nonsens_eqa(1, 2) &
                                                - self%inverse_vector_n(1, 5) * wp_nonsens_eqa(1, 3) &
                                                - self%inverse_vector_n(1, 6) * wp_nonsens_eqa(1, 4))&
                                                / jd_i(i)%wp_amp_freq(1, 2)
                    !> time domain
                    ! self%inverse_vector(3, 3) = dot_product(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 3), &
                    !                                         jd_i(i)%kbr1b_2degdiff(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b))%eq_b  &
                    !                                         - MATMUL(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), [1, 2, 4, 5, 6]), &
                    !                                                  self%inverse_vector_n(3, [1, 2, 4, 5, 6]))) &
                    !                             / dot_product(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 3), &
                    !                                           wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 3))
                    
                    !> diff3
                    !> frequency domain
                    !> sensitive axis
                    call jd_i(i)%amp_identification(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 1), &
                                                    jd_i(i)%wp_amp_freq(:, 1))
                    call jd_i(i)%amp_identification(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 3), &
                                                    jd_i(i)%wp_amp_freq(:, 2))
                    !> nonsensitive axis
                    call jd_i(i)%amp_identification(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 2), &
                                                    wp_nonsens_eqa(:, 1))
                    call jd_i(i)%amp_identification(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 4), &
                                                    wp_nonsens_eqa(:, 2))
                    call jd_i(i)%amp_identification(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 5), &
                                                    wp_nonsens_eqa(:, 3))
                    call jd_i(i)%amp_identification(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 6), &
                                                    wp_nonsens_eqa(:, 4))
                    !> mirror maneuver
                    !> ref: estimation theory for KBR on-board calibration
                    if (i_mirror == 0) then
                        jd_i(i)%wp_amp_freq(2, 2) = -jd_i(i)%wp_amp_freq(2, 2)
                    else
                        jd_i(i)%wp_amp_freq(1, 2) = -jd_i(i)%wp_amp_freq(1, 2)
                    end if
                    !> solve the linear equation system
                    self%inverse_vector(2, 3) = (jd_i(i)%wp_amp_freq(1, 4) &
                                                - self%initial_vector(1) * jd_i(i)%wp_amp_freq(1, 1) &
                                                - self%inverse_vector_n(2, 2) * wp_nonsens_eqa(1, 1) &
                                                - self%inverse_vector_n(2, 4) * wp_nonsens_eqa(1, 2) &
                                                - self%inverse_vector_n(2, 5) * wp_nonsens_eqa(1, 3) &
                                                - self%inverse_vector_n(2, 6) * wp_nonsens_eqa(1, 4))&
                                                / jd_i(i)%wp_amp_freq(1, 2)
                    !> time domain
                    ! self%inverse_vector(4, 3) = dot_product(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 3), &
                    !                                         jd_i(i)%kbr1b_3degdiff(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b))%eq_b  &
                    !                                         - MATMUL(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), [1, 2, 4, 5, 6]), &
                    !                                                  self%inverse_vector_n(4, [1, 2, 4, 5, 6]))) &
                    !                             / dot_product(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 3), &
                    !                                           wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 3))

                case (3)
                    !> diff2
                    !> frequency domain
                    !> sensitive axis
                    call jd_i(i)%amp_identification(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 4), &
                                                    jd_i(i)%wp_amp_freq(:, 1))
                    call jd_i(i)%amp_identification(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 5), &
                                                    jd_i(i)%wp_amp_freq(:, 2))
                    !> nonsensitive axis
                    call jd_i(i)%amp_identification(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 1), &
                                                    wp_nonsens_eqa(:, 1))
                    call jd_i(i)%amp_identification(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 2), &
                                                    wp_nonsens_eqa(:, 2))
                    call jd_i(i)%amp_identification(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 3), &
                                                    wp_nonsens_eqa(:, 3))
                    call jd_i(i)%amp_identification(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 6), &
                                                    wp_nonsens_eqa(:, 4))
                    !> mirror maneuver
                    !> ref: estimation theory for KBR on-board calibration
                    if (i_mirror == 0) then
                        jd_i(i)%wp_amp_freq(2, 2) = -jd_i(i)%wp_amp_freq(2, 2)
                    else
                        jd_i(i)%wp_amp_freq(1, 2) = -jd_i(i)%wp_amp_freq(1, 2)
                    end if
                    !> solve the linear equation system
                    self%inverse_vector(1, 5) = (jd_i(i)%wp_amp_freq(1, 3) &
                                                - self%initial_vector(4) * jd_i(i)%wp_amp_freq(1, 1) &
                                                - self%inverse_vector_n(1, 1) * wp_nonsens_eqa(1, 1) &
                                                - self%inverse_vector_n(1, 2) * wp_nonsens_eqa(1, 2) &
                                                - self%inverse_vector_n(1, 3) * wp_nonsens_eqa(1, 3) &
                                                - self%inverse_vector_n(1, 6) * wp_nonsens_eqa(1, 4))&
                                                / jd_i(i)%wp_amp_freq(1, 2)
                    !> time domain
                    ! self%inverse_vector(3, 5) = dot_product(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 5), &
                    !                                         jd_i(i)%kbr1b_2degdiff(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b))%eq_b  &
                    !                                         - MATMUL(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), [1, 2, 3, 4, 6]), &
                    !                                                  self%inverse_vector_n(3, [1, 2, 3, 4, 6]))) &
                    !                             / dot_product(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 5), &
                    !                                           wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 5))
                    
                    !> diff3
                    !> frequency domain
                    call jd_i(i)%amp_identification(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 4), &
                                                    jd_i(i)%wp_amp_freq(:, 1))
                    call jd_i(i)%amp_identification(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 5), &
                                                    jd_i(i)%wp_amp_freq(:, 2))
                    !> nonsensitive axis
                    call jd_i(i)%amp_identification(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 1), &
                                                    wp_nonsens_eqa(:, 1))
                    call jd_i(i)%amp_identification(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 2), &
                                                    wp_nonsens_eqa(:, 2))
                    call jd_i(i)%amp_identification(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 3), &
                                                    wp_nonsens_eqa(:, 3))
                    call jd_i(i)%amp_identification(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 6), &
                                                    wp_nonsens_eqa(:, 4))
                    !> mirror maneuver
                    !> ref: estimation theory for KBR on-board calibration
                    if (i_mirror == 0) then
                        jd_i(i)%wp_amp_freq(2, 2) = -jd_i(i)%wp_amp_freq(2, 2)
                    else
                        jd_i(i)%wp_amp_freq(1, 2) = -jd_i(i)%wp_amp_freq(1, 2)
                    end if
                    !> solve the linear equation system
                    self%inverse_vector(2, 5) = (jd_i(i)%wp_amp_freq(1, 4) &
                                                - self%initial_vector(4) * jd_i(i)%wp_amp_freq(1, 1) &
                                                - self%inverse_vector_n(1, 1) * wp_nonsens_eqa(1, 1) &
                                                - self%inverse_vector_n(1, 2) * wp_nonsens_eqa(1, 2) &
                                                - self%inverse_vector_n(1, 3) * wp_nonsens_eqa(1, 3) &
                                                - self%inverse_vector_n(1, 6) * wp_nonsens_eqa(1, 4))&
                                                / jd_i(i)%wp_amp_freq(1, 2)
                    !> time domain
                    ! self%inverse_vector(4, 5) = dot_product(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 5), &
                    !                                         jd_i(i)%kbr1b_3degdiff(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b))%eq_b  &
                    !                                         - MATMUL(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), [1, 2, 3, 4, 6]), &
                    !                                                  self%inverse_vector_n(4, [1, 2, 3, 4, 6]))) &
                    !                             / dot_product(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 5), &
                    !                                           wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 5))

                case (4)
                    !> diff2
                    !> frequency domain
                    !> sensitive axis
                    call jd_i(i)%amp_identification(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 4), &
                                                    jd_i(i)%wp_amp_freq(:, 1))
                    call jd_i(i)%amp_identification(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 6), &
                                                    jd_i(i)%wp_amp_freq(:, 2))
                    !> nonsensitive axis
                    call jd_i(i)%amp_identification(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 1), &
                                                    wp_nonsens_eqa(:, 1))
                    call jd_i(i)%amp_identification(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 2), &
                                                    wp_nonsens_eqa(:, 2))
                    call jd_i(i)%amp_identification(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 3), &
                                                    wp_nonsens_eqa(:, 3))
                    call jd_i(i)%amp_identification(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 5), &
                                                    wp_nonsens_eqa(:, 4))
                    !> mirror maneuver
                    !> ref: estimation theory for KBR on-board calibration
                    if (i_mirror == 0) then
                        jd_i(i)%wp_amp_freq(2, 2) = -jd_i(i)%wp_amp_freq(2, 2)
                    else
                        jd_i(i)%wp_amp_freq(1, 2) = -jd_i(i)%wp_amp_freq(1, 2)
                    end if
                    !> solve the linear equation system
                    self%inverse_vector(1, 6) = (jd_i(i)%wp_amp_freq(1, 3) &
                                                - self%initial_vector(4) * jd_i(i)%wp_amp_freq(1, 1) &
                                                - self%inverse_vector_n(1, 1) * wp_nonsens_eqa(1, 1) &
                                                - self%inverse_vector_n(1, 2) * wp_nonsens_eqa(1, 2) &
                                                - self%inverse_vector_n(1, 3) * wp_nonsens_eqa(1, 3) &
                                                - self%inverse_vector_n(1, 5) * wp_nonsens_eqa(1, 4))&
                                                / jd_i(i)%wp_amp_freq(1, 2)
                    !> time domain
                    ! self%inverse_vector(3, 6) = dot_product(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 6), &
                    !                                         jd_i(i)%kbr1b_2degdiff(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b))%eq_b  &
                    !                                         - MATMUL(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), [1, 2, 3, 4, 5]), &
                    !                                                  self%inverse_vector_n(3, [1, 2, 3, 4, 5]))) &
                    !                             / dot_product(wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 6), &
                    !                                           wp_eqa_diff2(nfilter: size(jd_i(i)%kbr1b_2degdiff%eq_b), 6))

                    !> diff3
                    !> frequency domain
                    !> sensitive axis
                    call jd_i(i)%amp_identification(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 4), &
                                                    jd_i(i)%wp_amp_freq(:, 1))
                    call jd_i(i)%amp_identification(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 6), &
                                                    jd_i(i)%wp_amp_freq(:, 2))
                    !> nonsensitive axis
                    call jd_i(i)%amp_identification(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 1), &
                                                    wp_nonsens_eqa(:, 1))
                    call jd_i(i)%amp_identification(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 2), &
                                                    wp_nonsens_eqa(:, 2))
                    call jd_i(i)%amp_identification(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 3), &
                                                    wp_nonsens_eqa(:, 3))
                    call jd_i(i)%amp_identification(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 5), &
                                                    wp_nonsens_eqa(:, 4))
                    !> mirror maneuver
                    !> ref: estimation theory for KBR on-board calibration
                    if (i_mirror == 0) then
                        jd_i(i)%wp_amp_freq(2, 2) = -jd_i(i)%wp_amp_freq(2, 2)
                    else
                        jd_i(i)%wp_amp_freq(1, 2) = -jd_i(i)%wp_amp_freq(1, 2)
                    end if
                    !> solve the linear equation system
                    self%inverse_vector(2, 6) = (jd_i(i)%wp_amp_freq(1, 4) &
                                                - self%initial_vector(4) * jd_i(i)%wp_amp_freq(1, 1) &
                                                - self%inverse_vector_n(1, 1) * wp_nonsens_eqa(1, 1) &
                                                - self%inverse_vector_n(1, 2) * wp_nonsens_eqa(1, 2) &
                                                - self%inverse_vector_n(1, 3) * wp_nonsens_eqa(1, 3) &
                                                - self%inverse_vector_n(1, 5) * wp_nonsens_eqa(1, 4))&
                                                / jd_i(i)%wp_amp_freq(1, 2)
                    !> time domain
                    ! self%inverse_vector(4, 6) = dot_product(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 6), &
                    !                                         jd_i(i)%kbr1b_3degdiff(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b))%eq_b  &
                    !                                         - MATMUL(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), [1, 2, 3, 4, 5]), &
                    !                                                  self%inverse_vector_n(4, [1, 2, 3, 4, 5]))) &
                    !                             / dot_product(wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 6), &
                    !                                           wp_eqa_diff3(nfilter: size(jd_i(i)%kbr1b_3degdiff%eq_b), 6))

                case default
            end select

            if (allocated(wp_eqa_diff2)) deallocate(wp_eqa_diff2, stat=err)
            if (err /= 0) print *, "wp_eqa_diff2: Deallocation request denied"
            if (allocated(wp_eqa_diff3)) deallocate(wp_eqa_diff3, stat=err)
            if (err /= 0) print *, "wp_eqa_diff3: Deallocation request denied"
            if (allocated(wp_eqb_diff2)) deallocate(wp_eqb_diff2, stat=err)
            if (err /= 0) print *, "wp_eqb_diff2: Deallocation request denied"
            if (allocated(wp_eqb_diff3)) deallocate(wp_eqb_diff3, stat=err)
            if (err /= 0) print *, "wp_eqb_diff3: Deallocation request denied"
            
            !> deallocate
            if (allocated(jd_i(i)%gps1b_trac)) deallocate(jd_i(i)%gps1b_trac, stat=err)
            if (err /= 0) print *, "jd(i)%gps1b_trac: Deallocation request denied"
            if (allocated(jd_i(i)%gps1b_lead)) deallocate(jd_i(i)%gps1b_lead, stat=err)
            if (err /= 0) print *, "jd(i)%gps1b_lead: Deallocation request denied"
            if (allocated(jd_i(i)%sca1b_trac)) deallocate(jd_i(i)%sca1b_trac, stat=err)
            if (err /= 0) print *, "jd(i)%sca1b_trac: Deallocation request denied"
            if (allocated(jd_i(i)%sca1b_lead)) deallocate(jd_i(i)%sca1b_lead, stat=err)
            if (err /= 0) print *, "jd(i)%sca1b_lead: Deallocation request denied"
            if (allocated(jd_i(i)%kbr1b_both)) deallocate(jd_i(i)%kbr1b_both, stat=err)
            if (err /= 0) print *, "jd(i)%kbr1b_both: Deallocation request denied"
            if (allocated(jd_i(i)%acc1b_lead)) deallocate(jd_i(i)%acc1b_lead, stat=err)
            if (err /= 0) print *, "jd(i)%acc1b_lead: Deallocation request denied"
            if (allocated(jd_i(i)%acc1b_trac)) deallocate(jd_i(i)%acc1b_trac, stat=err)
            if (err /= 0) print *, "jd(i)%acc1b_trac: Deallocation request denied"
            if (allocated(jd_i(i)%kbr1b_2degdiff)) deallocate(jd_i(i)%kbr1b_2degdiff, stat=err)
            if (err /= 0) print *, "jd(i)%kbr1b_2degdiff: Deallocation request denied"
            if (allocated(jd_i(i)%kbr1b_3degdiff)) deallocate(jd_i(i)%kbr1b_3degdiff, stat=err)
            if (err /= 0) print *, "jd(i)%kbr1b_3degdiff: Deallocation request denied"
        end do assign_jd_loop_n

        !> expectation
        expectation_loop: do i = 1, 6, 1
            wp_temp = expec_without_outlier(self%inverse_vector(:, i))
            self%inverse_vector_expe(i) = wp_temp(1)
            self%wp_std(i) = wp_temp(2)
        end do expectation_loop
        
        call logger%info("phase_centre_vad", "Solve the linear inverse problem successfully")

    end subroutine solver_gnss

    subroutine simu_ant_phase_corr(self)
        class(satellite)    , INTENT(INOUT)              :: self

        !> reg
        integer(kind=ip)                                 :: i

        simu_loop: do i = 1, size(self%kbr1b_both), 1
            self%kbr1b_both(i)%ant_phase_corr_simu = dot_product(self%kbr1b_both(i)%eq_a(1: 3), self%initial_vector(1: 3)) &
                                                    + dot_product(self%kbr1b_both(i)%eq_a(4: 6), self%initial_vector(4: 6))
        end do simu_loop
    end subroutine simu_ant_phase_corr
    
    subroutine output_rpt(self, dict, version, date, sat_id)
        CLASS(satellite)     , INTENT(INOUT)             :: self

        type(parameterlist_t), intent(in   )             :: dict
        character(len=*)     , intent(in   )             :: version, date
        character(len=*)     , INTENT(IN   )             :: sat_id
        
        type(parameterlistIterator_t)                    :: iterator
        type(io_file)                                    :: ofile
        type(random_type)                                :: file_unit

        character(len=100)                               :: flags(3), key, value, temp
        character(len=14)                                :: datenow
        CHARACTER(len=3000)                              :: result_path(2)
        character(len=3000)                              :: vkbfile_name

        integer(kind=ip)                                 :: i, ios, err, d(8), fplerror
        
        real(kind=wp)                                    :: wp_ia_a, wp_ia_b
        
        logical                                          :: lg_a = .true., lg_b = .true.
        
        !> calculate the included angle
        wp_ia_a = included_angle(self%inverse_vector_expe(1: 3), self%inverse_vector_expe(1: 3) + self%wp_std(1: 3))
        wp_ia_b = included_angle(self%inverse_vector_expe(4: 6), self%inverse_vector_expe(4: 6) + self%wp_std(4: 6))
        
        !> check the validation is valid or not
        if (wp_ia_a > 3.0e-4_Wp) lg_a = .false.
        if (wp_ia_b > 3.0e-4_Wp) lg_b = .false.
        
        datenow = yyyymmddhhmmss()
        !> get the value
        fplerror = dict%Get(key='VKBRPT', value=result_path)
        do i = 1, 2, 1
            if (index(result_path(i), trim(sat_id)) /= 0) vkbfile_name = result_path(i)
        end do
        ofile%name = trim(vkbfile_name)
        
        !>------------------------------------------------------------------------------------------
        !> output file
        ofile%unit = file_unit%random_int_range()
        open(unit=ofile%unit, file=trim(ofile%name), iostat=ios, status="unknown", action="write")
        if ( ios /= 0 ) then
            call logger%fatal('phase_centre_vad', 'Error opening '//trim(ofile%name))
            stop
        end if
        
        write(ofile%unit, "(a)") 'header:'
        write(ofile%unit, "(2x, a)") "dimensions:"
        write(ofile%unit, "(4x, a)") "num_records: 1"
        write(ofile%unit, "(2x, a)") "global_attributes:"
        write(ofile%unit, "(4x, a)") "acknowledgement:"
        write(ofile%unit, "(4x, a)") "conventions:"
        write(ofile%unit, "(4x, a)") "creator_email:"
        write(ofile%unit, "(4x, a)") "creator_institution: 503"
        write(ofile%unit, "(4x, a)") "creator_name: Subsystem of ground processing"
        write(ofile%unit, "(4x, a)") "creator_type: group"
        write(ofile%unit, "(4x, a)") "creator_url:"
        write(ofile%unit, "(4x, a)") "date_created:"
        write(ofile%unit, "(4x, a)") "date_issued:"
        write(ofile%unit, "(4x, a)") "history:"
        write(ofile%unit, "(4x, a)") "id:"
        write(ofile%unit, "(4x, a)") "institution:"
        write(ofile%unit, "(4x, a)") "instrument: Platform geometry"
        write(ofile%unit, "(4x, a)") "instrument_vocabulary"
        write(ofile%unit, "(4x, a)") "keywords: TH, Platform geometry"
        write(ofile%unit, "(4x, a)") "keywords_vocabulary:"
        write(ofile%unit, "(4x, a)") "licence:"
        write(ofile%unit, "(4x, a)") "name_authority:"
        write(ofile%unit, "(4x, a)") "platform: TH-04"
        write(ofile%unit, "(4x, a)") "platform_vocabulary:"
        write(ofile%unit, "(4x, a)") "processing_level: 1B"
        write(ofile%unit, "(4x, a)") "product_version: "//trim(version)
        write(ofile%unit, "(4x, a)") "program: "
        write(ofile%unit, "(4x, a)") "project: "
        write(ofile%unit, "(4x, a)") "reference:"
        write(ofile%unit, "(4x, a)") "source: both satellites of TH-04 SRF location for KBR antenna phase centre"
        write(ofile%unit, "(4x, a)") "summary: Evaluation vector position, in SRF, for KBR antenna phase centre"
        write(ofile%unit, "(4x, a)") "time_coverage_start: "//trim(date)
        write(ofile%unit, "(4x, a)") "time_coverage_end: "//trim(date)
        write(ofile%unit, "(4x, a)") "title: TH-04 Level-1B KBR Antenna Phase Centre Location Evaluation"
        write(ofile%unit, "(2x, a)") "non-standard-attributes:"
        write(ofile%unit, "(4x, a)") "epoch_time:"
        write(ofile%unit, "(4x, a)") "software_build_time: "//trim(datenow)
        write(ofile%unit, "(4x, a)") "software_version: 09"
        write(ofile%unit, "(2x, a)") "variables:"
        write(ofile%unit, "(4x, a)") "- TH-04_id:"
        write(ofile%unit, "(8x, a)") "comment: 1st column"
        write(ofile%unit, "(8x, a)") "coverage_content_type: referenceInformation"
        write(ofile%unit, "(8x, a)") "long_name: TH-04 satellite identifier"
        write(ofile%unit, "(8x, a)") "units: char"
        write(ofile%unit, "(8x, a)") "valid_range: A, B"
        write(ofile%unit, "(8x, a)") "value_meanings:"
        write(ofile%unit, "(10x, a)") "- A = TH-04 1"
        write(ofile%unit, "(10x, a)") "- B = TH-04 2"
        write(ofile%unit, "(4x, a)") "- error:"
        write(ofile%unit, "(8x, a)") "comment: 2nd column"
        write(ofile%unit, "(8x, a)") "coverage_content_type: modelResult"
        write(ofile%unit, "(8x, a)") "long_name: Error for the validation"
        write(ofile%unit, "(4x, a)") "- flag:"
        write(ofile%unit, "(8x, a)") "comment: 3rd column"
        write(ofile%unit, "(8x, a)") "coverage_content_type: modelResult"
        write(ofile%unit, "(8x, a)") "long_name: flag indicating whether the validation is valid"
        write(ofile%unit, "(a)") "# End of YAML header"
        if (trim(sat_id) == '_A_') then
            write(ofile%unit, "(a, 1x, es12.5, 1x, l1)") "A", wp_ia_a, lg_a
        else
            write(ofile%unit, "(a, 1x, es12.5, 1x, l1)") "B", wp_ia_b, lg_b
        end if
        
        close(unit=ofile%unit, iostat=ios)
        if ( ios /= 0 ) then
            call logger%fatal('phase_centre_vad', "Error closing "//trim(ofile%name))
            stop
        end if
        
        if (trim(sat_id) == '_A_') then
            call logger%info('phase_centre_vad', 'create a report for the validation successfully for the leading satellite')
        else
            call logger%info('phase_centre_vad', 'create a report for the validation successfully for the tracking satellite')
        end if
    
    
    end subroutine output_rpt

    subroutine output_data(self, dict, version, date, sat_id)
        CLASS(satellite)     , INTENT(INOUT)             :: self

        type(parameterlist_t), intent(in   )             :: dict
        character(len=*)     , intent(in   )             :: version, date
        character(len=*)     , INTENT(IN   )             :: sat_id

        type(parameterlistIterator_t)                    :: iterator
        type(io_file)                                    :: ofile
        type(random_type)                                :: file_unit

        character(len=100)                               :: flags(3), key, value, temp
        character(len=14)                                :: datenow
        CHARACTER(len=3000)                              :: result_path(2)
        character(len=3000)                              :: vkbfile_name

        integer(kind=ip)                                 :: i, ios, err, d(8), fplerror

        datenow = yyyymmddhhmmss()
        !> get the value
        fplerror = dict%Get(key='VKB', value=result_path)
        do i = 1, 2, 1
            if (index(result_path(i), trim(sat_id)) /= 0) vkbfile_name = result_path(i)
        end do
        ofile%name = trim(vkbfile_name)
        
        !>------------------------------------------------------------------------------------------
        !> output file
        ofile%unit = file_unit%random_int_range()
        open(unit=ofile%unit, file=trim(ofile%name), iostat=ios, status="unknown", action="write")
        if ( ios /= 0 ) then
            call logger%fatal('phase_centre_vad', 'Error opening '//trim(ofile%name))
            stop
        end if
        
        write(ofile%unit, "(a)") 'header:'
        write(ofile%unit, "(2x, a)") "dimensions:"
        write(ofile%unit, "(4x, a)") "num_records: 1"
        write(ofile%unit, "(2x, a)") "global_attributes:"
        write(ofile%unit, "(4x, a)") "acknowledgement:"
        write(ofile%unit, "(4x, a)") "conventions:"
        write(ofile%unit, "(4x, a)") "creator_email:"
        write(ofile%unit, "(4x, a)") "creator_institution: 503"
        write(ofile%unit, "(4x, a)") "creator_name: Subsystem of ground processing"
        write(ofile%unit, "(4x, a)") "creator_type: group"
        write(ofile%unit, "(4x, a)") "creator_url:"
        write(ofile%unit, "(4x, a)") "date_created:"
        write(ofile%unit, "(4x, a)") "date_issued:"
        write(ofile%unit, "(4x, a)") "history:"
        write(ofile%unit, "(4x, a)") "id:"
        write(ofile%unit, "(4x, a)") "institution:"
        write(ofile%unit, "(4x, a)") "instrument: Platform geometry"
        write(ofile%unit, "(4x, a)") "instrument_vocabulary"
        write(ofile%unit, "(4x, a)") "keywords: TH, Platform geometry"
        write(ofile%unit, "(4x, a)") "keywords_vocabulary:"
        write(ofile%unit, "(4x, a)") "licence:"
        write(ofile%unit, "(4x, a)") "name_authority:"
        write(ofile%unit, "(4x, a)") "platform: TH-04"
        write(ofile%unit, "(4x, a)") "platform_vocabulary:"
        write(ofile%unit, "(4x, a)") "processing_level: 1B"
        write(ofile%unit, "(4x, a)") "product_version: "//trim(version)
        write(ofile%unit, "(4x, a)") "program: "
        write(ofile%unit, "(4x, a)") "project: "
        write(ofile%unit, "(4x, a)") "reference:"
        write(ofile%unit, "(4x, a)") "source: both satellites of TH-04 SRF location for KBR antenna phase centre"
        write(ofile%unit, "(4x, a)") "summary: Vector position, in SRF, for KBR antenna phase centre"
        write(ofile%unit, "(4x, a)") "time_coverage_start: "//trim(date)
        write(ofile%unit, "(4x, a)") "time_coverage_end: "//trim(date)
        write(ofile%unit, "(4x, a)") "title: TH-04 Level-1B KBR Antenna Phase Centre Location"
        write(ofile%unit, "(2x, a)") "non-standard-attributes:"
        write(ofile%unit, "(4x, a)") "epoch_time:"
        write(ofile%unit, "(4x, a)") "software_build_time: "//trim(datenow)
        write(ofile%unit, "(4x, a)") "software_version: 09"
        write(ofile%unit, "(2x, a)") "variables:"
        write(ofile%unit, "(4x, a)") "- TH-04_id:"
        write(ofile%unit, "(8x, a)") "comment: 1st column"
        write(ofile%unit, "(8x, a)") "coverage_content_type: referenceInformation"
        write(ofile%unit, "(8x, a)") "long_name: TH-04 satellite identifier"
        write(ofile%unit, "(8x, a)") "units: char"
        write(ofile%unit, "(8x, a)") "valid_range: A, B"
        write(ofile%unit, "(8x, a)") "value_meanings:"
        write(ofile%unit, "(10x, a)") "- A = TH-04 1"
        write(ofile%unit, "(10x, a)") "- B = TH-04 2"
        write(ofile%unit, "(4x, a)") "- xpos:"
        write(ofile%unit, "(8x, a)") "comment: 2nd column"
        write(ofile%unit, "(8x, a)") "coverage_content_type: modelResult"
        write(ofile%unit, "(8x, a)") "long_name: In SRF, the x component of KBR antenna phase centre vector"
        write(ofile%unit, "(4x, a)") "- ypos:"
        write(ofile%unit, "(8x, a)") "comment: 3rd column"
        write(ofile%unit, "(8x, a)") "coverage_content_type: modelResult"
        write(ofile%unit, "(8x, a)") "long_name: In SRF, the y component of KBR antenna phase centre vector"
        write(ofile%unit, "(4x, a)") "- zpos:"
        write(ofile%unit, "(8x, a)") "comment: 4th column"
        write(ofile%unit, "(8x, a)") "coverage_content_type: modelResult"
        write(ofile%unit, "(8x, a)") "long_name: In SRF, the z component of KBR antenna phase centre vector"
        write(ofile%unit, "(4x, a)") "- std_x:"
        write(ofile%unit, "(8x, a)") "comment: 5th column"
        write(ofile%unit, "(8x, a)") "coverage_content_type: modelResult"
        write(ofile%unit, "(8x, a)") "long_name: standard deviation of the x component"
        write(ofile%unit, "(4x, a)") "- std_y:"
        write(ofile%unit, "(8x, a)") "comment: 6th column"
        write(ofile%unit, "(8x, a)") "coverage_content_type: modelResult"
        write(ofile%unit, "(8x, a)") "long_name: standard deviation of the y component"
        write(ofile%unit, "(4x, a)") "- std_z:"
        write(ofile%unit, "(8x, a)") "comment: 7th column"
        write(ofile%unit, "(8x, a)") "coverage_content_type: modelResult"
        write(ofile%unit, "(8x, a)") "long_name: standard deviation of the z component"
        write(ofile%unit, "(a)") "# End of YAML header"
        if (trim(sat_id) == '_A_') then
            write(ofile%unit, "(a, 1x, f8.4, 1x, 2f9.5, 3es13.5)") "A", self%inverse_vector_expe(1: 3), self%wp_std(1: 3)
        else
            write(ofile%unit, "(a, 1x, f8.4, 1x, 2f9.5, 3es13.5)") "B", self%inverse_vector_expe(4: 6), self%wp_std(4: 6)
        end if
        
        close(unit=ofile%unit, iostat=ios)
        if ( ios /= 0 ) then
            call logger%fatal('phase_centre_vad', "Error closing "//trim(ofile%name))
            stop
        end if
        
        if (trim(sat_id) == '_A_') then
            call logger%info('phase_centre_vad', 'writa data to file successfully for the leading satellite')
        else
            call logger%info('phase_centre_vad', 'writa data to file successfully for the tracking satellite')
        end if
    end subroutine output_data

    subroutine solve_phase_centre_vad_eq(self, i_index_motiv)
        class(satellite)     , intent(inout)                 :: self
        integer(kind=ip)     , INTENT(IN   ), OPTIONAL       :: i_index_motiv
        real(kind=wp), ALLOCATABLE                           :: a(:, :), b(:)
        real(kind=wp), ALLOCATABLE                           :: a_valid(:, :), b_valid(:)
        real(kind=wp)                                        :: theta_n_1(8), theta_n(8)
        real(kind=wp)                                        :: temp(6)
        real(kind=wp)                                        :: temp1(size(self%kbr1b_both))
        real(kind=wp)                                        :: temp2(size(self%kbr1b_both))
        integer(kind=ip)                                     :: i, err, j
        integer(kind=ip)                                     :: nfilter, fplerror
        character(len=10)                                    :: count_i
        character(len=1)                                     :: c_index_motiv
        CHARACTER(len=:), allocatable                        :: config_path
        
        allocate(a(size(self%kbr1b_both), 6), stat=err)
        if (err /= 0) print *, "a: Allocation request denied"
        
        assign_a_loop: do i = 1, size(self%kbr1b_both), 1
            a(i, 1: 6) = self%kbr1b_both(i)%eq_a(1: 6)
        end do assign_a_loop
        
        theta_n(1: 6) = ls_solver(a(:, 1: 6), self%kbr1b_both%eq_b)
        self%inverse_vector_expe = theta_n(1: 6)
        temp = self%kbr1b_both%eq_b - matmul(a(:, 1: 6), theta_n(1: 6))
        self%error(1) = sqrt(temp(2)**2 + temp(3)**2)
        self%error(2) = sqrt(temp(5)**2 + temp(6)**2)
        
        if (allocated(a)) deallocate(a, stat=err)
        if (err /= 0) print *, "a: Deallocation request denied"
        
        write(c_index_motiv, "(i1)") i_index_motiv
        call logger%info("phase_centre_vad", "Solve the linear inverse problem successfully of Motivation "//c_index_motiv)
        
        ! theta_n_1 = 0.0_wp
! 
        ! !> high pass filter filename
        ! fplerror = xml_i%urlpaths%GetAsString(key='ConfigPath', string=config_path)
        ! !> high pass filter
        ! !temp1 = self%kbr1b_both%eq_b
        ! !call fir_filter(trim(config_path)//'coeff_high_pass_0.004_100db.fcf', temp1, 0.2_wp, &
        ! !                temp2, nfilter)
        ! !self%kbr1b_both%eq_b = temp2
        ! nfilter = 0_ip
        ! !> assign the valid values of a and b
        ! allocate(a_valid(1: -nfilter + size(self%kbr1b_both%eq_b), 8), stat=err)
        ! if (err /= 0) print *, "a_valid: Allocation request denied"
        ! allocate(b_valid(1: -nfilter + size(self%kbr1b_both%eq_b)), stat=err)
        ! if (err /= 0) print *, "b_valid: Allocation request denied"
        ! do i = 1, size(self%kbr1b_both(1)%eq_a), 1
        !     a_valid(:, i) = self%kbr1b_both(int((nfilter + 1.0_wp) / 2.0_wp): &
        !                     size(self%kbr1b_both%eq_b) - int((nfilter - 1.0_wp) / 2.0_wp))%eq_a(i)
        ! end do 
        ! b_valid = self%kbr1b_both(nfilter: size(self%kbr1b_both%eq_b))%eq_b & 
        !         + sum(self%kbr1b_both(nfilter: size(self%kbr1b_both%eq_b))%ant_phase_corr_simu - &
        !             self%kbr1b_both(nfilter: size(self%kbr1b_both%eq_b))%eq_b) &
        !             / size(self%kbr1b_both(nfilter: size(self%kbr1b_both%eq_b)))
        ! 
        ! sequential_ls_loop: do i = 8, size(self%kbr1b_both) - nfilter, 1
        !     allocate(a(i, 8), stat=err)
        !     if (err /= 0) print *, "a: Allocation request denied"
        !     allocate(b(i), stat=err)
        !     if (err /= 0) print *, "b: Allocation request denied"
! 
        !     assign_ab_loop: do j = 1, i, 1
        !         a(j, :) = a_valid(j, :)
        !         b(j) = b_valid(j)
        !     end do assign_ab_loop
! 
        !     write(count_i, '(i10)') i
! 
        !     theta_n = ls_solver(a(:, 1: 6), b)
        !     if (norm2(theta_n - theta_n_1) < 1.0e-7) then
        !         call logger%info('phase_centre_vad', 'the phase centre validation equations are solved successfully')
        !         self%inverse_vector = theta_n(1: 6)
        !         temp = b - matmul(a(:, 1: 6), theta_n(1: 6))
        !         self%error(1) = sqrt(temp(2)**2 + temp(3)**2)
        !         self%error(2) = sqrt(temp(5)**2 + temp(6)**2)
        !         exit
        !     else
        !         call logger%trivia('phase_centre_vad', &
        !         'the solution does not reach the convergence condition, '//trim(count_i)//'th iteration continues...')
        !         self%inverse_vector = theta_n(1: 6)
        !         temp = b - matmul(a(:, 1: 6), theta_n(1: 6))
        !         self%error(1) = sqrt(temp(2)**2 + temp(3)**2)
        !         self%error(2) = sqrt(temp(5)**2 + temp(6)**2)
        !     end if
! 
        !     theta_n_1 = theta_n
        !     
        !     if (allocated(b)) deallocate(b, stat=err)
        !     if (err /= 0) print *, "b: Deallocation request denied"
        !     if (allocated(a)) deallocate(a, stat=err)
        !     if (err /= 0) print *, "a: Deallocation request denied"
        ! end do sequential_ls_loop
! 
        ! if (allocated(b_valid)) deallocate(b_valid, stat=err)
        ! if (err /= 0) print *, "b_valid: Deallocation request denied"
        ! if (allocated(a_valid)) deallocate(a_valid, stat=err)
        ! if (err /= 0) print *, "a_valid: Deallocation request denied"
        
    end subroutine solve_phase_centre_vad_eq

    subroutine create_phase_centre_vad_eq(self, i_index_motiv)
        class(satellite)    , INTENT(INOUT)                  :: self
        integer(kind=ip)    , INTENT(IN   ), OPTIONAL        :: i_index_motiv
        type(hashtable)                                      :: q_scac, q_scad, q_gnia, q_gnib
        integer(kind=ip)                                     :: i, ios, j, istat, fplerror
        integer(kind=ip)                                     :: tkbr, tscac, tscad, tgnia, tgnib
        real(kind=wp), ALLOCATABLE                           :: q_c(:), q_d(:), i_c(:), i_d(:)
        real(kind=wp)                                        :: range_res_ave
        real(kind=wp)                                        :: temp, wp_multi_factor, wp_rotm(3, 3)
        real(kind=wp)                                        :: intersatellite_range(size(self%kbr1b_both))
        character(len=14)                                    :: datenow
        character(len=1)                                     :: c_index_motiv
        character(len=:), ALLOCATABLE                        :: c_resultpath
        character(len=3000)                                  :: c_multipath
        type(io_file)                                        :: multi_file
        !type(pyplot)                                         :: plt
        
        
        datenow = yyyymmddhhmmss()
        !> read multipath factor
        fplerror = xml_i%urlpaths%GetAsString(key='ConfigPath', string=c_resultpath)
        c_multipath = c_resultpath//'multipath.txt'
        multi_file%name = c_multipath
        call multi_file%file_obj_init()
        open(unit=multi_file%unit, file=multi_file%name, iostat=ios, status="old", action="read")
        if ( ios /= 0 ) stop "Error opening file name"
        read(multi_file%unit, *)
        read(multi_file%unit, *) wp_multi_factor
        close(unit=multi_file%unit, iostat=ios)
        if ( ios /= 0 ) stop "Error closing file unit multi_file%unit"
        
        !> delete
        !open(unit=588, file='..//output//inertial_trajectory_2019-01-01_C_04.txt', iostat=ios, status="old", action="read")
        !if ( ios /= 0 ) stop "Error opening file name"
        !open(unit=899, file='..//output//inertial_trajectory_2019-01-01_D_04.txt', iostat=ios, status="old", action="read")
        !if ( ios /= 0 ) stop "Error opening file name"
        !read(588, *); read(899, *)
        !do i = 1, size(self%kbr1b_both), 1
        !    read(588, *) temp, self%kbr1b_both(i)%pos_i_lead
        !    read(899, *) temp, self%kbr1b_both(i)%pos_i_trac
        !end do
        !
        !close(unit=588, iostat=ios)
        !if ( ios /= 0 ) stop "Error closing file unit 588"
        !close(unit=899, iostat=ios)
        !if ( ios /= 0 ) stop "Error closing file unit 899"
        !> delete
        
        !open(unit=424, file='..//temp//trajectory'//datenow//'.txt', iostat=ios, status="unknown", action="write")
        !if ( ios /= 0 ) stop "Error opening file name"
        !> calculate the simulated inter-satellite range
        ! simu_range: do i = 1, size(self%kbr1b_both), 1
        !     self%kbr1b_both(i)%range_simu = norm2(self%kbr1b_both(i)%pos_i_lead - self%kbr1b_both(i)%pos_i_trac)
        !     !write(424, '(6f30.15)') self%kbr1b_both(i)%pos_i_lead, self%kbr1b_both(i)%pos_i_trac
        ! end do simu_range

        !>------------------------------------------------------------------------------------------
        !> obtain position vector in the inertial frame
        !> using hash table
        call q_gnia%init(nitems=size(self%gps1b_lead))
        call q_gnib%init(nitems=size(self%gps1b_trac))

        !> check if the nrows of sca_lead and sca_track are the same
        if (size(self%gps1b_lead) /= size(self%gps1b_trac)) then
            call logger%error('phase_centre_vad', 'the number of lines of two SCA files are different')
            call xml_o%xml2file(1, 'the number of lines of two SCA files are different')
            stop
        end if
        
        put_gps_value: do i = 1, size(self%gps1b_lead), 1
            tgnia = int(self%gps1b_lead(i)%gpst_gps1b)
            tgnib = int(self%gps1b_trac(i)%gpst_gps1b)
            call q_gnia%put(key=tgnia, rvals=self%gps1b_lead(i)%pos_i)
            call q_gnib%put(key=tgnib, rvals=self%gps1b_trac(i)%pos_i)
        end do put_gps_value

        create_eqb: do i = 1, size(self%kbr1b_both), 1
            !> quaternion to rotation matrix section
            tkbr = int(self%kbr1b_both(i)%gpst_kbr1b)
            if (q_gnia%has_key(tkbr)) then
                call q_gnia%get(key=tkbr, rvals=i_c)
                self%kbr1b_both(i)%pos_i_lead = i_c
            else
                call logger%error('phase_centre_vad', 'time tags of KBR data and KOE data not compatible')
                call xml_o%xml2file(1, 'time tags of KBR data and KOE data not compatible')
                stop
            end if
            if (q_gnib%has_key(tkbr)) then
                call q_gnib%get(key=tkbr, rvals=i_d)
                self%kbr1b_both(i)%pos_i_trac = i_d
            else
                call logger%error('phase_centre_vad', 'time tags of KBR data and KOE data not compatible')
                call xml_o%xml2file(1, "time tags of KBR data and KOE data not compatible")
                stop
            end if
            self%kbr1b_both(i)%range_pod = norm2(self%kbr1b_both(i)%pos_i_lead - self%kbr1b_both(i)%pos_i_trac)
        end do create_eqb

        !close(424)
        !stop

        !> calculate the residual between the biased range from KBR and the simulated inter-satellite range
        ! self%kbr1b_both%range_resi = -self%kbr1b_both%range + self%kbr1b_both%tof_range + self%kbr1b_both%range_simu
        self%kbr1b_both%range_resi = self%kbr1b_both%range + self%kbr1b_both%tof_range - self%kbr1b_both%range_pod
        ! call plt%initialize()
        ! call plt%add_plot(self%kbr1b_both%gpst_kbr1b, self%kbr1b_both%range_simu, label='$\sin(x)$', linestyle='b-o',markersize=5,linewidth=2, istat=istat)
        ! call plt%savefig('sinx.png', pyfile='sinx.py', ismat=.true., istat=istat)
        ! 
        ! call plt%initialize()
        ! call plt%add_plot(self%kbr1b_both%gpst_kbr1b, self%kbr1b_both%range, label='$\sin(x)$', linestyle='b-o',markersize=5,linewidth=2, ! istat=istat)
        ! call plt%savefig('sinx.png', pyfile='sinx.py', ismat=.true., istat=istat)
        ! 
        ! call plt%initialize()
        ! call plt%add_plot(self%kbr1b_both%gpst_kbr1b, self%kbr1b_both%range_resi, label='$\sin(x)$', linestyle='b-o',markersize=5,linewidth=2, istat=istat)
        ! call plt%savefig('sinx.png', pyfile='sinx.py', ismat=.true., istat=istat)
        ! print *, self%kbr1b_both%range_resi
        ! stop

        !> calculate the line-of-sight vector
        self%kbr1b_both%los_l2t(1) =  self%kbr1b_both%pos_i_lead(1) - self%kbr1b_both%pos_i_trac(1)
        self%kbr1b_both%los_l2t(2) =  self%kbr1b_both%pos_i_lead(2) - self%kbr1b_both%pos_i_trac(2)
        self%kbr1b_both%los_l2t(3) =  self%kbr1b_both%pos_i_lead(3) - self%kbr1b_both%pos_i_trac(3)
        self%kbr1b_both%los_t2l(1) = -self%kbr1b_both%los_l2t(1)
        self%kbr1b_both%los_t2l(2) = -self%kbr1b_both%los_l2t(2)
        self%kbr1b_both%los_t2l(3) = -self%kbr1b_both%los_l2t(3)

        !> calculate the inter-satellite range
        cal_inter_range: do i = 1, size(self%kbr1b_both), 1
            intersatellite_range(i) = norm2(self%kbr1b_both(i)%los_l2t)
            do j = 1, 3, 1
                self%kbr1b_both(i)%los_l2t(j) = self%kbr1b_both(i)%los_l2t(j) / intersatellite_range(i)
                self%kbr1b_both(i)%los_t2l(j) = self%kbr1b_both(i)%los_t2l(j) / intersatellite_range(i)
            end do
        end do cal_inter_range

        !> quaternion to rotation matrix
        !> using hash table
        call q_scac%init(nitems=size(self%sca1b_lead))
        call q_scad%init(nitems=size(self%sca1b_trac))

        !> check if the nrows of sca_lead and sca_track are the same
        if (size(self%sca1b_lead) /= size(self%sca1b_trac)) then
            call logger%error('phase_centre_vad', 'the number of lines of two SCA files are different')
            call xml_o%xml2file(1, 'the number of lines of two SCA files are different')
            stop
        end if
        
        put_sca_value: do i = 1, size(self%sca1b_lead), 1
            tscac = int(self%sca1b_lead(i)%gpst_sca1b)
            tscad = int(self%sca1b_trac(i)%gpst_sca1b)
            call q_scac%put(key=tscac, rvals=self%sca1b_lead(i)%quaternion)
            call q_scad%put(key=tscad, rvals=self%sca1b_trac(i)%quaternion)
        end do put_sca_value

        !> -----------------------------------------------------------------------------------------
        ! open(unit=424, file='..//temp//eq_a_'//datenow//'.txt', iostat=ios, status="unknown", position="append")
        ! if ( ios /= 0 ) stop "Error opening file name"
        !> -----------------------------------------------------------------------------------------
        range_res_ave = 0.0_wp
        create_eq: do i = 1, size(self%kbr1b_both), 1
            !> range residual average section
            range_res_ave = range_res_ave + self%kbr1b_both(i)%range_resi

            !> quaternion to rotation matrix section
            tkbr = int(self%kbr1b_both(i)%gpst_kbr1b)
            if (q_scac%has_key(tkbr)) then
                call q_scac%get(key=tkbr, rvals=q_c)
                self%kbr1b_both(i)%quaternion_c = q_c
                self%kbr1b_both(i)%rotm_c_i2s = q2m(self%kbr1b_both(i)%quaternion_c)
                self%kbr1b_both(i)%rotm_c_s2i = transpose(self%kbr1b_both(i)%rotm_c_i2s)
            else
                call logger%error('phase_centre_vad', 'time tags of KBR data and SCA data not compatible')
                call xml_o%xml2file(1, 'time tags of KBR data and SCA data not compatible')
                stop
            end if
            if (q_scad%has_key(tkbr)) then
                call q_scad%get(key=tkbr, rvals=q_d)
                self%kbr1b_both(i)%quaternion_d = q_d
                self%kbr1b_both(i)%rotm_d_i2s = q2m(self%kbr1b_both(i)%quaternion_d)
                self%kbr1b_both(i)%rotm_d_s2i = transpose(self%kbr1b_both(i)%rotm_d_i2s)
            else
                call logger%error('phase_centre_vad', 'time tags of KBR data and SCA data not compatible')
                call xml_o%xml2file(1, "time tags of KBR data and SCA data not compatible")
                stop
            end if

            !> assign matrix A in Ax=b
            self%kbr1b_both(i)%eq_a(1: 3) = matmul(self%kbr1b_both(i)%los_l2t, self%kbr1b_both(i)%rotm_c_s2i)
            self%kbr1b_both(i)%eq_a(4: 6) = matmul(self%kbr1b_both(i)%los_t2l, self%kbr1b_both(i)%rotm_d_s2i)
            ! print *, self%kbr1b_both(i)%eq_a(1: 3)

            ! write(424, '(10f40.20)') self%kbr1b_both(i)%eq_a
            !> euler angle
            !> leading satellite
            self%kbr1b_both(i)%wp_rotm_c_irf2losf = TRANSPOSE(losf2irf_rotm(self%kbr1b_both(i)%pos_i_lead, &
                                                                            self%kbr1b_both(i)%pos_i_trac))
            wp_rotm = matmul(self%kbr1b_both(i)%wp_rotm_c_irf2losf, self%kbr1b_both(i)%rotm_c_s2i)
            self%kbr1b_both(i)%wp_eul_c_srf2losf(1) =  atan(wp_rotm(3, 2) / wp_rotm(3, 3))
            self%kbr1b_both(i)%wp_eul_c_srf2losf(2) = -asin(wp_rotm(3, 1))
            self%kbr1b_both(i)%wp_eul_c_srf2losf(3) =  atan(wp_rotm(2, 1) / wp_rotm(1, 1))
            !> tracking satellite
            self%kbr1b_both(i)%wp_rotm_d_irf2losf = TRANSPOSE(losf2irf_rotm(self%kbr1b_both(i)%pos_i_trac, &
                                                                            self%kbr1b_both(i)%pos_i_lead))
            wp_rotm = matmul(self%kbr1b_both(i)%wp_rotm_d_irf2losf, self%kbr1b_both(i)%rotm_d_s2i)
            self%kbr1b_both(i)%wp_eul_d_srf2losf(1) =  atan(wp_rotm(3, 2) / wp_rotm(3, 3))
            self%kbr1b_both(i)%wp_eul_d_srf2losf(2) = -asin(wp_rotm(3, 1))
            self%kbr1b_both(i)%wp_eul_d_srf2losf(3) =  atan(wp_rotm(2, 1) / wp_rotm(1, 1))
        end do create_eq
        
        !> multipath error
        select case (i_index_motiv)
            case (1)
                self%kbr1b_both%wp_multipath_error = wp_multi_factor/1000.0_wp * self%kbr1b_both%wp_eul_c_srf2losf(3)
            case (2)
                self%kbr1b_both%wp_multipath_error = wp_multi_factor/1000.0_wp * self%kbr1b_both%wp_eul_c_srf2losf(2)
            case (3)
                self%kbr1b_both%wp_multipath_error = wp_multi_factor/1000.0_wp * self%kbr1b_both%wp_eul_d_srf2losf(3)
            case (4)
                self%kbr1b_both%wp_multipath_error = wp_multi_factor/1000.0_wp * self%kbr1b_both%wp_eul_d_srf2losf(2)
        end select

        !> -----------------------------------------------------------------------------------------
        ! close(unit=424, iostat=ios)
        ! if ( ios /= 0 ) stop "Error closing file unit 424"
        !> -----------------------------------------------------------------------------------------

        !> simulate the antenna phase correction
        ! call self%simu_ant_phase_corr()

        !> assign array b in Ax=b
        self%kbr1b_both%eq_b = self%kbr1b_both%range_resi - self%kbr1b_both%wp_multipath_error

        !>------------------------------------------------------------------------------------------
        !> output eq_b
        ! open(unit=424, file='..//temp//eq_b_'//datenow//'.txt', iostat=ios, status="unknown", position="append")
        ! if ( ios /= 0 ) stop "Error opening file name"
        ! write2file_eq_b: do i = 1, size(self%kbr1b_both), 1
        !     write(424, *) self%kbr1b_both(i)%eq_b
        ! end do write2file_eq_b
        ! close(424)

        !> create equation for 2-degree diff and 3-degree diff
        create_2degdiff_loop: do i = 1, size(self%kbr1b_2degdiff), 1
            self%kbr1b_2degdiff(i)%eq_a = self%kbr1b_both(i + 2)%eq_a + self%kbr1b_both(i)%eq_a - 2.0_wp * self%kbr1b_both(i + 1)%eq_a
            self%kbr1b_2degdiff(i)%eq_b = self%kbr1b_both(i + 2)%eq_b + self%kbr1b_both(i)%eq_b - 2.0_wp * self%kbr1b_both(i + 1)%eq_b
        end do create_2degdiff_loop
        
        create_3degdiff_loop: do i = 1, size(self%kbr1b_3degdiff), 1
            self%kbr1b_3degdiff(i)%eq_a = self%kbr1b_2degdiff(i + 1)%eq_a + self%kbr1b_2degdiff(i)%eq_a
            self%kbr1b_3degdiff(i)%eq_b = self%kbr1b_2degdiff(i + 1)%eq_b + self%kbr1b_2degdiff(i)%eq_b
        end do create_3degdiff_loop
        
        write(c_index_motiv, "(i1)") i_index_motiv
        call logger%info('phase_centre_vad', 'create the equations to be solved successfully of Motivation '//c_index_motiv)

    end subroutine create_phase_centre_vad_eq

    elemental subroutine gpst2jdtt(gpst, jdtt)
        real(kind=wp)             , intent(in   )        :: gpst
        real(kind=wp)             , intent(  out)        :: jdtt
        
        jdtt = (gpst + 19.0_wp + 32.184_wp) / 86400.0_wp + 2451545.0_wp
    end subroutine gpst2jdtt

    ! subroutine trajectory_forward(self, i_index_motiv)
    !     class(satellite)     , intent(inout)                 :: self
    !     integer(kind=ip)     , INTENT(IN   ), OPTIONAL       :: i_index_motiv
! 
    !     integer(kind=ip), parameter                          :: n      = 6_ip
    !     !> number of state variables
    !     real(kind=wp)   , parameter                          :: tol    = 1.0e-14_wp
    !     !> event location tolerance 
    !     integer(kind=ip)                                     :: idid, i, err, j, ios
    !     character(len=1)                                     :: c_index_motiv
    !     !> -----------------------------------------------------------------------------------------
! 
    !     !> convert gpst in acc1b to jdtt
    !     call gpst2jdtt(self%acc1b_lead%gpst_acc1b, self%acc1b_lead%jdtt)
    !     !> check whether the initial epoches of KBR1B and GNI1B are the same or not
    !     if (.not. isequal(self%gps1b_lead(1)%gpst_gps1b, self%gps1b_trac(1)%gpst_gps1b)) then
    !         call logger%error('phase_centre_vad', 'the initial time of GNI1B_C and GNI1B_D are not the same')
    !         call xml_o%xml2file(1, "the initial time of GNI1B_A and GNI1B_B are not the same")
    !         stop
    !     end if
! 
    !     if (.not. isequal(self%gps1b_lead(1)%gpst_gps1b, self%kbr1b_both(1)%gpst_kbr1b)) then
    !         call logger%error('phase_centre_vad', 'the initial time of GNI1B_C and KBR1B are not the same')
    !         call xml_o%xml2file(1, "the initial time of GNI1B_A and GNI1B_B are not the same")
    !         stop
    !     end if
! 
    !     !> raise information
    !     write(c_index_motiv, "(i1)") i_index_motiv
    !     call logger%info('phase_centre_vad', 'starting to simulate trajectories for both satellites of Motivation '//c_index_motiv)
! 
    !     !> simulate the trajectory
    !     call lead%initialize(n, maxnum=500000_ip, df=twobody, rtol=[1.0e-14_wp], atol=[1.0e-14_wp], report=twobody_report)
    !     call trac%initialize(n, maxnum=500000_ip, df=twobody, rtol=[1.0e-14_wp], atol=[1.0e-14_wp], report=twobody_report)
    !     !> transfer the initial epoch in gps time to julian date
    !     !> leadig satellite
    !     call gpst2jdtt(self%gps1b_lead(1)%gpst_gps1b, lead%date)
    !     !> tracking satellite
    !     call gpst2jdtt(self%gps1b_trac(1)%gpst_gps1b, trac%date)
    !     !> assign non-gravitational acceleration
    !     allocate(lead%acc_non_grav_gcrs(size(self%acc1b_lead), 3), stat=err)
    !     if (err /= 0) print *, "lead%acc_non_grav_gcrs: Allocation request denied"
    !     allocate(trac%acc_non_grav_gcrs(size(self%acc1b_lead), 3), stat=err)
    !     if (err /= 0) print *, "trac%acc_non_grav_gcrs: Allocation request denied"
    !     
    !     acc_srf2gcrs_loop: do i = 1, size(self%acc1b_lead), 1
    !         !> convert non-gravitational acceleration in SRF to the inertial frame
    !         lead%acc_non_grav_gcrs(i, :) = matmul(self%sca1b_lead(i)%rotm_s2i, self%acc1b_lead(i)%non_grav_acc)
    !         trac%acc_non_grav_gcrs(i, :) = matmul(self%sca1b_trac(i)%rotm_s2i, self%acc1b_trac(i)%non_grav_acc)
    !         !> factor the non-gravitational acceleration
    !         factor_acc_loop: do j = 1, 3, 1
    !             lead%acc_non_grav_gcrs(i, j) = lead%acc_non_grav_gcrs(i, j) * self%bf_lead(i) + self%af_lead(i)
    !             trac%acc_non_grav_gcrs(i, j) = trac%acc_non_grav_gcrs(i, j) * self%bf_trac(i) + self%af_trac(i)
    !         end do factor_acc_loop
    !     end do acc_srf2gcrs_loop
    !     !> -------end of transformation-------
! 
    !     !> leading satellite
    !     call lead%first_call()
    !     !> make sure the interval and the final epoch
    !     lead%dt = self%kbr1b_both(2)%gpst_kbr1b - self%kbr1b_both(1)%gpst_kbr1b
    !     lead%tf = self%kbr1b_both(size(self%kbr1b_both))%gpst_kbr1b - self%kbr1b_both(1)%gpst_kbr1b
    !     ! lead%tf = 86400.0_wp
    !     call lead%integrate(lead%t0, self%gps1b_lead(1)%pos_i, lead%tf, idid=idid, integration_mode=2, tstep=lead%dt)
    !     !> th%kbr1b_both%pos_i_kbr is obtained from the report subroutine
    !     do i = 1, size(self%kbr1b_both), 1
    !         do j = 1, 3, 1
    !             self%kbr1b_both(i)%pos_i_lead(j) = th%kbr1b_both(i)%pos_i_kbr(j)
    !         end do
    !     end do
    !     call logger%info('phase_centre_vad', 'the trajectory simulation of Motivation '//c_index_motiv//' of the leading satellite is done')
    !     !> -------end of leading satellite-------
! 
    !     !> tracking satellite
    !     call trac%first_call()
    !     !> make sure the interval and the final epoch
    !     trac%dt = self%kbr1b_both(2)%gpst_kbr1b - self%kbr1b_both(1)%gpst_kbr1b
    !     trac%tf = self%kbr1b_both(size(self%kbr1b_both))%gpst_kbr1b - self%kbr1b_both(1)%gpst_kbr1b
    !     ! trac%tf = 86400.0_wp
    !     call trac%integrate(trac%t0, self%gps1b_trac(1)%pos_i, trac%tf, idid=idid, integration_mode=2, tstep=trac%dt)
    !     !> th%kbr1b_both%pos_i_kbr is obtained from the report subroutine
    !     do i = 1, size(self%kbr1b_both), 1
    !         do j = 1, 3, 1
    !             self%kbr1b_both(i)%pos_i_trac(j) = th%kbr1b_both(i)%pos_i_kbr(j)
    !         end do
    !     end do
    !     call logger%info('phase_centre_vad', 'the trajectory simulation of Motivation '//c_index_motiv//' of the tracking satellite is done')
    !     !> -------end of tracking satellite-------
! 
    !     !> necessary deallocate
    !     if (allocated(trac%acc_non_grav_gcrs)) deallocate(trac%acc_non_grav_gcrs, stat=err)
    !     if (err /= 0) print *, "trac%acc_non_grav_gcrs: Deallocation request denied"
    !     if (allocated(lead%acc_non_grav_gcrs)) deallocate(lead%acc_non_grav_gcrs, stat=err)
    !     if (err /= 0) print *, "lead%acc_non_grav_gcrs: Deallocation request denied"
    !     
    !     !> delete
    !     ! open(unit=588, file='..//output//inertial_trajectory_2019-01-01_C_0411.txt', iostat=ios, status="old", action="write")
    !     ! if ( ios /= 0 ) stop "Error opening file name"
    !     ! open(unit=899, file='..//output//inertial_trajectory_2019-01-01_D_0411.txt', iostat=ios, status="old", action="write")
    !     ! if ( ios /= 0 ) stop "Error opening file name"
    !     ! 
    !     ! do i = 1, size(self%kbr1b_both), 1
    !     !     write(588, '(4f30.15)') self%kbr1b_both(i)%gpst_kbr1b, self%kbr1b_both(i)%pos_i_lead
    !     !     write(899, '(4f30.15)') self%kbr1b_both(i)%gpst_kbr1b, self%kbr1b_both(i)%pos_i_trac
    !     ! end do
    !     ! 
    !     ! close(unit=588, iostat=ios)
    !     ! if ( ios /= 0 ) stop "Error closing file unit 588"
    !     ! close(unit=899, iostat=ios)
    !     ! if ( ios /= 0 ) stop "Error closing file unit 899"
    !     !> delete
    !     
    ! end subroutine trajectory_forward
! 
    ! subroutine twobody(me, t_in, x_in, xdot)
    !     !! derivative routine for two-body orbit propagation
    !     implicit none
    !     class(ddeabm_class), intent(inout)           :: me
    !     real(kind=wp)           , intent(in   )      :: t_in
    !     real(kind=wp)           , intent(in   )      :: x_in(:)
    !     real(kind=wp)           , intent(  out)      :: xdot(:)
    !     real(kind=wp)                                :: residual_date
    !     integer(kind = 4)                            :: record_num
    !     real(kind = 8)                               :: RM_w(3,3)
    !     real(kind = 16)                              :: DUT1
    !     real(kind = 8)                               :: Position_sun(6)
    !     real(kind = 8)                               :: Position_moon(6)
    !     real(kind = 16)                              :: P_sun(6)
    !     real(kind = 16)                              :: P_moon(6)
    !     real(kind = 16)                              :: acc_solid_earth(3)
    ! 
    !     real(kind = 8)                               :: gamma
    !     real(kind = 8)                               :: beta
    !     real(kind=wp)                                     :: acc_non_grav(3)
    !     real(dp)                                     :: gcrs2itrs(3, 3)
    !     real(dp)                                     :: itrs2gcrs(3, 3)
    !     real(dp)                                     :: time_8
! 
    !     real(kind=wp)                                :: au = 149597870700.0_wp
    !     integer(kind=ip)                             :: i, err
! 
    !     gamma = 1.0_wp
    !     beta = 1.0_wp
! 
    !     residual_date = t_in / 86400.0_wp
! 
    !     select type (me)
    !     class is (spacecraft)
! 
    !         !> llocate(z_f(1), stat=err)
    !         !> f (err /= 0) print *, "z_f: Allocation request denied"
    !         !> _f(1)%name = './/input//GRACE_A_correct_2012-06-01.txt'
    !         !> _f(1)%nheader = 0
    !         !> _f(1)%unit = 256
    !         !> pen(unit=z_f(1)%unit, file=z_f(1)%name, iostat=ios, status="old", action="read")
    !         !> f ( ios /= 0 ) stop "Error opening file name"
! 
    !         !> _f(1)%nrow = get_file_n(z_f(1)%unit)
    !         !> ead_data_loop: do i = 1, z_f(1)%nrow, 1
    !         !>    read(z_f(1)%unit, *) me%non_grav(i, 1), me%non_grav(i, 2: 4)
    !         !>    me%non_grav(i, 1) = gpst2jdtt(s%non_grav(i, 1))
    !         !> nd do read_data_loop
! 
    !         !> lose(unit=z_f(1)%unit, iostat=ios)
    !         !> f ( ios /= 0 ) stop "Error closing file unit z_f"
! 
    !         !> f (allocated(z_f)) deallocate(z_f, stat=err)
    !         !> f (err /= 0) print *, "z_f: Deallocation request denied"
    !         
    !         time_8 = me%date + residual_date
    !         record_num = int((me%tf - me%t0) / me%dt + 1)
! 
    !         gl%r_inertial = x_in(1: 3)
    !         gl%v_inertial = x_in(4: 6)
! 
    !         call gcrs_itrs_m05(2_ip, [time_8], itrs2gcrs)
    !         call gcrs_itrs_m05(1_ip, [time_8], gcrs2itrs)
    !         gl%r_earth = matmul(gcrs2itrs, gl%r_inertial)
    !         !10: moon
    !         !11: sun
    !         call PLEPH ( time_8, 10, 3, Position_moon )
    !         call PLEPH ( time_8, 11, 3, Position_sun  )
    !         P_sun  = Position_sun
    !         P_moon = Position_moon
    !         p_sun(4: 6) = matmul(gcrs2itrs, p_sun(1: 3))
    !         P_moon(4: 6) = matmul(gcrs2itrs, P_moon(1: 3))
    !         call solid_tide_M04(1, [me%date + residual_date], gl%r_earth, p_sun(4: 6) * au, P_moon(4: 6) * au, acc_solid_earth)
    !         gl%acc_solid_inertial = matmul(itrs2gcrs, acc_solid_earth)
! 
    !         !> acceleration due to the third body
    !         call ThirdBodyPerturbation_M06(1_ip, p_sun(4: 6) * au, p_moon(4: 6) * au, gl%r_inertial, gl%acc_thre_inertial)
! 
    !         ! calculate the gravitional accelaration in the earth-fixed system
    !         call accxyz(gl%r_earth, gl%acc_grav_earth, me%degree, th%cs_coeffs%c_coeffs, th%cs_coeffs%s_coeffs)
! 
    !         gl%acc_grav_inertial = matmul(itrs2gcrs, gl%acc_grav_earth)
! 
    !         !! interpolation nongravtational
    !         ! acc_non_grav = 0.0_wp
    !         ! acc_non_grav(1) = linear_interp(me%non_grav(:, 1), me%non_grav(:, 2), real(me%date + residual_date, wp))
    !         ! acc_non_grav(2) = linear_interp(me%non_grav(:, 1), me%non_grav(:, 3), real(me%date + residual_date, wp))
    !         ! acc_non_grav(3) = linear_interp(me%non_grav(:, 1), me%non_grav(:, 4), real(me%date + residual_date, wp))
! 
    !         !> non-gravitational acceleration in the inertial frame
    !         interp_acc_gcrs_loop: do i = 1, 3, 1
    !             gl%acc_non_grav(i) = linear_interp(th%acc1b_lead%jdtt, me%acc_non_grav_gcrs(:, i), me%date + residual_date)
    !         end do interp_acc_gcrs_loop
! 
    !         xdot(1: 3) = gl%v_inertial
    !         xdot(4: 6) = gl%acc_grav_inertial! + gl%acc_thre_inertial + gl%acc_solid_inertial + gl%acc_non_grav
! 
    !         ! write(*,'(*(F30.15, 1X))') t , gl%r_earth, gl%acc_grav_inertial + gl%acc_rela_inertial
    !         me%fevals = me%fevals + 1
! 
    !     end select
    ! end subroutine twobody
    ! !***********************************************************************************************
    ! !***********************************************************************************************
    ! subroutine twobody_report(me, t_in, x_in)
    !     !! report function - write time, state to console
    !     implicit none
    !     class(ddeabm_class), intent(inout)          :: me
    !     real(kind=wp)           , intent(in   )     :: t_in
    !     real(kind=wp)           , intent(in   )     :: x_in(:)
    !     real(kind=wp), allocatable                  :: c_coef(:, :)
    !     real(kind=wp), allocatable                  :: s_coef(:, :)
    !     character(len = 80)                         :: output_file
    !     character(len = 80)                         :: earth_fixed_coor
    !     character(len = 80)                         :: potential_file
    !     character(len = 80)                         :: RM_file
    !     integer(kind = 8)                           :: file_unit_report
    !     integer(kind = 8)                           :: file_unit_earth
    !     integer(kind = 8)                           :: file_unit_potential
    !     integer(kind = 8)                           :: file_unit_RM
    !     real(kind = 8)                              :: RM_w(3,3)
    !     real(kind = 16)                             :: DUT1
    !     double precision                            :: residual_date
    !     character(len = 100)                        :: suffix
    !     real(dp)                                    :: gcrs2itrs(3, 3)
    !     real(dp)                                    :: time_8
! 
    !     integer(kind=ip)                            :: counter = 1_ip
! 
! 
    !     !suffix = '_60L_M2.txt'
    !     !residual_date = t_in / 86400.0
    !     !file_unit_report = 333
    !     !output_file = '../output/B-report.txt'
    !     !file_unit_earth = 67
    !     !earth_fixed_coor = 'output/A-coordinates_earth_fixed_system'//suffix
    !     !file_unit_potential = 96
    !     !potential_file = 'output/A-potential'//suffix
    !     !file_unit_RM = 117
    !     !RM_file = 'output/RM'//suffix
! 
    !     select type (me)
    !     class is (spacecraft)
    !         time_8 = me%date + residual_date
! 
    !         !allocate(c_coef(0: me%degree, 0: me%degree), s_coef(0: me%degree, 0: me%degree))
! 
    !         if (me%first) then  !print header
    !             !open(file_unit_report, file=output_file, status='replace')
    !             !open(file_unit_earth, file=earth_fixed_coor, status='replace')
    !             !open(file_unit_potential, file=potential_file, status='replace')
    !             !open(file_unit_RM, file=RM_file, status='replace')
! 
    !             !write(file_unit_report, '(*(A15, 1X))') 'time (sec)', 'x (m)', 'y (m)', 'z (m)'
    !             !write(file_unit_earth, '(*(A15, 1x))') 'x y z'
    !             !write(file_unit_potential, *) 'potential'
! 
    !             me%first = .false.
! 
    !             !close(file_unit_report)
    !             !close(file_unit_earth)
    !             !close(file_unit_potential)
    !             !close(file_unit_RM)
    !         end if
! 
    !         !open(file_unit_report, file=output_file, status='old', access='append')
    !         !open(file_unit_earth, file=earth_fixed_coor, status='old', access='append')
    !         !open(file_unit_potential, file=potential_file, status='old', access='append')
    !         !open(file_unit_RM, file=RM_file, status='old', access='append')
! 
    !         ! Print t and x vector
    !         !write(file_unit_report, '(*(f40.20, 1x))') me%date + residual_date, x_in
    !         me%final_state = x_in
! 
    !         ! print the positions in
    !         !if (me%w_time) then
    !         !    call ITRSandGCRS_P(1, me%date, residual_date, 37, x_in(1: 3), gl%r_earth, RM_w, DUT1)
    !         !end if
    !         !call gcrs_itrs_m05(1_ip, [time_8], gcrs2itrs)
    !         !gl%r_earth = matmul(gcrs2itrs, x_in(1: 3))
    !         !write(file_unit_earth, '(*(F30.15, 1X))') me%date + residual_date, gl%r_earth
    !         !write(file_unit_RM, '(*(F30.15, 1X))') me%date + residual_date, RM_w(1,:), RM_w(2,:), RM_w(3,:)
! 
    !         ! print potential
    !         !call ReadCS(C_coef, S_coef, me%degree)
    !         !call process(C_coef, S_coef, me%degree + 1, gl%r_earth, 0, me%degree, gl%potential)
    !         !write(file_unit_potential, *) gl%potential
! 
    !         ! write(*,'(*(F30.15, 1X))') t_in , gl%r_earth, gl%acc_grav_inertial + gl%acc_rela_inertial
    !         !close(file_unit_report)
    !         !close(file_unit_earth)
    !         !close(file_unit_potential)
    !         !close(file_unit_RM)
! 
    !         !deallocate(c_coef)
    !         !deallocate(s_coef)
! 
    !         th%kbr1b_both(counter)%pos_i_kbr = x_in(1: 3)
    !         counter = counter + 1_ip
    !         if (counter > (me%tf / me%dt + 1.0_wp)) counter = 1_ip
    !     end select
    ! end subroutine twobody_report

    subroutine destructor(self, returncode)
        class(satellite)          , intent(inout)        :: self
        integer(kind=ip)          , INTENT(inOUT)        :: returncode
        integer(kind=ip)                                 :: err

        if (allocated(self%kbr1b_both)) deallocate(self%kbr1b_both, stat=err)
        if (err /= 0) then 
            call logger%error('phase_centre_vad', "self%kbr1b_both: Deallocation request denied")
            call xml_o%xml2file(1, "self%kbr1b_both: Deallocation request denied")
            stop
        end if
        
        if (allocated(self%kbr1b_2degdiff)) deallocate(self%kbr1b_2degdiff, stat=err)
        if (err /= 0) then 
            call logger%error('phase_centre_vad', "self%kbr1b_2degdiff: Deallocation request denied")
            call xml_o%xml2file(1, "self%kbr1b_2degdiff: Deallocation request denied")
            stop
        end if
        
        if (allocated(self%kbr1b_3degdiff)) deallocate(self%kbr1b_3degdiff, stat=err)
        if (err /= 0) then 
            call logger%error('phase_centre_vad', "self%kbr1b_3degdiff: Deallocation request denied")
            call xml_o%xml2file(1, "self%kbr1b_3degdiff: Deallocation request denied")
            stop
        end if

        if (allocated(self%gps1b_lead)) deallocate(self%gps1b_lead, stat=err)
        if (err /= 0) then
            call logger%error('phase_centre_vad', "self%gps1b_lead: Deallocation request denied")
            call xml_o%xml2file(1, "self%gps1b_lead: Deallocation request denied")
            stop
        end if

        if (allocated(self%gps1b_trac)) deallocate(self%gps1b_trac, stat=err)
        if (err /= 0) then
            call logger%error('phase_centre_vad', "self%gps1b_trac: Deallocation request denied")
            call xml_o%xml2file(1, "self%gps1b_trac: Deallocation request denied")
            stop
        end if

        if (allocated(self%sca1b_lead)) deallocate(self%sca1b_lead, stat=err)
        if (err /= 0) then
            call logger%error('phase_centre_vad', "self%sca1b_lead: Deallocation request denied")
            call xml_o%xml2file(1, "self%sca1b_lead: Deallocation request denied")
            stop
        end if

        if (allocated(self%cs_coeffs%c_coeffs)) deallocate(self%cs_coeffs%c_coeffs, stat=err)
        if (err /= 0) then 
            call logger%error('phase_centre_vad', "self%cs_coeffs%c_coeffs(1: 60, 1: 60): Deallocation request denied")
            call xml_o%xml2file(1, "self%cs_coeffs(1: 60, 1: 60): Deallocation request denied")
            stop
        end if

        if (allocated(self%acc1b_trac)) deallocate(self%acc1b_trac, stat=err)
        if (err /= 0) then
            call logger%error('phase_centre_vad', "self%acc1b_trac: Deallocation request denied")
            call xml_o%xml2file(1, "self%acc1b_trac: Deallocation request denied")
            stop
        end if

        if (allocated(self%acc1b_lead)) deallocate(self%acc1b_lead, stat=err)
        if (err /= 0) then
            call logger%error('phase_centre_vad', "self%acc1b_lead: Deallocation request denied")
            call xml_o%xml2file(1, "self%acc1b_lead: Deallocation request denied")
            stop
        end if
        returncode = 0_ip
    end subroutine destructor

    subroutine initialise(self, dict, i_maneuver_time)
        class(satellite)     , intent(inout)             :: self
        type(parameterlist_t), intent(in   )             :: dict
        integer(kind=ip)     , intent(inout)             :: i_maneuver_time(:)

        type(parameterlistIterator_t)                    :: iterator
        type(io_file)                                    :: ifile
        type(io_file), allocatable                       :: ifiles(:)

        character(len=3000)                              :: flags(20), key, temp
        CHARACTER(len=3000), ALLOCATABLE                 :: value(:)

        integer(kind=ip)                                 :: i, ios, err, line, col, fplerror, ind, ip_ndata
        integer(kind=ip), ALLOCATABLE                    :: shape1(:)
        
        real(kind=wp)                                    :: reg, c_temp, s_temp

        !> data product flags
        flags = ['KBR1BFile', &
                 'ROI1B-AFile', 'ROI1B-BFile', &
                 'SCA1B-AFile', "SCA1B-BFile", &
                 'ACC1B-AFile', "ACC1B-BFile", &
                 'GKB1B-AFile', "GKB1B-BFile", &
                 'KOE1B-AFile', 'KOE1B-BFile', &
                 'THA1B-AFile', "THA1B-BFile", &
                 'VAC1B-AFile', "VAC1B-BFile", &
                 'GSM', &
                 'ResultPath', 'TempPath', 'LogPath', 'ConfigPath']

        iterator = dict%GetIterator()
        do while (.not. iterator%hasfinished())
            !> get key and its value
            key = Iterator%GetKey()
            !> checking the shape of a parameter
            fplerror = iterator%GetShape(shape1)
            !> allocate value
            allocate(value(shape1(1)), stat=err)
            if (err /= 0) print *, "value(shape): Allocation request denied"
            allocate(ifiles(shape1(1)), stat=err)
            if (err /= 0) print *, "ifiles: Allocation request denied"

            check_flag: if (.not. any(flags == key)) then
                call logger%error('phase_centre_vad', 'data product flag error, please check the xml file')
                call xml_o%xml2file(1, "data product flag error, please check the input xml file")
                stop
            end if check_flag
            if (iterator%get(value) /= 0) then
                call logger%error('phase_centre_vad', 'get the value of '//key//' error.')
                call xml_o%xml2file(1, "get the value of "//key//" error.")
                stop
            end if

            !> total lines
            total_lines_loop: do i = 1, size(value), 1
            !   > skip the directory
                if (key == 'ResultPath' .or. key == "TempPath" .or. key == "LogPath" .or. key == "ConfigPath") then
                    cycle
                end if
                !> init file obj
                ifiles(i)%name = trim(value(i))
                call ifiles(i)%file_obj_init()
            end do total_lines_loop
            

            !> init the file obj
            ifiles_loop: do ind = 1, size(value), 1
                !> ind cannot greater than 4
                if (ind > 4_ip) then
                    call logger%error('phase_centre_vad', 'You must perform the maneuvers within four days')
                    call xml_o%xml2file(1, 'You must perform the maneuvers within two days')
                    stop
                end if
                !> skip the directory
                if (key == 'ResultPath' .or. key == "TempPath" .or. key == "LogPath" .or. key == "ConfigPath") then
                    cycle
                end if

                !> open file and read data
                ifile%name = trim(value(ind))
                call ifile%file_obj_init()
                open(unit=ifile%unit, file=ifile%name, iostat=ios, status="old", action="read")
                if ( ios /= 0 ) then
                    call logger%error('phase_centre_vad', 'Error opening '//' '//trim(ifile%name))
                    call xml_o%xml2file(1, "Error opening "//" "//trim(ifile%name))
                    stop
                end if
                read_data: select case (key)
                    case ('VAC1B-AFile')
                        !> read in initial vector header
                        read_VAC1B_A_header: do i = 1, ifile%nheader, 1
                            read(ifile%unit, *) temp
                        end do read_VAC1B_A_header

                        !> read in factors
                        read(ifile%unit, *) self%bf_lead(1)
                        read(ifile%unit, *) self%bf_lead(2)
                        read(ifile%unit, *) self%bf_lead(3)
                        read(ifile%unit, *) self%af_lead(1)
                        read(ifile%unit, *) self%af_lead(2)
                        read(ifile%unit, *) self%af_lead(3)
                        
                        call logger%info('phase_centre_vad', 'read in VAC1B_A data successfully')
                    case ("VAC1B-BFile")
                        !> read in initial vector header
                        read_VAC1B_B_header: do i = 1, ifile%nheader, 1
                            read(ifile%unit, *) temp
                        end do read_VAC1B_B_header

                        !> read in factors
                        read(ifile%unit, *) self%bf_trac(1)
                        read(ifile%unit, *) self%bf_trac(2)
                        read(ifile%unit, *) self%bf_trac(3)
                        read(ifile%unit, *) self%af_trac(1)
                        read(ifile%unit, *) self%af_trac(2)
                        read(ifile%unit, *) self%af_trac(3)
                        
                        call logger%info('phase_centre_vad', 'read in VAC1B_B data successfully')
                    case ('KBR1BFile')
                        if (ind == 1) then
                            !> data lines in input files
                            ip_ndata = 0_ip
                            data_lines_kbr: do i = 1, size(value), 1
                                ip_ndata = ip_ndata + ifiles(i)%nrow - ifiles(i)%nheader
                            end do data_lines_kbr
                            !> allocate the kbr1b array
                            allocate(self%kbr1b_both(ip_ndata), stat=err)
                            allocate(self%kbr1b_2degdiff(ip_ndata - 8), stat=err)
                            allocate(self%kbr1b_3degdiff(ip_ndata - 12), stat=err)
                            
                            if (err /= 0) then
                                call logger%error("phase_centre_vad", "self%kbr1b_both: Allocation request denied")
                                call xml_o%xml2file(1, "self%kbr1b_both: Allocation request denied")
                                stop
                            end if
                            ip_ndata = 0_ip
                            !> read in kbr header
                            read_kbr_header: do i = 1, ifile%nheader, 1
                                read(ifile%unit, *) temp
                            end do read_kbr_header
                            !> read in kbr data
                            read_kbr_data: do i = 1, ifile%nrow - ifile%nheader, 1
                                read(ifile%unit, *) self%kbr1b_both(i)%gpst_kbr1b, temp, self%kbr1b_both(i)%range, &
                                                    self%kbr1b_both(i)%range_rate, self%kbr1b_both(i)%range_accl, &
                                                    self%kbr1b_both(i)%iono_corr,  self%kbr1b_both(i)%tof_range, &
                                                    self%kbr1b_both(i)%tof_rate,   self%kbr1b_both(i)%tof_accl
                                ip_ndata = ip_ndata + 1_ip
                                if (i > 2_ip) then
                                    if (.not. isequal(self%kbr1b_both(i)%gpst_kbr1b - self%kbr1b_both(i - 1)%gpst_kbr1b, &
                                                      self%kbr1b_both(i - 1)%gpst_kbr1b - self%kbr1b_both(i - 2)%gpst_kbr1b)) then
                                        call logger%error("phase_centre_vad", "Time stamp gap occurred in KBR1B file "//trim(real2str(self%kbr1b_both(i)%gpst_kbr1b)))
                                        call xml_o%xml2file(1, "Time stamp gap occurred in KBR1B file")
                                        stop
                                    end if
                                end if
                            end do read_kbr_data
                        else
                            !> read in kbr header
                            read_kbr_header_n: do i = 1, ifile%nheader, 1
                                read(ifile%unit, *) temp
                            end do read_kbr_header_n
                            !> read in kbr data
                            read_kbr_data_n: do i = 1+ip_ndata, ifile%nrow-ifile%nheader+ip_ndata, 1
                                read(ifile%unit, *) self%kbr1b_both(i)%gpst_kbr1b, temp, self%kbr1b_both(i)%range, &
                                                    self%kbr1b_both(i)%range_rate, self%kbr1b_both(i)%range_accl, &
                                                    self%kbr1b_both(i)%iono_corr,  self%kbr1b_both(i)%tof_range, &
                                                    self%kbr1b_both(i)%tof_rate,   self%kbr1b_both(i)%tof_accl
                                ip_ndata = ip_ndata + 1_ip
                                if (i > 2_ip+ip_ndata) then
                                    if (.not. isequal(self%kbr1b_both(i)%gpst_kbr1b - self%kbr1b_both(i - 1)%gpst_kbr1b, &
                                                      self%kbr1b_both(i - 1)%gpst_kbr1b - self%kbr1b_both(i - 2)%gpst_kbr1b)) then
                                        call logger%error("phase_centre_vad", "Time stamp gap occurred in KBR1B file "//trim(real2str(self%kbr1b_both(i)%gpst_kbr1b)))
                                        call xml_o%xml2file(1, "Time stamp gap occurred in KBR1B file")
                                        stop
                                    end if
                                end if
                            end do read_kbr_data_n
                        end if
                        call logger%info('phase_centre_vad', 'read in KBR1B_X data successfully')
                    case ('ROI1B-AFile')
                        if (ind == 1) then
                            !> data lines in input files
                            ip_ndata = 0_ip
                            data_lines_gni_a: do i = 1, size(value), 1
                                ip_ndata = ip_ndata + ifiles(i)%nrow - ifiles(i)%nheader
                            end do data_lines_gni_a
                            !> allocate the gps1b_lead array
                            if (.not. ALLOCATED(self%gps1b_lead)) then
                                allocate(self%gps1b_lead(ip_ndata), stat=err)
                                if (err /= 0) then
                                    call logger%error('phase_centre_vad', "self%gps1b_lead: Allocation request denied")
                                    call xml_o%xml2file(1, "self%gps1b_lead: Allocation request denied")
                                    stop
                                end if
                            end if
                            ip_ndata = 0_ip
                            !> read in gps1b_lead header
                            read_gni_lead_header: do i = 1, ifile%nheader, 1
                                read(ifile%unit, *) temp
                            end do read_gni_lead_header
                            !> read in gps1b_lead data
                            read_gni_lead_data: do i = 1, ifile%nrow - ifile%nheader, 1
                                read(ifile%unit, *) self%gps1b_lead(i)%gpst_gps1b, temp, temp, temp, &
                                                    self%gps1b_lead(i)%pos_i, reg, reg, reg,&
                                                    self%gps1b_lead(i)%vel_i
                                ip_ndata = ip_ndata + 1_ip
                                if (i > 2_ip) then
                                    if (.not. isequal(self%gps1b_lead(i)%gpst_gps1b - self%gps1b_lead(i - 1)%gpst_gps1b, &
                                                      self%gps1b_lead(i - 1)%gpst_gps1b - self%gps1b_lead(i - 2)%gpst_gps1b)) then
                                        call logger%error("phase_centre_vad", "Time stamp gap occurred in ROI1B file for the leading satellite")
                                        call xml_o%xml2file(1, "Time stamp gap occurred in ROI1B file for the leading satellite")
                                        stop
                                    end if
                                end if
                            end do read_gni_lead_data
                        else
                            !> read in gps1b_lead header
                            read_gni_lead_header_n: do i = 1, ifile%nheader, 1
                                read(ifile%unit, *) temp
                            end do read_gni_lead_header_n
                            !> read in gps1b_lead data
                            read_gni_lead_data_n: do i = 1+ip_ndata, ifile%nrow-ifile%nheader+ip_ndata, 1
                                read(ifile%unit, *) self%gps1b_lead(i)%gpst_gps1b, temp, temp, temp, &
                                                    self%gps1b_lead(i)%pos_i, reg, reg, reg,&
                                                    self%gps1b_lead(i)%vel_i
                                ip_ndata = ip_ndata + 1_ip
                                if (i > 2_ip+ip_ndata) then
                                    if (.not. isequal(self%gps1b_lead(i)%gpst_gps1b - self%gps1b_lead(i - 1)%gpst_gps1b, &
                                                      self%gps1b_lead(i - 1)%gpst_gps1b - self%gps1b_lead(i - 2)%gpst_gps1b)) then
                                        call logger%error("phase_centre_vad", "Time stamp gap occurred in ROI1B file for the leading satellite")
                                        call xml_o%xml2file(1, "Time stamp gap occurred in ROI1B file for the leading satellite")
                                        stop
                                    end if
                                end if
                            end do read_gni_lead_data_n
                        end if
                        call logger%info('phase_centre_vad', 'read in GNI1B_A data successfully')
                    CASE ('ROI1B-BFile')
                        if (ind == 1) then
                            !> data lines in input files
                            ip_ndata = 0_ip
                            data_lines_gni_b: do i = 1, size(value), 1
                                ip_ndata = ip_ndata + ifiles(i)%nrow - ifiles(i)%nheader
                            end do data_lines_gni_b
                            !> allocate the gps1b_trac array
                            if (.not. ALLOCATED(self%gps1b_trac)) then
                                allocate(self%gps1b_trac(ip_ndata), stat=err)
                                if (err /= 0) then
                                    call logger%error('phase_centre_vad', "self%gps1b_trac: Allocation request denied")
                                    call xml_o%xml2file(1, "self%gps1b_trac: Allocation request denied")
                                    stop
                                end if
                            end if
                            ip_ndata = 0_ip
                            !> read in gps1b_trac header
                            read_gni_trac_header: do i = 1, ifile%nheader, 1
                                read(ifile%unit, *) temp
                            end do read_gni_trac_header
                            !> read in gps1b_trac data
                            read_gni_trac_data: do i = 1, ifile%nrow - ifile%nheader, 1
                                read(ifile%unit, *) self%gps1b_trac(i)%gpst_gps1b, temp, temp, temp, &
                                                    self%gps1b_trac(i)%pos_i, reg, reg, reg, &
                                                    self%gps1b_trac(i)%vel_i
                                ip_ndata = ip_ndata + 1_ip
                                if (i > 2_ip) then
                                    if (.not. isequal(self%gps1b_trac(i)%gpst_gps1b - self%gps1b_trac(i - 1)%gpst_gps1b, &
                                                      self%gps1b_trac(i - 1)%gpst_gps1b - self%gps1b_trac(i - 2)%gpst_gps1b)) then
                                        call logger%error("phase_centre_vad", "Time stamp gap occurred in ROI1B file for the tracking satellite")
                                        call xml_o%xml2file(1, "Time stamp gap occurred in ROI1B file for the tracking satellite")
                                        stop
                                    end if
                                end if
                            end do read_gni_trac_data
                        else
                            !> read in gps1b_trac header
                            read_gni_trac_header_n: do i = 1, ifile%nheader, 1
                                read(ifile%unit, *) temp
                            end do read_gni_trac_header_n
                            !> read in gps1b_trac data
                            read_gni_trac_data_n: do i = 1+ip_ndata, ifile%nrow-ifile%nheader+ip_ndata, 1
                                read(ifile%unit, *) self%gps1b_trac(i)%gpst_gps1b, temp, temp, temp, &
                                                    self%gps1b_trac(i)%pos_i, reg, reg, reg, &
                                                    self%gps1b_trac(i)%vel_i
                                ip_ndata = ip_ndata + 1_ip
                                if (i > 2_ip+ip_ndata) then
                                    if (.not. isequal(self%gps1b_trac(i)%gpst_gps1b - self%gps1b_trac(i - 1)%gpst_gps1b, &
                                                      self%gps1b_trac(i - 1)%gpst_gps1b - self%gps1b_trac(i - 2)%gpst_gps1b)) then
                                        call logger%error("phase_centre_vad", "Time stamp gap occurred in ROI1B file for the tracking satellite")
                                        call xml_o%xml2file(1, "Time stamp gap occurred in ROI1B file for the tracking satellite")
                                        stop
                                    end if
                                end if
                            end do read_gni_trac_data_n
                        end if
                        call logger%info('phase_centre_vad', 'read in GNI1B_B data successfully')
                    
                    case ('KOE1B-AFile')
                        if (ind == 1) then
                            !> data lines in input files
                            ip_ndata = 0_ip
                            data_lines_gnv_a: do i = 1, size(value), 1
                                ip_ndata = ip_ndata + ifiles(i)%nrow - ifiles(i)%nheader
                            end do data_lines_gnv_a
                            !> allocate the gps1b_lead array
                            if (.not. ALLOCATED(self%gps1b_lead)) then
                                allocate(self%gps1b_lead(ip_ndata), stat=err)
                                if (err /= 0) then
                                    call logger%error('phase_centre_vad', "self%gps1b_lead: Allocation request denied")
                                    call xml_o%xml2file(1, "self%gps1b_lead: Allocation request denied")
                                    stop
                                end if
                            end if
                            ip_ndata = 0_ip
                            !> read in gps1b_lead header
                            read_gnv_lead_header: do i = 1, ifile%nheader, 1
                                read(ifile%unit, *) temp
                            end do read_gnv_lead_header
                            !> read in gps1b_lead data
                            read_gnv_lead_data: do i = 1, ifile%nrow-ifile%nheader, 1
                                read(ifile%unit, *) self%gps1b_lead(i)%gpst_gps1b, temp, temp, temp, &
                                                    self%gps1b_lead(i)%pos_e, reg, reg, reg,&
                                                    self%gps1b_lead(i)%vel_e
                                ip_ndata = ip_ndata + 1_ip
                                if (i > 2_ip) then
                                    if (.not. isequal(self%gps1b_lead(i)%gpst_gps1b - self%gps1b_lead(i - 1)%gpst_gps1b, &
                                                      self%gps1b_lead(i - 1)%gpst_gps1b - self%gps1b_lead(i - 2)%gpst_gps1b)) then
                                        call logger%error("phase_centre_vad", "Time stamp gap occurred in ROI1B file for the leading satellite")
                                        call xml_o%xml2file(1, "Time stamp gap occurred in ROI1B file for the leading satellite")
                                        stop
                                    end if
                                end if
                            end do read_gnv_lead_data
                        else
                            !> read in gps1b_lead header
                            read_gnv_lead_header_n: do i = 1, ifile%nheader, 1
                                read(ifile%unit, *) temp
                            end do read_gnv_lead_header_n
                            !> read in gps1b_lead data
                            read_gnv_lead_data_n: do i = 1+ip_ndata, ifile%nrow-ifile%nheader+ip_ndata, 1
                                read(ifile%unit, *) self%gps1b_lead(i)%gpst_gps1b, temp, temp, temp, &
                                                    self%gps1b_lead(i)%pos_e, reg, reg, reg,&
                                                    self%gps1b_lead(i)%vel_e
                                ip_ndata = ip_ndata + 1_ip
                                if (i > 2_ip+ip_ndata) then
                                    if (.not. isequal(self%gps1b_lead(i)%gpst_gps1b - self%gps1b_lead(i - 1)%gpst_gps1b, &
                                                      self%gps1b_lead(i - 1)%gpst_gps1b - self%gps1b_lead(i - 2)%gpst_gps1b)) then
                                        call logger%error("phase_centre_vad", "Time stamp gap occurred in ROI1B file for the leading satellite")
                                        call xml_o%xml2file(1, "Time stamp gap occurred in ROI1B file for the leading satellite")
                                        stop
                                    end if
                                end if
                            end do read_gnv_lead_data_n
                        end if
                        call logger%info('phase_centre_vad', 'read in GNV1B_A data successfully')
                    CASE ('KOE1B-BFile')
                        if (ind == 1) then
                            !> data lines in input files
                            ip_ndata = 0_ip
                            data_lines_gnv_b: do i = 1, size(value), 1
                                ip_ndata = ip_ndata + ifiles(i)%nrow - ifiles(i)%nheader
                            end do data_lines_gnv_b
                            !> allocate the gps1b_trac array
                            if (.not. ALLOCATED(self%gps1b_trac)) then
                                allocate(self%gps1b_trac(ip_ndata), stat=err)
                                if (err /= 0) then
                                    call logger%error('phase_centre_vad', "self%gps1b_trac: Allocation request denied")
                                    call xml_o%xml2file(1, "self%gps1b_trac: Allocation request denied")
                                    stop
                                end if
                            end if
                            ip_ndata = 0_ip
                            !> read in gps1b_trac header
                            read_gnv_trac_header: do i = 1, ifile%nheader, 1
                                read(ifile%unit, *) temp
                            end do read_gnv_trac_header
                            !> read in gps1b_trac data
                            read_gnv_trac_data: do i = 1, ifile%nrow - ifile%nheader, 1
                                read(ifile%unit, *) self%gps1b_trac(i)%gpst_gps1b, temp, temp, temp, &
                                                    self%gps1b_trac(i)%pos_e, reg, reg, reg, &
                                                    self%gps1b_trac(i)%vel_e
                                ip_ndata = ip_ndata + 1_ip
                                if (i > 2_ip) then
                                    if (.not. isequal(self%gps1b_trac(i)%gpst_gps1b - self%gps1b_trac(i - 1)%gpst_gps1b, &
                                                      self%gps1b_trac(i - 1)%gpst_gps1b - self%gps1b_trac(i - 2)%gpst_gps1b)) then
                                        call logger%error("phase_centre_vad", "Time stamp gap occurred in ROI1B file for the tracking satellite")
                                        call xml_o%xml2file(1, "Time stamp gap occurred in ROI1B file for the tracking satellite")
                                        stop
                                    end if
                                end if
                            end do read_gnv_trac_data
                        else
                            !> read in gps1b_trac header
                            read_gnv_trac_header_n: do i = 1, ifile%nheader, 1
                                read(ifile%unit, *) temp
                            end do read_gnv_trac_header_n
                            !> read in gps1b_trac data
                            read_gnv_trac_data_n: do i = 1+ip_ndata, ifile%nrow-ifile%nheader+ip_ndata, 1
                                read(ifile%unit, *) self%gps1b_trac(i)%gpst_gps1b, temp, temp, temp, &
                                                    self%gps1b_trac(i)%pos_e, reg, reg, reg, &
                                                    self%gps1b_trac(i)%vel_e
                                ip_ndata = ip_ndata + 1_ip
                                if (i > 2_ip+ip_ndata) then
                                    if (.not. isequal(self%gps1b_trac(i)%gpst_gps1b - self%gps1b_trac(i - 1)%gpst_gps1b, &
                                                      self%gps1b_trac(i - 1)%gpst_gps1b - self%gps1b_trac(i - 2)%gpst_gps1b)) then
                                        call logger%error("phase_centre_vad", "Time stamp gap occurred in ROI1B file for the tracking satellite")
                                        call xml_o%xml2file(1, "Time stamp gap occurred in ROI1B file for the tracking satellite")
                                        stop
                                    end if
                                end if
                            end do read_gnv_trac_data_n
                        end if
                        call logger%info('phase_centre_vad', 'read in GNV1B_B data successfully')
                    
                    case ('SCA1B-AFile')
                        if (ind == 1) then
                            !> data lines in input files
                            ip_ndata = 0_ip
                            data_lines_sca_a: do i = 1, size(value), 1
                                ip_ndata = ip_ndata + ifiles(i)%nrow - ifiles(i)%nheader
                            end do data_lines_sca_a
                            !> allocate the sca1b_c array
                            allocate(self%sca1b_lead(ip_ndata), stat=err)
                            if (err /= 0) then
                                call logger%error('phase_centre_vad', "self%sca1b_lead: Allocation request denied")
                                call xml_o%xml2file(1, "self%sca1b_lead: Allocation request denied")
                                stop
                            end if
                            ip_ndata = 0_ip
                            !> read in sca1b_lead header
                            read_sca_lead_header: do i = 1, ifile%nheader, 1
                                read(ifile%unit, *) temp
                            end do read_sca_lead_header
                            !> read in sca1b_lead data
                            read_sca_lead_data: do i = 1, ifile%nrow - ifile%nheader, 1
                                read(ifile%unit, *) self%sca1b_lead(i)%gpst_sca1b, temp, temp, reg, &
                                                    self%sca1b_lead(i)%quaternion
                                ip_ndata = ip_ndata + 1_ip
                                !> convert quaternion to rotation matrix
                                self%sca1b_lead(i)%rotm_i2s = q2m(self%sca1b_lead(i)%quaternion)
                                self%sca1b_lead(i)%rotm_s2i = TRANSPOSE(self%sca1b_lead(i)%rotm_i2s)
                                !> check if the time stamp gap exists
                                if (i > 2_ip) then
                                    if (.not. isequal(self%sca1b_lead(i)%gpst_sca1b - self%sca1b_lead(i - 1)%gpst_sca1b, &
                                                      self%sca1b_lead(i - 1)%gpst_sca1b - self%sca1b_lead(i - 2)%gpst_sca1b)) then
                                        call logger%error("phase_centre_vad", "Time stamp gap occurred in SCA1B file for the leading satellite")
                                        call xml_o%xml2file(1, "Time stamp gap occurred in SCA1B file for the leading satellite")
                                        stop
                                    end if
                                end if
                            end do read_sca_lead_data
                        else
                            !> read in sca1b_lead header
                            read_sca_lead_header_n: do i = 1, ifile%nheader, 1
                                read(ifile%unit, *) temp
                            end do read_sca_lead_header_n
                            !> read in sca1b_lead data
                            read_sca_lead_data_n: do i = 1+ip_ndata, ifile%nrow-ifile%nheader+ip_ndata, 1
                                read(ifile%unit, *) self%sca1b_lead(i)%gpst_sca1b, temp, temp, reg, &
                                                    self%sca1b_lead(i)%quaternion
                                ip_ndata = ip_ndata + 1_ip
                                !> convert quaternion to rotation matrix
                                self%sca1b_lead(i)%rotm_i2s = q2m(self%sca1b_lead(i)%quaternion)
                                self%sca1b_lead(i)%rotm_s2i = TRANSPOSE(self%sca1b_lead(i)%rotm_i2s)
                                !> check if the time stamp gap exists
                                if (i > 2_ip+ip_ndata) then
                                    if (.not. isequal(self%sca1b_lead(i)%gpst_sca1b - self%sca1b_lead(i - 1)%gpst_sca1b, &
                                                      self%sca1b_lead(i - 1)%gpst_sca1b - self%sca1b_lead(i - 2)%gpst_sca1b)) then
                                        call logger%error("phase_centre_vad", "Time stamp gap occurred in SCA1B file for the leading satellite")
                                        call xml_o%xml2file(1, "Time stamp gap occurred in SCA1B file for the leading satellite")
                                        stop
                                    end if
                                end if
                            end do read_sca_lead_data_n
                        end if
                        call logger%info('phase_centre_vad', 'read in SCA1B_A data successfully')
                    case ('SCA1B-BFile')
                        if (ind == 1) then
                            !> data lines in input files
                            ip_ndata = 0_ip
                            data_lines_sca_b: do i = 1, size(value), 1
                                ip_ndata = ip_ndata + ifiles(i)%nrow - ifiles(i)%nheader
                            end do data_lines_sca_b
                            !> allocate the sca1b_D array
                            allocate(self%sca1b_TRAC(ip_ndata), stat=err)
                            if (err /= 0) then
                                call logger%error('phase_centre_vad', "self%sca1b_trac: Allocation request denied")
                                call xml_o%xml2file(1, "self%sca1b_trac: Allocation request denied")
                                stop
                            end if
                            ip_ndata = 0_ip
                            !> read in sca1b_trac header
                            read_sca_trac_header: do i = 1, ifile%nheader, 1
                                read(ifile%unit, *) temp
                            end do read_sca_trac_header
                            !> read in sca1b_trac data
                            read_sca_trac_data: do i = 1, ifile%nrow - ifile%nheader, 1
                                read(ifile%unit, *) self%sca1b_trac(i)%gpst_sca1b, temp, temp, reg, &
                                                    self%sca1b_trac(i)%quaternion
                                ip_ndata = ip_ndata + 1_ip
                                !> convert quaternion to rotation matrix
                                self%sca1b_trac(i)%rotm_i2s = q2m(self%sca1b_trac(i)%quaternion)
                                self%sca1b_trac(i)%rotm_s2i = TRANSPOSE(self%sca1b_trac(i)%rotm_i2s)
                                !> check if the time stamp gap exists
                                if (i > 2_ip) then
                                    if (.not. isequal(self%sca1b_trac(i)%gpst_sca1b - self%sca1b_trac(i - 1)%gpst_sca1b, &
                                                      self%sca1b_trac(i - 1)%gpst_sca1b - self%sca1b_trac(i - 2)%gpst_sca1b)) then
                                        call logger%error("phase_centre_vad", "Time stamp gap occurred in SCA1B file for the tracking satellite")
                                        call xml_o%xml2file(1, "Time stamp gap occurred in SCA1B file for the tracking satellite")
                                        stop
                                    end if
                                end if
                            end do read_sca_trac_data
                        else
                            !> read in sca1b_trac header
                            read_sca_trac_header_n: do i = 1, ifile%nheader, 1
                                read(ifile%unit, *) temp
                            end do read_sca_trac_header_n
                            !> read in sca1b_trac data
                            read_sca_trac_data_n: do i = 1+ip_ndata, ifile%nrow-ifile%nheader+ip_ndata, 1
                                read(ifile%unit, *) self%sca1b_trac(i)%gpst_sca1b, temp, temp, reg, &
                                                    self%sca1b_trac(i)%quaternion
                                ip_ndata = ip_ndata + 1_ip
                                !> convert quaternion to rotation matrix
                                self%sca1b_trac(i)%rotm_i2s = q2m(self%sca1b_trac(i)%quaternion)
                                self%sca1b_trac(i)%rotm_s2i = TRANSPOSE(self%sca1b_trac(i)%rotm_i2s)
                                !> check if the time stamp gap exists
                                if (i > 2_ip+ip_ndata) then
                                    if (.not. isequal(self%sca1b_trac(i)%gpst_sca1b - self%sca1b_trac(i - 1)%gpst_sca1b, &
                                                      self%sca1b_trac(i - 1)%gpst_sca1b - self%sca1b_trac(i - 2)%gpst_sca1b)) then
                                        call logger%error("phase_centre_vad", "Time stamp gap occurred in SCA1B file for the tracking satellite")
                                        call xml_o%xml2file(1, "Time stamp gap occurred in SCA1B file for the tracking satellite")
                                        stop
                                    end if
                                end if
                            end do read_sca_trac_data_n
                        end if
                        call logger%info('phase_centre_vad', 'read in SCA1B_B data successfully')
                    case ('ACC1B-AFile')
                        if (ind == 1) then
                            !> data lines in input files
                            ip_ndata = 0_ip
                            data_lines_acc_a: do i = 1, size(value), 1
                                ip_ndata = ip_ndata + ifiles(i)%nrow - ifiles(i)%nheader
                            end do data_lines_acc_a
                            !> allocate the acc1b_lead array
                            allocate(self%acc1b_lead(ip_ndata), stat=err)
                            if (err /= 0) then
                                call logger%error('phase_centre_vad', "self%acc1b_lead: Allocation request denied")
                                call xml_o%xml2file(1, "self%acc1b_lead: Allocation request denied")
                                stop
                            end if
                            ip_ndata = 0_ip
                            !> read in acc1b_lead header
                            read_acc_lead_header: do i = 1, ifile%nheader, 1
                                read(ifile%unit, *) temp
                            end do read_acc_lead_header
                            !> read in acc1b_lead data
                            read_acc_lead_data: do i = 1, ifile%nrow - ifile%nheader, 1
                                read(ifile%unit, *) self%acc1b_lead(i)%gpst_acc1b, temp, temp, &
                                                    self%acc1b_lead(i)%non_grav_acc
                                ip_ndata = ip_ndata + 1_ip
                            end do read_acc_lead_data
                        else
                            !> read in acc1b_lead header
                            read_acc_lead_header_n: do i = 1, ifile%nheader, 1
                                read(ifile%unit, *) temp
                            end do read_acc_lead_header_n
                            !> read in acc1b_lead data
                            read_acc_lead_data_n: do i = ip_ndata+1, ifile%nrow-ifile%nheader+ip_ndata, 1
                                read(ifile%unit, *) self%acc1b_lead(i)%gpst_acc1b, temp, temp, &
                                                    self%acc1b_lead(i)%non_grav_acc
                                ip_ndata = ip_ndata + 1_ip
                            end do read_acc_lead_data_n
                        end if
                        
                        call logger%info('phase_centre_vad', 'read in ACC1B_A data successfully')
                    case ("ACC1B-BFile")
                        if (ind == 1) then
                            !> data lines in input files
                            ip_ndata = 0_ip
                            data_lines_acc_b: do i = 1, size(value), 1
                                ip_ndata = ip_ndata + ifiles(i)%nrow - ifiles(i)%nheader
                            end do data_lines_acc_b
                            !> allocate the acc1b_trac array
                            allocate(self%acc1b_trac(ip_ndata), stat=err)
                            if (err /= 0) then
                                call logger%error('phase_centre_vad', "self%acc1b_trac: Allocation request denied")
                                call xml_o%xml2file(1, "self%acc1b_trac: Allocation request denied")
                                stop
                            end if
                            ip_ndata = 0_ip
                            !> read in acc1b_trac header
                            read_acc_trac_header: do i = 1, ifile%nheader, 1
                                read(ifile%unit, *) temp
                            end do read_acc_trac_header
                            !> read in acc1b_trac data
                            read_acc_trac_data: do i = 1, ifile%nrow - ifile%nheader, 1
                                read(ifile%unit, *) self%acc1b_trac(i)%gpst_acc1b, temp, temp, &
                                                    self%acc1b_trac(i)%non_grav_acc
                                ip_ndata = ip_ndata + 1_ip
                            end do read_acc_trac_data
                        else
                            !> read in acc1b_trac header
                            read_acc_trac_header_n: do i = 1, ifile%nheader, 1
                                read(ifile%unit, *) temp
                            end do read_acc_trac_header_n
                            !> read in acc1b_trac data
                            read_acc_trac_data_n: do i = ip_ndata+1, ifile%nrow-ifile%nheader+ip_ndata, 1
                                read(ifile%unit, *) self%acc1b_trac(i)%gpst_acc1b, temp, temp, &
                                                    self%acc1b_trac(i)%non_grav_acc
                                ip_ndata = ip_ndata + 1_ip
                            end do read_acc_trac_data_n
                        end if
                        call logger%info('phase_centre_vad', 'read in ACC1B_B data successfully')
                    case ('GSM')
                        !> allocate the stocks coefficients array
                        allocate(self%cs_coeffs%c_coeffs(0: lead%degree, 0: lead%degree), stat=err)
                        ALLOCATE(self%cs_coeffs%c_coeffs_1d(1: (lead%degree + 2) * (lead%degree + 1) / 2), stat=err)
                        if (err /= 0) then
                            call logger%error('phase_centre_vad', "self%cs_coeffs%c_coeffs(1: 60, 1: 60): Allocation request denied")
                            call xml_o%xml2file(1, "self%cs_coeffs%c_coeffs(1: 60, 1: 60): Allocation request denied")
                            stop
                        end if
                        allocate(self%cs_coeffs%s_coeffs(0: lead%degree, 0: lead%degree), stat=err)
                        ALLOCATE(self%cs_coeffs%s_coeffs_1d(1: (lead%degree + 2) * (lead%degree + 1) / 2), stat=err)
                        if (err /= 0) then
                            call logger%error('phase_centre_vad', "self%cs_coeffs%s_coeffs(1: 60, 1: 60): Allocation request denied")
                            call xml_o%xml2file(1, "self%cs_coeffs%s_coeffs(1: 60, 1: 60): Allocation request denied")
                            stop
                        end if

                        !> read in stocks header
                        read_cs_coeffs_header: do i = 1, ifile%nheader, 1
                            read(ifile%unit, *) temp
                        end do read_cs_coeffs_header

                        !> read in stocks data
                        read_cs_coeffs_data: do i = 1, ifile%nrow - ifile%nheader, 1
                            read(ifile%unit, *) line, col, self%cs_coeffs%c_coeffs_1d(i), self%cs_coeffs%s_coeffs_1d(i)
                            !> check if quit
                            if (i > (lead%degree + 1) * (lead%degree + 2) / 2) exit
                            self%cs_coeffs%c_coeffs(line, col) = self%cs_coeffs%c_coeffs_1d(i)
                            self%cs_coeffs%s_coeffs(line, col) = self%cs_coeffs%s_coeffs_1d(i)
                        end do read_cs_coeffs_data
                        call logger%info('phase_centre_vad', 'read in GSMGRFO data successfully')
                    case ('GKB1B-AFile')
                        !> read in initial vector header
                        read_GKB1B_A_header: do i = 1, ifile%nheader, 1
                            read(ifile%unit, *) temp
                        end do read_GKB1B_A_header

                        !> read in initial vectors
                        read_GKB1B_A_data: do i = 1, 3, 1
                            read(ifile%unit, *) self%initial_vector_ka(i), self%initial_vector_k(i)
                        end do read_GKB1B_A_data
                        call logger%info('phase_centre_vad', 'read in GKB1B_A data successfully')
                    case ("GKB1B-BFile")
                        !> read in initial vector header
                        read_GKB1B_B_header: do i = 1, ifile%nheader, 1
                            read(ifile%unit, *) temp
                        end do read_GKB1B_B_header

                        !> read in initial vectors
                        read_GKB1B_B_data: do i = 4, 6, 1
                            read(ifile%unit, *) self%initial_vector_ka(i), self%initial_vector_k(i)
                        end do read_GKB1B_B_data
                        call logger%info('phase_centre_vad', 'read in GKB1B_B data successfully')
                    case ('KBR1AFile')
                    
                    case ('KHK1AFile')
                    
                    case ('KHK1BFile')
                    case ('ConfigPath')
                    case ('THA1B-AFile')
                        !> allocate the tha1b_lead array
                        ! allocate(self%tha1b_lead(ifile%nrow - ifile%nheader), stat=err)
                        ! if (err /= 0) then
                        !     call logger%error('phase_centre_vad', "self%tha1b_lead: Allocation request denied")
                        !     call xml_o%xml2file(1, "self%tha1b_lead: Allocation request denied")
                        !     stop
                        ! end if
                        ! !> read in tha1b_lead header
                        ! read_tha_lead_header: do i = 1, ifile%nheader, 1
                        !     read(ifile%unit, *) temp
                        ! end do read_tha_lead_header
                        ! !> read in tha1b_lead data
                        ! read_tha_lead_data: do i = 1, ifile%nrow - ifile%nheader, 1
                        !     read(ifile%unit, *) self%tha1b_lead(i)%gpst_intg, self%tha1b_lead(i)%gpst_frac, temp, temp, temp, &
                        !                         self%tha1b_lead(i)%thruster_time, self%tha1b_lead(i)%accum_time
                        !     self%tha1b_lead(i)%gpst = self%tha1b_lead(i)%gpst_intg + self%tha1b_lead(i)%gpst_frac * 1e-6_wp
                        !     
                        ! end do read_tha_lead_data
                        ! call logger%info('phase_centre_vad', 'read in THA1B_A data successfully')
                    case ("THA1B-BFile")
                        !> allocate the tha1b_trac array
                        ! allocate(self%tha1b_trac(ifile%nrow - ifile%nheader), stat=err)
                        ! if (err /= 0) then
                        !     call logger%error('phase_centre_vad', "self%tha1b_trac: Allocation request denied")
                        !     call xml_o%xml2file(1, "self%tha1b_trac: Allocation request denied")
                        !     stop
                        ! end if
                        ! !> read in tha1b_trac header
                        ! read_tha_trac_header: do i = 1, ifile%nheader, 1
                        !     read(ifile%unit, *) temp
                        ! end do read_tha_trac_header
                        ! !> read in tha1b_trac data
                        ! read_tha_trac_data: do i = 1, ifile%nrow - ifile%nheader, 1
                        !     read(ifile%unit, *) self%tha1b_trac(i)%gpst_intg, self%tha1b_trac(i)%gpst_frac, temp, temp, temp, &
                        !                         self%tha1b_trac(i)%thruster_time, self%tha1b_trac(i)%accum_time
                        !     !> assign the gpst
                        !     self%tha1b_trac(i)%gpst = self%tha1b_trac(i)%gpst_intg + self%tha1b_trac(i)%gpst_frac * 1e-6_wp
                        !     
                        ! end do read_tha_trac_data
                        ! call logger%info('phase_centre_vad', 'read in THA1B_B data successfully')                   
                    case ('ResultPath')
                    case ("TempPath")
                    case ("LogPath")
                    
                    case default
                        call logger%error('phase_centre_vad', 'encounter unknown data product '//trim(key))
                        call xml_o%xml2file(1, 'encounter unknown data product '//trim(key))
                        stop
                end select read_data
                close(unit=ifile%unit, iostat=ios)
                if ( ios /= 0 ) stop "Error closing file unit ifile%unit"
            end do ifiles_loop
            if (allocated(value)) deallocate(value, stat=err)
            if (err /= 0) print *, "value: Deallocation request denied"
            if (allocated(ifiles)) deallocate(ifiles, stat=err)
            if (err /= 0) print *, "ifiles: Deallocation request denied"
            call iterator%next()
        end do
        
        !> using the arithmetic mean of phase centres for K/Ka band microwave as the equivalent phase centre
        assign_pcv_loop: do i = 1, 6, 1
            self%initial_vector(i) = 16.0_wp / 7.0_wp * self%initial_vector_ka(i) - 9.0_wp / 7.0_wp * self%initial_vector_k(i)
        end do assign_pcv_loop

    end subroutine initialise

end module phase_centre_vad