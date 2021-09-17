module mod_cte

   use iso_fortran_env, only: INT32, INT64, REAL32, REAL64

   implicit none

real :: START, FINISH, ACC_TIME=0.e0

   !---------------------!
   ! Constant parameters !
   !---------------------!

   integer, parameter     :: si = INT32  ! simple integer
   integer, parameter     :: di = INT64  ! double integer
   integer, parameter     :: sp = REAL32 ! simple precision
   integer, parameter     :: dp = REAL64 ! double precison
   integer, parameter     :: sc = 100
   integer, parameter     :: dc = 200
   integer, parameter     :: qc = 255

   integer, parameter     :: kd = dp

   real(kd), parameter    :: zero  = 0.0_kd
   real(kd), parameter    :: one   = 1.0_kd
   real(kd), parameter    :: two   = 2.0_kd
   real(kd), parameter    :: three = 3.0_kd
   real(kd), parameter    :: four  = 4.0_kd
   real(kd), parameter    :: five  = 5.0_kd
   real(kd), parameter    :: half  = one/two
   real(kd), parameter    :: pi    = four*atan( one ) 
   real(kd), parameter    :: euler = exp( one )

   complex(kd), parameter :: ci    = complex(zero,one)
   complex(kd), parameter :: cone  = complex(one,zero)
   complex(kd), parameter :: czero = complex(zero,zero)

   character*9, parameter  :: fmt_sp  = '(f16.8)'
   character*9, parameter  :: fmt_dp  = '(f24.16)'
   character*9, parameter  :: fmt_qp  = '(f40.32)'
   character*9, parameter  :: fmt_si  = '(I12)'
   character*9, parameter  :: fmt_exp = '(e24.16)'
   character*9, parameter  :: fmt_sci = '(es24.16)'
   character*30, parameter :: fmt_zp  = '(f23.16,SP,f23.16,"I")'
   character*30, parameter :: fmt_zp2 = '("(",f23.16,",",f23.16,") ")' 
   character*30, parameter :: fmt_cp  = '(f15.8,SP,f15.8,"I")'
   character*30, parameter :: fmt_cp2 = '("(",f15.8,",",f15.8,") ")' 

   logical, parameter     :: true = .true., false = .false.

   !--------------------!
   ! Phisical constants !
   !--------------------!
   real(kd), parameter    :: c_si = 299792458._kd*1d+0               ! speed of light
   real(kd), parameter    :: q_si = 1.602176634_kd*1d-19             ! electron charge
   real(kd), parameter    :: mp_si = 1.67262192369_kd*1d-27          ! proton mass
   real(kd), parameter    :: mn_si = 1.67492749804_kd*1d-27          ! neutron mass
   real(kd), parameter    :: me_si = 9.10938188_kd*1d-31             ! electron mass
   real(kd), parameter    :: avogadro_si = 6.02214076_kd*1d+23       ! Avogadro constant
   real(kd), parameter    :: planck_si = 6.62607015_kd*1d-34         ! Planck's constant
   real(kd), parameter    :: hbar_si = planck_si*half/pi             ! hbar
   real(kd), parameter    :: mu_0_si = 1.25663706212_kd*1d-6         ! mu_0
   real(kd), parameter    :: molar_gas_si = 8.314462618_kd*1d+0      ! molar gas constant
   real(kd), parameter    :: kB_si = molar_gas_si/avogadro_si        ! Boltzmann's const
   real(kd), parameter    :: epsilon_0_si = one/(mu_0_si*c_si**2)    ! epsilon_0
   real(kd), parameter    :: coulomb_si = one/(four*pi*epsilon_0_si) ! Coulomb's constant
   real(kd), parameter    :: alpha = 7.2973525693_kd*1d-3            ! Fine-structure constant
   real(kd), parameter    :: hartree_si = me_si*(alpha*c_si)**2      ! Hartree energy
   real(kd), parameter    :: a0_si = hbar_si/(c_si*me_si*alpha)      ! Bohr radius
   real(kd), parameter    :: u_si = 1d-3/avogadro_si                 ! atomic mass unit
   ! Some convertions of Joule to eV
   real(kd), parameter    :: hartree_ev = hartree_si/q_si   
   real(kd), parameter    :: hbar_ev = hbar_si/q_si   
   real(kd), parameter    :: kB_ev = kB_si/q_si   
!  real(kd), parameter    :: J_to_ev = one/q_si

   !--------------------------!
   ! Computational parameters !
   !--------------------------!

   real(kd), parameter    :: min_exp = one*1d-100,     max_exp = one*1d+100
   real(kd), parameter    :: ethrs15 = 0.111_kd*1d-15, ethrs31 = 0.123_kd*1d-31
   real(kd), parameter    :: eps_kd  = epsilon(one)

end module mod_cte
