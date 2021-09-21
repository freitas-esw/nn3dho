program freennp

  use mod_cte
  use mod_rng
  use mod_ios
  use mod_global
  use ho3dnn

  implicit none

  real(kd) :: cpu
  integer  :: wall

  real(kd), allocatable :: g(:), h(:), ene_g(:), acc_g(:)
  real(kd), allocatable :: vec_v(:), hat_v(:), vec_m(:), hat_m(:)
  real(kd), allocatable :: fim(:,:), inv(:,:)
  real(kd) :: xa(3), xt(3), xn(3)
  real(kd) :: ela, elt, eln, eat, ean
  integer  :: acc_a, acc_t, acc_n
  integer  :: i, j, k, m

  real(kd) :: beta1 = 0.9_kd, beta2 = 0.999_kd
  real(kd) :: eta = 0.001_kd, eps = one/10**8

  call start_tracking_time( cpu, wall )
  call initialize_rng(10)
  call read_set

  xa = randu( 1, 3 )
  xt = randu( 1, 3 )
  xn = randu( 1, 3 )

  i = size( gradient_nn(xa) )
  allocate( g(i), h(i), ene_g(i), acc_g(i) )
  allocate( vec_v(i), vec_m(i) )
  allocate( hat_v(i), hat_m(i) )
  allocate( fim(i,i), inv(i,i) )

  eta = rate

  vec_m = zero
  vec_v = zero

  do m = 1, batches

    if ( mod(m,int(0.2*batches))==0 ) write(*,'(1f6.2,a)') 1e2*m/batches,'%'

    fim   = zero
    acc_g = zero
    ene_g = zero

    do 
      acc_n = 0
      do i = 1, 1000
        call trial_move( xn, gs_nn, acc_n )  
      enddo
      if (acc_n==0) exit
      if ( acc_n/1000.0 > 0.43 .and. acc_n/1000.0 < 0.48 ) exit
      delta = delta - log(0.46*1000/acc_n)
    enddo

    do i = 1, reports/2
      do j = 1, cycles
        call trial_move( xa, gs_actual, acc_a )
        call trial_move( xt, gs_trial,  acc_t )
        call trial_move( xn, gs_nn,     acc_n )
      enddo
    enddo

    acc_a = 0
    acc_t = 0
    acc_n = 0

    eat = zero
    ean = zero

    do i = 1, reports
      do j = 1, cycles
        call trial_move( xa, gs_actual, acc_a )
        call trial_move( xt, gs_trial,  acc_t )
        call trial_move( xn, gs_nn,     acc_n )
      enddo
      ela = ene_actual(xa)
      elt = ene_trial(xt)
      eat = eat + elt 
      eln = ene_nn(xn)
      ean = ean + eln
      if ( i == reports ) then 
        write(2,fmt='(1I8)',advance='no') m ! i*cycles
        write(2,fmt='(2f10.4)',advance='no') ela, real(acc_a,kd)/(i*cycles)
        write(2,fmt='(3f10.4)',advance='no') elt, eat/i, real(acc_t,kd)/(i*cycles)
        write(2,fmt='(3f11.4)') eln, ean/i, real(acc_n,kd)/(i*cycles)
      endif      

      g = gradient_nn ( xn )

      acc_g = acc_g + g / reports
      ene_g = ene_g + eln * g / reports

      do j = 1, size(fim,1)  
        do k = 1, size(fim,2)      
          fim(j,k) = fim(j,k) + g(j)*g(k) / reports
        enddo           
      enddo

    enddo

    eat = eat / reports
    ean = ean / reports
  
! ----------------- KFAC Optimizer ----------------- !
    do j = 1, size(fim,1)  
      do k = 1, size(fim,2)      
        fim(j,k) = fim(j,k) - acc_g(j)*acc_g(k) 
      enddo           
      fim(j,j) = fim(j,j) + lambda
    enddo

    inv = inverse( fim, size(fim,1) )
    h = lambda * get_params()
    h = h + ene_g - ean*acc_g
    g = maprod( inv, h )

    call write_out( g, size(g), "teste" )

    g = eta * g   ! / norm2(g)
! -------------------------------------------------- !


! ----------------- ADAM Optimizer ----------------- !
!    g = ene_g - ean * acc_g
!
!    vec_m = beta1 * vec_m + ( one - beta1 )*g
!    vec_v = beta2 * vec_v + ( one - beta2 )*( g*g )
!
!    hat_m = vec_m / ( one - beta1**m )
!    hat_v = vec_v / ( one - beta2**m )
!
!    g = eta * hat_m / ( sqrt( hat_v ) + eps )
! -------------------------------------------------- !

    call update_parameter( g )

  enddo

  call ann % save( 'output' )
  call finalize_rng()
  call end_tracking_time( cpu, wall, 'output' )

end program
