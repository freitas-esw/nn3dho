module ho3dnn

  use mod_cte
  use mod_ios
  use mod_rng
  use mod_network
  use mod_layer

  implicit none
   
  integer :: seed
  integer :: reports, cycles, batches

  integer :: design(10)

  real(kd) :: delta, rate, a

  type(neuralnet) :: ann

  contains
 
    subroutine read_set
      integer :: i
      namelist /honn/ delta, reports, cycles, design, seed, batches, rate, a
      open(unit=1,file="3dhonn.in")
      read(1, nml=honn )
      !open(unit=3,file='debug')
      i = design(1)
      design(1)=3
      ann = neuralnet( design(1:i), 'tanh', seed )
      call ann % layers(i) % set_activation ( 'sigmoid' )
      call ann%save("output")
    end subroutine

    function gs_actual( x ) result( wfn )
      real(kd) :: x(3), wfn
      wfn = exp( - half * norm2(x)**2 )
    end function 

    function gs_trial( x ) result( wfn )
      real(kd) :: x(3), wfn
      wfn = one / ( a**2 + norm2(x)**2 )**2
    end function 
    
    function gs_nn( x ) result( wfn )
      real(kd) :: x(3), wfn
      real(kd),allocatable::aux(:)
!      aux = [one]
      aux = ann % output( x )
      ! wfn = aux(1) * exp( -norm2(x)**2 )
      wfn = aux(1) / ( a**2 + norm2(x)**2 )**2
    end function 

    function ene_actual( x ) result( ene )
      real(kd) :: x(3), ene
      ene = - half * ( - three + norm2(x)**2 )  
      ene = ene + half * norm2(x)**2
    end function

    function ene_trial( x ) result( ene )
      real(kd) :: x(3), ene
      ene = - half * four * three * ( norm2(x)**2 - a**2 ) / ( a**2 + norm2(x)**2 )**2
! 2*Pi*int_0^infty r^2/(a^2+r^2)^4 dr = Pi^2/(16 a^5)
! Pi * int_0^infty r^4/(a^2+r^2)^4 dr = Pi^2/(32 a^3)
! -12 * Pi * int_0^infty r^2 *(r^2-a^2)/(a^2+r^2)^6 dr = 3Pi^2/(32a^7)
! ene : a^4+3 / (2 a^2)
      ene = ene + half * norm2(x)**2
    end function

    function ene_nn( x ) result( ene )
      real(kd) :: x(3), ene
      real(kd),allocatable::d(:),dd(:)
      call ann % output_ann( x )
      call ann % ddnn( d, dd )
      d  = d  /  ann % layers( size(ann%dims) ) % a(1)
      dd = dd / ann % layers( size(ann%dims) ) % a(1)
      ene = - half * four * three * ( norm2(x)**2 - a**2 ) / ( a**2 + norm2(x)**2 )**2
      ene = ene - sum( -four * d * x )/( a**2 + norm2(x)**2 )**2
      ! ene = - half * ( four * norm2(x)**2 - three*two )  ! dd e^(-r**2)
      ! ene = ene - half * sum( - four * d * x )           ! d e^(-r**2)*d nn
      ene = ene - half * ( dd(1)+dd(2)+dd(3) )
      ene = ene + half * norm2(x)**2 
      ene = ene 
    end function

    subroutine trial_move( x, f, acc, dopt )
      real(kd) :: x(3)
      real(kd), optional :: dopt
      integer  :: acc
      interface
        function f(x)
          import kd
          real(kd) :: x(3)
          real(kd) :: f
        end function f
      end interface
      real(kd) :: q, xt(3)
      if ( present(dopt) ) then
        xt = x + dopt*(half-randu(seed,3))
      else
        xt = x + delta*(half-randu(seed,3))
      endif
      q = f(xt)**2/f(x)**2
      if ( q > randu(seed) ) then
        x = xt
        acc=acc+1
      endif
    end subroutine

    function gradient_nn( x ) result( grad )
      real(kd) :: x(3)
      real(kd), allocatable :: grad(:)
      type(array1d), allocatable :: db(:)
      type(array2d), allocatable :: dw(:)
      integer                    :: i, j, k, n
         
      call ann % output_ann( x )
      n = 0
      do i = 2, size( ann % dims )
        n = n + ann % dims(i) * ( 1 + ann % dims(i-1) )
      enddo
      allocate( grad(n) )
      n = 1
      call ann % backprop( (/ zero /) , dw, db )
       
      do i = 2, size( ann % dims )
        do j = 1, ann % dims(i)
           grad(n) = db(i) % array(j)
           n = n + 1
        enddo   
      enddo
   
      do i = 1, size( ann % dims )-1
        do j = 1, ann % dims(i)
          do k = 1, ann % dims(i+1)
            grad(n) = dw(i) % array(j,k)
            n = n + 1
          enddo
        enddo
      enddo

      grad = grad / ann % layers( size(ann%dims) ) % a(1)**2

    end function

      
    function inverse( a, n ) result ( inv )
      integer, intent(in)     :: n
      real(kd), intent(inout) :: a(n,n)
      real(kd)                :: inv(n,n)

      real(kd) :: L(n,n), U(n,n), b(n), d(n), x(n)
      real(kd) :: coeff, copy(n,n)
      integer  :: i, j, k

      ! step 0: initialization for matrices L and U and b
      ! Fortran 90/95 aloows such operations on matrices
      L=0.0
      U=0.0
      b=0.0

copy = a
      ! step 1: forward elimination
      do k=1, n-1
        do i=k+1,n
          coeff=a(i,k)/a(k,k)
          L(i,k) = coeff
        do j=k+1,n
          a(i,j) = a(i,j)-coeff*a(k,j)
        end do
        end do
      end do

      ! Step 2: prepare L and U matrices 
      ! L matrix is a matrix of the elimination coefficient
      ! + the diagonal elements are 1.0
      do i=1,n
        L(i,i) = 1.0
      end do
      ! U matrix is the upper triangular part of A
      do j=1,n
        do i=1,j
          U(i,j) = a(i,j)
        end do
      end do

      ! Step 3: compute columns of the inverse matrix C
      do k=1,n
        b(k)=1.0
        d(1) = b(1)
      ! Step 3a: Solve Ld=b using the forward substitution
        do i=2,n
          d(i)=b(i)
          do j=1,i-1
            d(i) = d(i) - L(i,j)*d(j)
          end do
        end do
      ! Step 3b: Solve Ux=d using the back substitution
        x(n)=d(n)/U(n,n)
        do i = n-1,1,-1
          x(i) = d(i)
          do j=n,i+1,-1
            x(i)=x(i)-U(i,j)*x(j)
          end do
          x(i) = x(i)/u(i,i)
        end do
      ! Step 3c: fill the solutions x(n) into column k of C
        do i=1,n
          inv(i,k) = x(i)
        end do
        b(k)=0.0
      end do
    end function inverse
  
    function maprod ( mat, array ) result ( res )
      real(kd), intent(in)  :: mat(:,:), array(:)
      real(kd), allocatable :: res(:)
      integer               :: i, j
      allocate( res( size(mat,1) ) )
      do i = 1, size(mat,1)
        do j = 2, size(array)
          res(i) = res(i) + mat(i,j)*array(j)
        enddo
      enddo
    end function maprod
      
    subroutine update_parameter( dpar )
      real(kd)                    :: dpar(:)
      integer                     :: i, j, k, n
      type(array1d), allocatable  :: db(:)
      type(array2d), allocatable  :: dw(:)
      call db_init(db, ann % dims)
      call dw_init(dw, ann % dims)
      n = 1
call write_out(dpar,size(dpar),"teste") 
call write_out("","teste") 
      !if ( norm2(dpar) > one ) dpar = dpar / ( 0.8_kd * norm2(dpar) )
      do i = 2, size( ann % dims )
        do j = 1, ann % dims(i)
          ! dpar(n) = zero
          db(i) % array(j) = dpar(n) ! + 0.01_kd * ann % layers(i) % b(j)
          n = n + 1
        enddo
      enddo
      ! if ( norm2(dpar) > one ) dpar = dpar / norm2(dpar)
      do i = 1, size( ann % dims )-1
        do j = 1, ann % dims(i)
          do k = 1, ann % dims(i+1)
            dw(i) % array(j,k) = dpar(n) ! + 0.01_kd * ann % layers(i) % w(j,k)
            n = n + 1
          enddo
        enddo
      enddo
      call ann % update ( dw, db, one )  
    end subroutine update_parameter

end module 
