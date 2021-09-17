module mod_layer

  ! Defines the layer type and its methods.

  use mod_activation
  use mod_kinds, only: ik, rk
  use mod_rng,   only: randg

  implicit none

  private
  public :: array1d, array2d, db_init, db_co_sum, dw_init, dw_co_sum, layer
  public :: operator(+)

  type :: layer
    real(rk), allocatable :: a(:) ! activations
    real(rk), allocatable :: b(:) ! biases
    real(rk), allocatable :: w(:,:) ! weights
    real(rk), allocatable :: z(:) ! arg. to activation function
    procedure(activation_function), pointer, nopass :: activation => null()
    procedure(activation_function), pointer, nopass :: activation_prime => null()
    procedure(activation_function), pointer, nopass :: activation_double => null()
    character(len=:), allocatable :: activation_str ! activation character string
  contains
    procedure, pass(self)         :: copy_layer
    procedure, public, pass(self) :: set_activation

    generic, public :: assignment(=) => copy_layer
  end type layer

  type :: array1d
    real(rk), allocatable :: array(:)
!  contains
!    procedure, pass(self) :: copy_array1d
!    generic, public :: assignment(=) => copy_array1d
  end type array1d

  type :: array2d
    real(rk), allocatable :: array(:,:)
!  contains
!    procedure, pass(self) :: copy_array2d
!    generic, public :: assignment(=) => copy_array2d
  end type array2d

  interface layer
    module procedure :: constructor
  end interface layer

  interface array1d
    module procedure :: array1d_constructor
  end interface array1d

  interface array2d
    module procedure :: array2d_constructor
  end interface array2d

  interface operator (+)
     module procedure array1d_add
!    pure function array1d_add( v1, v2 ) result ( v )
!      import array1d      
!      class (array1d), intent(in) :: v1, v2
!      class (array1d)             :: v
!    end function array1d_add
  end interface 

 contains

  type(layer) function constructor(this_size, next_size, stream_id) result( l )
    ! Layer class constructor. this_size is the number of neurons in the layer.
    ! next_size is the number of neurons in the next layer, used to allocate
    ! the weights.
    integer(ik), intent(in) :: this_size, next_size, stream_id
    allocate(l % a(this_size))
    allocate(l % z(this_size))
    l % a = 0
    l % z = 0
    l % w = randg(stream_id, this_size, next_size) / this_size
    allocate(l % b(this_size))
    l % b = 0.0_rk ! randg(stream_id, this_size)
    call l%set_activation( 'standard' )
  end function constructor


  pure type(array1d) function array1d_constructor(length) result(a)
    ! Overloads the default type constructor.
    integer, intent(in) :: length
    allocate(a % array(length))
    a % array = 0
  end function array1d_constructor


  pure type(array2d) function array2d_constructor(dims) result(a)
    ! Overloads the default type constructor.
    integer, intent(in) :: dims(2)
    allocate(a % array(dims(1), dims(2)))
    a % array = 0
  end function array2d_constructor


  subroutine copy_layer( self, from )
  !pure subroutine copy( self, from )
     class(layer), intent(inout) :: self
     class(layer), intent(in)    :: from
     self%a = from%a
     self%b = from%b
     self%w = from%w
     self%z = from%z
     call self%set_activation( from%activation_str )
  end subroutine copy_layer


  pure function array1d_add( v1, v2 ) result ( v )
    type (array1d), intent(in) :: v1, v2
    type (array1d)             :: v
    v % array = v1 % array + v2 % array
  end function array1d_add 


  pure subroutine db_init(db, dims)
    ! Initialises biases structure.
    type(array1d), allocatable, intent(in out) :: db(:)
    integer(ik), intent(in) :: dims(:)
    integer :: n, nm
    nm = size(dims)
    allocate(db(nm))
    do n = 1, nm - 1
      db(n) = array1d(dims(n))
    end do
    db(n) = array1d(dims(n))
  end subroutine db_init

  pure subroutine dw_init(dw, dims)
    ! Initialises weights structure.
    type(array2d), allocatable, intent(in out) :: dw(:)
    integer(ik), intent(in) :: dims(:)
    integer :: n, nm
    nm = size(dims)
    allocate(dw(nm))
    do n = 1, nm - 1
      dw(n) = array2d(dims(n:n+1))
    end do
    dw(n) = array2d([dims(n), 1])
  end subroutine dw_init

  subroutine db_co_sum(db)
    ! Performs a collective sum of bias tendencies.
    type(array1d), allocatable, intent(in out) :: db(:)
    integer(ik) :: n
    do n = 2, size(db)
#ifdef CAF
      call co_sum(db(n) % array)
#endif
    end do
  end subroutine db_co_sum

  subroutine dw_co_sum(dw)
    ! Performs a collective sum of weights tendencies.
    type(array2d), allocatable, intent(in out) :: dw(:)
    integer(ik) :: n
    do n = 1, size(dw) - 1
#ifdef CAF
      call co_sum(dw(n) % array)
#endif
    end do
  end subroutine dw_co_sum

  pure elemental subroutine set_activation(self, activation)
    ! Sets the activation function. Input string must match one of
    ! provided activation functions, otherwise it defaults to sigmoid.
    ! If activation not present, defaults to sigmoid.
    class(layer), intent(in out) :: self
    character(len=*), intent(in) :: activation
    select case(trim(activation))
      case('gaussian')
        self % activation => gaussian
        self % activation_prime => gaussian_prime
        self % activation_double => gaussian_double
        self % activation_str = 'gaussian'
      case('relu')
        self % activation => relu
        self % activation_prime => relu_prime
        self % activation_double => relu_double
        self % activation_str = 'relu'
      case('sigmoid')
        self % activation => sigmoid
        self % activation_prime => sigmoid_prime
        self % activation_double => sigmoid_double
        self % activation_str = 'sigmoid'
      case('step')
        self % activation => step
        self % activation_prime => step_prime
        self % activation_double => step_double
        self % activation_str = 'step'
      case('tanh')
        self % activation => tanhf
        self % activation_prime => tanh_prime
        self % activation_double => tanh_double
        self % activation_str = 'tanh'
      case('exp')
        self % activation => expf
        self % activation_prime => exp_prime
        self % activation_double => exp_double
        self % activation_str = 'exp'
      case default
        self % activation => sigmoid
        self % activation_prime => sigmoid_prime
        self % activation_double => sigmoid_double
        self % activation_str = 'sigmoid'
    end select
  end subroutine set_activation

end module mod_layer
