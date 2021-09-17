module mod_network

  use mod_cte,      only: one
  use mod_ios,      only: write_out, write_stop, check_open
  use mod_kinds,    only: ik, rk
  use mod_rng,      only: randu
  use mod_layer,    only: array1d, array2d, db_init, dw_init,&
                          db_co_sum, dw_co_sum, layer
  use mod_parallel, only: tile_indices

  implicit none

  private
  public :: neuralnet

  type :: neuralnet

    type(layer), allocatable :: layers(:)
    integer, allocatable :: dims(:)

  contains

    procedure, pass(self)         :: copy
    procedure, public, pass(self) :: accuracy
    procedure, public, pass(self) :: backprop
    procedure, public, pass(self) :: gradient
    procedure, public, pass(self) :: pseudoforce
    procedure, public, pass(self) :: laplacian, ddnn 
    procedure, public, pass(self) :: fwdprop
    procedure, public, pass(self) :: init
    procedure, public, pass(self) :: load
    procedure, public, pass(self) :: loss
    procedure, public, pass(self) :: output_ann
    procedure, public, pass(self) :: output_batch
    procedure, public, pass(self) :: output_single
    procedure, public, pass(self) :: save
    procedure, public, pass(self) :: set_activation_equal
    procedure, public, pass(self) :: set_activation_layers
    procedure, public, pass(self) :: sync
    procedure, public, pass(self) :: train_batch
    procedure, public, pass(self) :: train_epochs
    procedure, public, pass(self) :: train_single
    procedure, public, pass(self) :: update

    generic, public :: assignment(=) => copy
    generic, public :: output => output_batch, output_single
    generic, public :: set_activation => set_activation_equal, set_activation_layers
    generic, public :: train => train_batch, train_epochs, train_single

  end type neuralnet

  interface neuralnet
    module procedure :: net_constructor
  end interface neuralnet

contains

  type(neuralnet) function net_constructor(dims, activation, stream_id) result(net)
    ! Network class constructor. Size of input array dims indicates the total
    ! number of layers (input + hidden + output), and the value of its elements
    ! corresponds the size of each layer.
    integer(ik), intent(in) :: dims(:), stream_id
    character(len=*), intent(in), optional :: activation
    call net % init(dims, stream_id)
    if (present(activation)) then
      call net % set_activation(activation)
    else
      call net % set_activation('sigmoid')
    end if
    call net % sync(1)
  end function net_constructor


  pure subroutine copy( self, from )
     class(neuralnet), intent(inout) :: self
     class(neuralnet), intent(in)    :: from
     self%dims   = from%dims
     self%layers = from%layers
  end subroutine copy


  pure real(rk) function accuracy(self, x, y)
    ! Given input x and output y, evaluates the position of the
    ! maximum value of the output and returns the number of matches
    ! relative to the size of the dataset.
    class(neuralnet), intent(in) :: self
    real(rk), intent(in) :: x(:,:), y(:,:)
    integer(ik) :: i, good
    good = 0
    do i = 1, size(x, dim=2)
      if (all(maxloc(self % output(x(:,i))) == maxloc(y(:,i)))) then
        good = good + 1
      end if
    end do
    accuracy = real(good) / size(x, dim=2)
  end function accuracy


  pure subroutine gradient(self, dw, db)
    ! Applies a gradient of the network output w.r.t. 
    ! the network parameters and returns the weight 
    ! and bias gradients.
    class(neuralnet), intent(in out) :: self
    type(array2d), allocatable, intent(out) :: dw(:)
    type(array1d), allocatable, intent(out) :: db(:)
    integer :: n, nm

    associate(dims => self % dims, layers => self % layers)

      call db_init(db, dims)
      call dw_init(dw, dims)

      n = size(dims)
      db(n) % array = self % layers(n) % activation_prime(layers(n) % z)
      dw(n-1) % array = matmul(reshape(layers(n-1) % a, [dims(n-1), 1]),&
                               reshape(db(n) % array, [1, dims(n)]))
      
      do n = size(dims) - 1, 2, -1
        db(n) % array = matmul(layers(n) % w, db(n+1) % array)&
                      * self % layers(n) % activation_prime(layers(n) % z)
        dw(n-1) % array = matmul(reshape(layers(n-1) % a, [dims(n-1), 1]),&
                                 reshape(db(n) % array, [1, dims(n)]))
      end do

    end associate

  end subroutine gradient


  subroutine backprop(self, y, dw, db)
  !pure subroutine backprop(self, y, dw, db)
    ! Applies a backward propagation through the network
    ! and returns the weight and bias gradients.
    class(neuralnet), intent(in out) :: self
    real(rk), intent(in) :: y(:)
    type(array2d), allocatable, intent(out) :: dw(:)
    type(array1d), allocatable, intent(out) :: db(:)
    integer :: n, nm

    associate(dims => self % dims, layers => self % layers)

      call db_init(db, dims)
      call dw_init(dw, dims)

      n = size(dims)
      db(n) % array = (layers(n) % a - y) * self % layers(n) % activation_prime(layers(n) % z)
      dw(n-1) % array = matmul(reshape(layers(n-1) % a, [dims(n-1), 1]),&
                               reshape(db(n) % array, [1, dims(n)]))

      do n = size(dims) - 1, 2, -1
        db(n) % array = matmul(layers(n) % w, db(n+1) % array)&
                      * self % layers(n) % activation_prime(layers(n) % z)
        dw(n-1) % array = matmul(reshape(layers(n-1) % a, [dims(n-1), 1]),&
                                 reshape(db(n) % array, [1, dims(n)]))
      end do

    end associate

  end subroutine backprop


  pure function pseudoforce( self ) result ( dx )
    ! Applies a gradient of the network output w.r.t. 
    ! the network inputs and returns the result
     class(neuralnet), intent(in) :: self
     real(rk), allocatable        :: dx(:), A(:)
     integer                      :: n

     associate( l => self%layers )

        n  = size(l)
        dx = self%layers(n)%activation_prime( l(n)%z )
        dx = matmul( l(n-1)%w, dx )
        do n = size(l)-1, 2, -1 
           dx = self%layers(n)%activation_prime( l(n)%z )*dx
           dx = matmul( l(n-1)%w, dx )
        enddo

     end associate

  end function pseudoforce


  subroutine ddnn( self, dx, ddx )
  !pure subroutine ddnn( self, dx, ddx )
    ! Applies a second derivative of the network output w.r.t. 
    ! the network inputs and returns the result
     class(neuralnet), intent(in)       :: self
     real(rk), allocatable, intent(out) :: dx(:), ddx(:)
     integer                            :: n, i
     real(rk), allocatable              :: da(:,:), dda(:,:)

     associate( l => self%layers )
        da  = transpose( l(1)%w )
        dda = transpose( l(1)%w )
        do i = 1, self%dims(1)
           da(:,i)  = self%layers(2)%activation_prime( l(2)%z )*da(:,i)
           dda(:,i) = self%layers(2)%activation_double( l(2)%z )*dda(:,i)**2
        enddo

        do n = 2, size(l)-1
           da  = matmul( transpose(l(n)%w), da )
           dda = matmul( transpose(l(n)%w), dda )
           do i = 1, self%dims(1)
              dda(:,i) = self%layers(n+1)%activation_prime( l(n+1)%z )*dda(:,i)
              dda(:,i) = dda(:,i) + self%layers(n+1)%activation_double( l(n+1)%z )*da(:,i)**2
              da(:,i)  = self%layers(n+1)%activation_prime( l(n+1)%z )*da(:,i)
           enddo
        enddo

        dx  = reshape( da, [ self%dims(1) ] )
        ddx = reshape( dda, [ self%dims(1) ] )

     end associate

  end subroutine ddnn


  pure function laplacian( self, x, dx, ddx ) result ( dnn )
     class(neuralnet), intent(in) :: self
     real(rk), intent(in)         :: x(:), dx(:), ddx(:)
     real(rk)                     :: dnn
     integer                      :: n
!     type(array1d), allocatable   :: ap(:), ad(:)
     real(rk), allocatable        :: z(:), a(:)
     real(rk), allocatable        :: wd(:), wdd(:)
     real(rk), allocatable        :: ap(:), ad(:)
     real(rk), allocatable        :: da(:), dda(:)

!     call db_init( ap, self%dims )
!     call db_init( ad, self%dims )

     associate( l => self%layers )

        da  = dx
        dda = ddx
        a   = x

        do n = 2, size(l)
           wd  = matmul(transpose( l(n-1)%w ), da )
           wdd = matmul(transpose( l(n-1)%w ), dda )
           z   = matmul(transpose( l(n-1)%w ), a ) + l(n)%b
           a   = self%layers(n)%activation( z )
           ap  = self%layers(n)%activation_prime( z )
           ad  = self%layers(n)%activation_double( z )
           da  = ap * wd
           dda = ad * wd**2 + ap * wdd
        enddo

        dnn = dda(1)

     end associate

!     a = self % layers(2) % activation(matmul(transpose(layers(1) % w), x) + layers(2) % b)
!     do n = 3, size(layers)
!        a = self % layers(n) % activation(matmul(transpose(layers(n-1) % w), a) + layers(n) % b)
!     end do

  end function laplacian


  pure subroutine fwdprop(self, x)
    ! Performs the forward propagation and stores arguments to activation
    ! functions and activations themselves for use in backprop.
    class(neuralnet), intent(in out) :: self
    real(rk), intent(in) :: x(:)
    integer(ik) :: n
    associate(layers => self % layers)
      layers(1) % a = x
      do n = 2, size(layers)
        layers(n) % z = matmul(transpose(layers(n-1) % w), layers(n-1) % a) + layers(n) % b
        layers(n) % a = self % layers(n) % activation(layers(n) % z)
      end do
    end associate
  end subroutine fwdprop


  subroutine init(self, dims, stream_id)
    ! Allocates and initializes the layers with given dimensions dims.
    class(neuralnet), intent(in out) :: self
    integer(ik), intent(in) :: dims(:), stream_id
    integer(ik) :: n
    self % dims = dims
    if (.not. allocated(self % layers)) allocate(self % layers(size(dims)))
    do n = 1, size(dims) - 1
      self % layers(n) = layer(dims(n), dims(n+1), stream_id)
    end do
    self % layers(n) = layer(dims(n), 1, stream_id)
    self % layers(1) % b = 0
    self % layers(size(dims)) % w = 0
  end subroutine init


  subroutine load(self, filename, stream_id)
    ! Loads the network from file.
    class(neuralnet), intent(in out) :: self
    character(len=*), intent(in) :: filename
    integer(ik) :: fileunit, n, num_layers, layer_idx
    integer(ik), allocatable :: dims(:), stream_id
    character(len=100) :: buffer ! activation string
    open(newunit=fileunit, file=filename, status='old', action='read')
    read(fileunit, *) num_layers
    allocate(dims(num_layers))
    read(fileunit, *) dims
    call self % init(dims, stream_id)
    do n = 1, num_layers
      read(fileunit, *) layer_idx, buffer
      call self % layers(layer_idx) % set_activation(trim(buffer))
    end do
    do n = 2, size(self % dims)
      read(fileunit, *) self % layers(n) % b
    end do
    do n = 1, size(self % dims) - 1
      read(fileunit, *) self % layers(n) % w
    end do
    close(fileunit)
  end subroutine load


  pure real(rk) function loss(self, x, y)
    ! Given input x and expected output y, returns the loss of the network.
    class(neuralnet), intent(in) :: self
    real(rk), intent(in) :: x(:), y(:)
    loss = 0.5 * sum((y - self % output(x))**2) / size(x)
  end function loss


  subroutine output_ann(self, x)
  !pure subroutine output_ann(self, x)
    ! Use forward propagation to compute the output of the network.
    ! This specific procedure is for a single sample of 1-d input data.
    class(neuralnet), intent(inout) :: self
    real(rk), intent(in) :: x(:)
    integer(ik) :: n

    associate(layers => self % layers)
      layers(1) % a = x
      do n = 2, size(layers)
        layers(n) % z = matmul(transpose(layers(n-1) % w), layers(n-1) % a) + layers(n) % b
        layers(n) % a = self % layers(n) % activation(layers(n) % z)
      end do
    end associate
    
  end subroutine output_ann


  pure function output_single(self, x) result(a)
    ! Use forward propagation to compute the output of the network.
    ! This specific procedure is for a single sample of 1-d input data.
    class(neuralnet), intent(in) :: self
    real(rk), intent(in) :: x(:)
    real(rk), allocatable :: a(:)
    integer(ik) :: n

    associate(layers => self % layers)
      a = self % layers(2) % activation(matmul(transpose(layers(1) % w), x) + layers(2) % b)
      do n = 3, size(layers)
        a = self % layers(n) % activation(matmul(transpose(layers(n-1) % w), a) + layers(n) % b)
      end do
    end associate

  end function output_single


  pure function output_batch(self, x) result(a)
    ! Use forward propagation to compute the output of the network.
    ! This specific procedure is for a batch of 1-d input data.
    class(neuralnet), intent(in) :: self
    real(rk), intent(in) :: x(:,:)
    real(rk), allocatable :: a(:,:)
    integer(ik) :: i
    allocate(a(self % dims(size(self % dims)), size(x, dim=2)))
    do i = 1, size(x, dim=2)
     a(:,i) = self % output_single(x(:,i))
    end do
  end function output_batch


  subroutine save(self, filename)
    ! Saves the network to a file.
    class(neuralnet), intent(in) :: self
    character(len=*), intent(in) :: filename
    character(30)                :: str_L
    integer(ik)                  :: fileunit, n, err, lin, col
!    open(newunit=fileunit, file=filename)
    call check_open( filename, fileunit, err )
    if ( err /= 0 ) call write_stop( 'save ann', 'open -- err /= 0' )
    write( unit=fileunit, fmt=*, iostat=err ) "Save ANN"
    if ( err /= 0 ) call write_stop( 'save ann', 'write -- err /= 0' )
    write(fileunit, fmt=*) size(self % dims)
    write(fileunit, fmt=*) self % dims
    do n = 1, size(self % dims)
      write(fileunit, fmt=*) n, self % layers(n) % activation_str
    end do
    do n = 2, size(self % dims)
      write( str_L, fmt=* ) "Layer ", n-1
      call write_out( str_L, filename )
      lin = size( self % layers(n) % b )
      write( fileunit, fmt=* ) "   b", lin
      call write_out( self % layers(n) % b, lin, filename )
      lin = size( self % layers(n-1) % w, 1 )
      col = size( self % layers(n-1) % w, 2 )
      write( fileunit, fmt=* ) "   W", lin, col
      call write_out( self % layers(n-1) % w, lin, col, filename )
    end do
    write( unit=fileunit, fmt=*, iostat=err ) " "
!    close(fileunit)
  end subroutine save


  pure subroutine set_activation_equal(self, activation)
    ! A thin wrapper around layer % set_activation().
    ! This method can be used to set an activation function
    ! for all layers at once. 
    class(neuralnet), intent(in out) :: self
    character(len=*), intent(in) :: activation
    call self % layers(:) % set_activation(activation)
  end subroutine set_activation_equal


  pure subroutine set_activation_layers(self, activation)
    ! A thin wrapper around layer % set_activation().
    ! This method can be used to set different activation functions
    ! for each layer separately. 
    class(neuralnet), intent(in out) :: self
    character(len=*), intent(in) :: activation(size(self % layers))
    call self % layers(:) % set_activation(activation)
  end subroutine set_activation_layers

  subroutine sync(self, image)
    ! Broadcasts network weights and biases from
    ! specified image to all others.
    class(neuralnet), intent(in out) :: self
    integer(ik), intent(in) :: image
    integer(ik) :: n
    if (num_images() == 1) return
    layers: do n = 1, size(self % dims)
#ifdef CAF
      call co_broadcast(self % layers(n) % b, image)
      call co_broadcast(self % layers(n) % w, image)
#endif
    end do layers
  end subroutine sync


  subroutine train_batch(self, x, y, eta)
    ! Trains a network using input data x and output data y,
    ! and learning rate eta. The learning rate is normalized
    ! with the size of the data batch.
    class(neuralnet), intent(in out) :: self
    real(rk), intent(in) :: x(:,:), y(:,:), eta
    type(array1d), allocatable :: db(:), db_batch(:)
    type(array2d), allocatable :: dw(:), dw_batch(:)
    integer(ik) :: i, im, n, nm
    integer(ik) :: is, ie, indices(2)

    im = size(x, dim=2) ! mini-batch size
    nm = size(self % dims) ! number of layers

    ! get start and end index for mini-batch
    indices = tile_indices(im)
    is = indices(1)
    ie = indices(2)

    call db_init(db_batch, self % dims)
    call dw_init(dw_batch, self % dims)

    do concurrent(i = is:ie)
      call self % fwdprop(x(:,i))
      !UNDO COMMENT call self % backprop(y(:,i), dw, db)
      do concurrent(n = 1:nm)
        dw_batch(n) % array =  dw_batch(n) % array + dw(n) % array
        db_batch(n) % array =  db_batch(n) % array + db(n) % array
      end do
    end do

    if (num_images() > 1) then
      call dw_co_sum(dw_batch)
      call db_co_sum(db_batch)
    end if

    call self % update(dw_batch, db_batch, eta / im)

  end subroutine train_batch


  subroutine train_epochs(self, x, y, eta, num_epochs, batch_size, stream_id )
    ! Trains for num_epochs epochs with mini-bachtes of size equal to batch_size.
    class(neuralnet), intent(in out) :: self
    integer(ik), intent(in) :: num_epochs, batch_size, stream_id
    real(rk), intent(in) :: x(:,:), y(:,:), eta

    integer(ik) :: i, n, nsamples, nbatch
    integer(ik) :: batch_start, batch_end

    real(rk) :: pos

    nsamples = size(y, dim=2)
    nbatch = nsamples / batch_size

    epochs: do n = 1, num_epochs
      batches: do i = 1, nbatch
      
        !pull a random mini-batch from the dataset  
        pos = randu( stream_id )
        batch_start = int(pos * (nsamples - batch_size + 1))
        if (batch_start == 0) batch_start = 1
        batch_end = batch_start + batch_size - 1
   
        call self % train(x(:,batch_start:batch_end), y(:,batch_start:batch_end), eta)
       
      end do batches
    end do epochs

  end subroutine train_epochs


  pure subroutine train_single(self, x, y, eta)
    ! Trains a network using a single set of input data x and output data y,
    ! and learning rate eta.
    class(neuralnet), intent(in out) :: self
    real(rk), intent(in) :: x(:), y(:), eta
    type(array2d), allocatable :: dw(:)
    type(array1d), allocatable :: db(:)
    call self % fwdprop(x)
    !UNDO COMMENT call self % backprop(y, dw, db)
    call self % update(dw, db, eta)
  end subroutine train_single


  pure subroutine update(self, dw, db, eta)
    ! Updates network weights and biases with gradients dw and db,
    ! scaled by learning rate eta.
    class(neuralnet), intent(in out) :: self
    class(array2d), intent(in) :: dw(:)
    class(array1d), intent(in) :: db(:)
    real(rk), intent(in) :: eta
    integer(ik) :: n

    associate(layers => self % layers, nm => size(self % dims))
      ! update biases
      do concurrent(n = 2:nm)
        layers(n) % b = layers(n) % b - eta * db(n) % array
      end do
      ! update weights
      do concurrent(n = 1:nm-1)
        layers(n) % w = layers(n) % w - eta * dw(n) % array
      end do
    end associate

  end subroutine update

end module mod_network
