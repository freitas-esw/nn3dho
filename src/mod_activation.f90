module mod_activation

  ! A collection of activation functions and their derivatives.

  use mod_kinds, only: ik, rk

  implicit none

  private

  public :: activation_function
  public :: gaussian, gaussian_prime, gaussian_double
  public :: relu, relu_prime, relu_double
  public :: sigmoid, sigmoid_prime, sigmoid_double
  public :: step, step_prime, step_double
  public :: tanhf, tanh_prime, tanh_double
  public :: expf, exp_prime, exp_double

  interface
    pure function activation_function(x)
      import :: rk
      real(rk), intent(in) :: x(:)
      real(rk) :: activation_function(size(x))
    end function activation_function
  end interface

contains

  pure function gaussian(x) result(res)
    ! Gaussian activation function.
    real(rk), intent(in) :: x(:)
    real(rk) :: res(size(x))
    res = exp(-x**2)
  end function gaussian

  pure function gaussian_prime(x) result(res)
    ! First derivative of the Gaussian activation function.
    real(rk), intent(in) :: x(:)
    real(rk) :: res(size(x))
    res = -2 * x * gaussian(x)
  end function gaussian_prime

  pure function gaussian_double(x) result(res)
    ! Second derivative of the Gaussian activation function.
    real(rk), intent(in) :: x(:)
    real(rk) :: res(size(x))
    res = -2 * gaussian(x) -2 * x * gaussian_prime(x)
  end function gaussian_double

  pure function relu(x) result(res)
    !! REctified Linear Unit (RELU) activation function.
    real(rk), intent(in) :: x(:)
    real(rk) :: res(size(x))
    res = max(0., x)
  end function relu

  pure function relu_prime(x) result(res)
    ! First derivative of the REctified Linear Unit (RELU) activation function.
    real(rk), intent(in) :: x(:)
    real(rk) :: res(size(x))
    where (x > 0)
      res = 1
    elsewhere
      res = 0
    end where
  end function relu_prime

  pure function relu_double(x) result(res)
    ! Second derivative of the REctified Linear Unit (RELU) activation function.
    real(rk), intent(in) :: x(:)
    real(rk) :: res(size(x))
    res = 0
  end function relu_double

  pure function sigmoid(x) result(res)
    ! Sigmoid activation function.
    real(rk), intent(in) :: x(:)
    real(rk) :: res(size(x))
    res = 1 / (1 + exp(-x))
  end function sigmoid

  pure function sigmoid_prime(x) result(res)
    ! First derivative of the sigmoid activation function.
    real(rk), intent(in) :: x(:)
    real(rk) :: res(size(x))
    res = sigmoid(x) * (1 - sigmoid(x))
  end function sigmoid_prime

  pure function sigmoid_double(x) result(res)
    ! Second derivative of the sigmoid activation function.
    real(rk), intent(in) :: x(:)
    real(rk) :: res(size(x))
    res = sigmoid(x) * (1 - sigmoid(x))**2 - sigmoid(x)**2 * (1 - sigmoid(x))
  end function sigmoid_double

  pure function step(x) result(res)
    ! Step activation function.
    real(rk), intent(in) :: x(:)
    real(rk) :: res(size(x))
    where (x > 0)
      res = 1
    elsewhere
      res = 0
    end where
  end function step

  pure function step_prime(x) result(res)
    ! First derivative of the step activation function.
    real(rk), intent(in) :: x(:)
    real(rk) :: res(size(x))
    res = 0
  end function step_prime

  pure function step_double(x) result(res)
    ! Second derivative of the step activation function.
    real(rk), intent(in) :: x(:)
    real(rk) :: res(size(x))
    res = 0
  end function step_double

  pure function tanhf(x) result(res)
    ! Tangent hyperbolic activation function. 
    ! Same as the intrinsic tanh, but must be 
    ! defined here so that we can use procedure
    ! pointer with it.
    real(rk), intent(in) :: x(:)
    real(rk) :: res(size(x))
    res = tanh(x)
  end function tanhf

  pure function tanh_prime(x) result(res)
    ! First derivative of the tanh activation function.
    real(rk), intent(in) :: x(:)
    real(rk) :: res(size(x))
    res = 1 - tanh(x)**2
  end function tanh_prime

  pure function tanh_double(x) result(res)
    ! Second derivative of the tanh activation function.
    real(rk), intent(in) :: x(:)
    real(rk) :: res(size(x))
    res = -2 * tanh(x) * ( 1 - tanh(x)**2 ) 
  end function tanh_double

  pure function expf(x) result(res)
    ! Exponential activation function. 
    ! Same as the intrinsic exp, but must be 
    ! defined here so that we can use procedure
    ! pointer with it.
    real(rk), intent(in) :: x(:)
    real(rk) :: res(size(x))
    res = exp(x)
  end function expf

  pure function exp_prime(x) result(res)
    ! First derivative of the exp activation function.
    real(rk), intent(in) :: x(:)
    real(rk) :: res(size(x))
    res = exp(x)
  end function exp_prime

  pure function exp_double(x) result(res)
    ! Second derivative of the exp activation function.
    real(rk), intent(in) :: x(:)
    real(rk) :: res(size(x))
    res = exp(x)
  end function exp_double

end module mod_activation
