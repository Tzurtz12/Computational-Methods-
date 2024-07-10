module funtzioa
use mcf_tipos
public::f
contains
      function f(x)
              real(kind=dp),intent(in)::x
              real(kind=dp)::f
              f=1.0_dp/sqrt(x)
      end function f
end module funtzioa      
