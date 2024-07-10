module funtzioa
use mcf_tipos
public::g
contains
      function g(x)
              real(kind=dp),intent(in)::x
              real(kind=dp)::g
              g=sin(x)**2
      end function g


end module funtzioa      
