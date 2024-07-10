module funtzioak
use mcf_tipos
public::F
contains
      function F(t,x) result(xprima)
              real(kind=dp),intent(in)::t
              real(kind=dp),dimension(:),intent(in)::x
              real(kind=dp),dimension(size(x))::xprima
              real(kind=dp),parameter::ganma=0.07_dp,b=0.5_dp,a=0.02_dp  
              xprima(1)=x(2)
              xprima(2)=-ganma*x(2)+2.0_dp*b*x(1)-4.0_dp*a*x(1)**3
      end function F


end module funtzioak      
