module funtzioak
use mcf_tipos
public::F

contains
        function F(t,y) result(yprima)
          real(kind=dp),intent(in)::t
          real(kind=dp),dimension(:),intent(in)::y
          real(kind=dp),dimension(size(y))::yprima
          real(kind=dp)::k1,k2
          k1=5.0_dp
          k2=6.0_dp
          yprima(1)=y(2)
          yprima(2)=y(4)
          yprima(3)=-(k1+k2)*y(1)+k2*y(3)
          yprima(4)=k2*(y(1)-y(2))
        end function F         
end module funtzioak      
