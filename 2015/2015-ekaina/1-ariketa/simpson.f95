program simpson
      use mcf_tipos
      real(kind=dp)::a,b,h,x1,x2,x3,I
      !tartea definitu:
      print*,"Hasierako puntua:"
      read*,a
      print*,"Bukaerako puntua:"
      read*,b
      !pausua
      h=(b-a)/2.0_dp
      !Ebaluatutako puntuak:
      x1=a
      x2=a+h
      x3=b
      !Integralaren balioa
        
      I=h/3.0_dp*(f(x1)+4.0_dp*f(x2)+f(x3))

      write(unit=11,fmt="(1f20.12)"),I

      contains
              function f(x)
                      real(kind=dp),intent(in)::x
                      real(kind=dp)::f
                      !Adibide bezala g(x)=x^2-4 funtzioa
                      f=x**2.0_dp-4.0_dp
              end function f
end program simpson      
