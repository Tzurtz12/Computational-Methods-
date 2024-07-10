program hiperbol
      use mcf_tipos
      integer,parameter::n=1000
      integer::i
      real(kind=dp)::a,b,h
      real(kind=dp),dimension(n)::fddd,x

      open(unit=11,file="emaitza.dat",status="replace",action="write")
      a=-2.0_dp
      b=2.0_dp
      h=(b-a)/real(n-1,dp)
      do i=1,n
      x(i)=a+h*real(i-1,dp)
      end do

      !Deribatuaren definizioa bi eta hiru puntuko formulak aplikatuz

      do i=2,n-2
        fddd(i)=(f(x(i+2))-3.0_dp*f(x(i+1))+3.0_dp*f(x(i))-f(x(i-1)))/h**3
      end do

      fddd(1)=(f(x(3))-3.0_dp*f(x(2))+3.0_dp*f(x(1)))/h**3
      fddd(n-1)=(-3.0_dp*f(x(n))+3.0_dp*f(x(n-1))-f(x(n-2)))/h**3
      fddd(n)=(3.0_dp*f(x(n))-f(x(n-1)))/h**3

      do i=1,n
        write(unit=11,fmt="(2f20.12)"),x(i),fddd(i)
      end do
contains
      function f(x)
              real(kind=dp),intent(in)::x
              real(kind=dp)::f
              f=1.0_dp/(cosh(x/sqrt(2.0_dp))**2)
      end function f
end program hiperbol      
