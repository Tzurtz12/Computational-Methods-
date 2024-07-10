program ariketa
      use mcf_tipos
      use rk4
      use funtzioak
      real(kind=dp),dimension(2)::x
      real(kind=dp)::x0,h,t
      real(kind=dp),parameter::ta=0.0_dp,tb=100.0_dp
      integer::i
      integer,parameter::n=10000
      open(unit=11,file="rk4.dat",status="replace",action="write")
      !RK4 algoritmoarekin
      x(1)=5.0_dp
      x(2)=0.0_dp

      h=(tb-ta)/real(n,dp)
      t=ta
      do i=1,n
        write(unit=11,fmt="(3f20.12)"),t,x
        call rk4_paso_dp(t,x,f,h)
      end do
      close(unit=11)

        !TAYLORREN BIGARREN ORDENA









end program ariketa      
