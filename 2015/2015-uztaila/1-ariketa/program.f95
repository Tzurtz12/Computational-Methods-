program ariketa
      use mcf_tipos
      use rk4
      use funtzioak
      real(kind=dp),dimension(4)::y
      real(kind=dp)::h,t
      real(kind=dp),parameter::ta=0.0_dp,tb=10.0_dp
      integer,parameter::n=1000
      integer::i
      y(1)=0.0_dp
      y(2)=1.0_dp
      y(3)=0.0_dp
      y(4)=0.0_dp
      h=(tb-ta)/real(n,dp)
      t=ta
      open(unit=11,file="emaitzak2.dat",status="replace",action="write")

      do i=1,n
        write(unit=11,fmt="(5f20.12)"),t,y
        call rk4_paso_dp(t,y,f,h)
      end do
      close(unit=11)
end program ariketa      
