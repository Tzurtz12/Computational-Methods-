program sinu
      use mcf_tipos
      use funtzioa
      use mcf_spline
      use mcf_interpoli
      use mcf_cuadratura
      use improper_cuadratura
      integer,parameter::n=1000
      real(kind=dp),dimension(n)::x,y,y2,f
      real(kind=dp)::h,xinterp,yinterp,a,b,integ,batu
      integer::i
      real(kind=dp),parameter::pi=acos(-1.0_dp),xa=0.0_dp,eps=1.0E-6
        
      open(unit=12,file="hasiera.dat",status="replace",action="write")  

      h=2.0_dp*pi/real(n-1,dp)
      do i=1,n
      x(i)=xa+h*real(i-1,dp)
      y(i)=sin(x(i))**2
      write(unit=12,fmt="(2f20.12)"),x(i),y(i)
      end do
      open(unit=11,file="spline.dat",status="replace",action="write")
      
      !Gure datuak interpolatu splin kubikoekin

      call spline(x,y,n,y2)

      do i=1,n
        xinterp=2.0_dp*pi*real(i-1,dp)/real(n-1,dp)
        call splint(x,y,y2,n,xinterp,yinterp)
        write(unit=11,fmt="(2f20.12)"),xinterp,yinterp
        f(i)=yinterp
      end do
     
     !integrala trapezioaren erregelarekin

      batu=0.0_dp
      do i=1,n-1
      batu=batu+(f(i+1)+f(i))*0.5*h
      end do
      
        
      a=0.0_dp
      b=2.0_dp*pi
      call romberg(g,a,b,integ,eps)

      open(unit=13,file="emaitzak.dat",status="replace",action="write")
      write(unit=13,fmt="(2f20.12,1e20.12)"),batu,integ,abs(batu-integ)
      close(unit=11)
      close(unit=12)
      close(unit=13)
!contains
!  function g(x)
!    real(kind=dp),intent(in)::x
!    real(kind=dp)::g
!    g=sin(x)**2
!  end function g    
end program sinu      
