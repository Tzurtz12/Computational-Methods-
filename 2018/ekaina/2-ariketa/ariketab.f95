program diferentziak
      use mcf_tipos
      integer,parameter::nx=52,nt=21
      real(kind=dp),dimension(nx)::rho,rho1
      real(kind=dp)::h,tau,xa,xb,ta,tb
      real(kind=dp),dimension(nx)::x
      real(kind=dp),dimension(nt)::t
      integer::i,j,k

      h=10.0_dp/51.0_dp
      tau=0.004_dp
      xa=-5.0_dp
      xb=5.0_dp 
      ta=0.0_dp
      tb=0.08_dp
      do i=1,nx
      x(i)=xa+h*real(i-1,dp)
      end do
     ! print*,x
      do i=1,nt
      t(i)=tau*real(i-1,dp)
      end do
      !rho(t,x) erabiliko dugu
     ! rho(:,:)=0.0_dp

      !hasierako balioak finkatu
      do i=1,nx
      rho(i)=8.0_dp/cosh(x(i))**2
      end do
      rho(1)=0.0_dp
      rho(2)=0.0_dp
      rho(nx)=0.0_dp
      rho(nx-1)=0.0_dp
      open(unit=12,file="hasiera.dat",status="replace",action="write")
      do i=1,nx
      write(unit=12,fmt="(2f12.6)"),x(i),rho(i)
     ! write(unit=13,fmt="(2f12.6)"),x(i),rho0(i)
      end do
    ! do k=1,1000
      do j=1,nt-1
        do i=3,nx-2
        rho1(i)=rho(i)-6.0_dp*tau*rho(i)*(rho(i+1)-rho(i))/h-&
                tau*(rho(i+2)-3.0_dp*rho(i+1)+3.0_dp*rho(i)-&
                rho(i-1))/h**3
                 
        !tau*(rho(j,i+2)-3.0_dp*rho(j,i+1)+3.0_dp*rho(j,i)-&
         !       rho(j,i-1))/h**3
        end do

     
        rho=rho1
     end do 
      rho1(1)=0.0_dp
      rho1(2)=0.0_dp
      rho1(nx)=0.0_dp
      rho1(nx-1)=0.0_dp
!------------------C
      open(unit=11,file="emaitza.dat",status="replace",action="write")
     ! open(unit=12,file="hasiera.dat",status="replace",action="write")
      open(unit=13,file="amaier.dat",status="replace",action="write")
      do j=1,nt
        do i=1,nx
        write(unit=11,fmt="(3f12.6)"),t(j),x(i),rho(i)
        end do
      end do
      do i=1,nx
     ! write(unit=12,fmt="(2f12.6)"),x(i),rho1(i)
      write(unit=13,fmt="(2f12.6)"),x(i),rho1(i)
      end do
      close(unit=11)
      close(unit=12)
      close(unit=13)


end program diferentziak      
