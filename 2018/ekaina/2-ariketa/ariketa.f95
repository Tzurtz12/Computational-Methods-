program diferentziak
      use mcf_tipos
      integer,parameter::nx=52,nt=21
      real(kind=dp),dimension(nt,nx)::rho
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
      rho(1,i)=8.0_dp/cosh(x(i))**2
      end do
      rho(:,1)=0.0_dp
      rho(:,nx)=0.0_dp
      do j=2,nt
      rho(j,2)=rho(j-1,2)-6.0_dp*tau*rho(j-1,2)*(rho(j-1,3)-rho(j-1,2))/h-&
                tau*(rho(j-1,5)-3.0_dp*rho(j-1,4)+3.0_dp*rho(j-1,3)-rho(j-1,2))/h**3
      rho(j,nx-2)=rho(j-1,nx-2)-6.0_dp*tau*rho(j-1,nx-2)*(rho(j-1,nx-1)-rho(j-1,nx-2))/h-&
                tau*(-3.0_dp*rho(j-1,nx)+3.0_dp*rho(j-1,nx-1)-rho(j-1,nx-2))/h**3  
      end do
      rho(:,3)=rho(:,2)/4.0_dp
      rho(:,nx-1)=rho(:,nx-1)/4.0_dp
      

      
    ! do k=1,1000
      do j=2,nt
        do i=4,nx-3
        rho(j,i)=rho(j-1,i)-6.0_dp*tau*rho(j-1,i)*(rho(j-1,i+1)-rho(j-1,i))/h-&
                tau*(rho(j-1,i+3)-3.0_dp*rho(j-1,i+2)+3.0_dp*rho(j-1,i+1)-rho(j-1,i))/h**3

                 
        !tau*(rho(j,i+2)-3.0_dp*rho(j,i+1)+3.0_dp*rho(j,i)-&
         !       rho(j,i-1))/h**3
        end do

     ! end do
     ! rho=rho1
     end do 
    ! rho(:,3)=
     ! rho(:,1)=0.0_dp
     ! rho(:,2)=0.0_dp
     ! rho(:,nx)=0.0_dp
     ! rho(:,nx-1)=0.0_dp
     ! rho(:,nx-2)=0.0_dp
!------------------C
      open(unit=11,file="emaitza.dat",status="replace",action="write")
      open(unit=12,file="hasiera.dat",status="replace",action="write")
      open(unit=13,file="amaier.dat",status="replace",action="write")
      do j=1,nt
        do i=1,nx
        write(unit=11,fmt="(3f12.6)"),t(i),x(i),rho(j,i)
        end do
      end do
      do i=1,nx
      write(unit=12,fmt="(2f12.6)"),x(i),rho(1,i)
      write(unit=13,fmt="(2f12.6)"),x(i),rho(nt,i)
      end do
      close(unit=11)
      close(unit=12)
      close(unit=13)


end program diferentziak      
