program soka
      use mcf_tipos
       
      real(kind=dp)::h,tau,v,batu,Epot,r
      integer,parameter::nt=10000,nx=101
      integer::i,j,np
      real(kind=dp),parameter::pi=acos(-1.0_dp),L=1.0_dp,T=4.0_Dp,P0=0.1_dp,&
              w=1.0_dp
      real(kind=dp),dimension(nx,nt)::u,p
      real(kind=dp),dimension(nx)::x,f
      real(kind=dp),dimension(nt)::tt

      h=2.0_dp*L/real(nx-1,dp)
      do i=1,nx
      x(i)=-L+h*real(i-1,dp)
      end do

      tau=2.0_dp*pi/real(nt-1,dp)

      p(:,:)=0.0_dp
      np=ceiling(3.0_dp*real(nx)/4.0_dp)
      !npp=int(np)
      do i=1,nt
      tt(i)=tau*real(i-1,dp)
      p(np,i)=P0*sin(w*tt(i))**2
      end do

      !Diskretizazio parametroa
      do i=1,nx
       r=tau*sqrt(T/rho(x(i)))/h     
      if (r>0.5_dp) then
              print*,"Diskretizazio parametro ezegokia",i,r
              stop
      end if

      end do

      !hasierako baldintzak

      u(:,1)=0.0_dp
      u(:,2)=0.0_dp

      do j=2,nt-1
      u(1,j)=0.0_dp
      u(nx,j)=0.0_dp
      do i=2,nx-1
        r=tau*sqrt(T/rho(x(i)))/h
        u(i,j+1)=2.0_dp*u(i,j)-u(i,j-1)+r*(u(i+1,j)-2.0_dp*u(i,j)+u(i-1,j))
      end do
      end do

      open(unit=11,file="emaitzak.dat",status="replace",action="write")

      do i=1,nx
        write(unit=11,fmt="(2f20.12)"),x(i),u(i,nt)
      end do

      close(unit=11)

      !-----------C atala-----------------

      f(1)=(u(2,nt)-u(1,nt))/h

      do i=2,nx-1
      f(i)=(u(i,nt)-u(i-1,nt))/h
      end do

      batu=0.0_dp

      do i=1,nx
        batu=batu+f(i)**2*h*0.5_dp
      end do
      Epot=batu*T*0.5_dp

      write(unit=*,fmt="(a,1f20.12)"),"Epot=",Epot
contains
      function rho(x)
              real(kind=dp),intent(in)::x
              real(kind=dp)::rho
              real(kind=dp),parameter::rho0=1.0_dp,L=1.0_dp
              rho=rho0*exp(-x**2/(4.0_dp*L**2))
      end function rho





end program soka      
