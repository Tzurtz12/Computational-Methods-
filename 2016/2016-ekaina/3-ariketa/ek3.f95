program arik3
      use mcf_tipos
      integer,parameter::nt=5001,nx=100
      integer::i,j,np
      real(kind=dp),dimension(nt,nx)::u
      real(kind=dp)::k,L,h,r,sigma,tau,batu
      real(kind=dp),dimension(nx)::x
      real(kind=dp),dimension(nt)::t,f
      real(kind=dp),parameter::pi=acos(-1.0_dp)

      k=0.05_dp
      L=1.0_dp
      sigma=0.1_dp
      h=2.0_dp*L/real(nx-1,dp)
      np=ceiling(0.06_dp*L**2*nt/k)
      do i=1,nx
      x(i)=-L+h*real(i-1,dp)
      end do

      tau=L**2/(k*real(nt-1,dp))
      do i=1,nt
      t(i)=tau*real(i-1,dp)
      end do

      r=tau*k/h**2
      if(r>0.5_dp) then
              print*,"Diskretizazio ezegokia,", r
              stop
      end if

      !hasierako balioak
      do i=1,nx
      u(1,i)=exp(-((x(i)-L/2.0_dp)/sigma)**2)/(sqrt(pi)*sigma)
      end do

      do j=1,nt-1
        do i=2,nx-1
        u(j+1,i)=r*(u(j,i+1)-2.0_dp*u(j,i)+u(j,i-1))+u(j,i)
        end do
        u(j+1,1)=u(j+1,2)
        u(j+1,nx)=u(j+1,nx-1)
      end do

      open(unit=12,file="emaitzakb.dat",status="replace",action="write")

      do i=1,nx
      write(unit=12,fmt="(2f20.12)"),x(i),u(301,i)
      end do

      close(unit=12)
!----------------------C atala----------------
      open(unit=13,file="emaitzac.dat",status="replace",action="write")

      !Integrala trapezio erregelarekin kalkulatuz

      do j=1,nt
      batu=0.0_dp
        do i=1,nx-1
        batu=batu+(u(j,i+1)+u(j,i))*0.5_dp*h
        end do
        f(j)=batu
        write(unit=13,fmt="(4f20.12)"),t(j),u(j,1),u(j,nx),f(j)
      end do
      close(unit=13)


end program arik3      
