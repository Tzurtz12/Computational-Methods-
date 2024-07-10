program ariketa
      use mcf_tipos
      integer,parameter::nt=10000,nx=100
      real(kind=dp),dimension(nx)::x,phi0,phi1
      real(kind=dp),dimension(nt)::t
      real(kind=dp)::h,tau,r
      integer::i,j,np
      real(kind=dp),parameter::pi=acos(-1.0_dp),k=0.05_dp,L=1.0_dp,sigma=0.1_dp,ta=0.0_dp,&
              tb=L**2/k
      !espazio-denbora diskretizatu
      h=2.0_dp*L/real(nx-1,dp)
      tau=(L**2/k)/real(nt-1,dp)
      open(unit=11,file="emaitza.dat",status="replace",action="write")
      do i=1,nx
      x(i)=-L+h*real(i-1,dp)
      phi0(i)=exp(-(x(i)-L/2.0_dp)**2/sigma)/(sqrt(pi)*sigma)
      end do
      
      r=tau*k/h**2
      if (r>0.5_dp) then
              print*,"Parametrizazio ezegokia: ",r
              stop
      end if

        
      do i=1,nt
        t(i)=ta+tau*real(i-1,dp)
      end do
      np=ceiling(3.0_dp*tb/50.0_dp)
      do j=1,nt
        do i=2,nx-1
                phi1(i)=r*(phi0(i+1)-2.0_dp*phi0(i)+phi0(i-1))+phi0(i)
        end do
        phi1(1)=phi1(2)
        phi1(nx)=phi1(nx-1)
        phi0=phi1
        if (j==np) then
                print*,"a"
                do i=1,nx
                write(unit=11,fmt="(2f20.12)"),x(i),phi1
                end do
        end if
      end do
      close(unit=11)
end program ariketa      
