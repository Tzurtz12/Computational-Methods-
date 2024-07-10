program partziala
      use mcf_tipos
      integer,parameter::nx=100,nt=10001
      real(kind=dp),dimension(nt,nx)::phi
      real(kind=dp)::h,tau,alpha,ganma,beta,lambda,chi
      real(kind=dp),dimension(nx)::x
      real(kind=dp),dimension(nt)::t
      integer::i,j,k
      real(kind=dp),parameter::a=-4.0_dp,b=6.0_dp,ta=0.0_dp,tb=2.0_dp
      h=(b-a)/real(nx-1,dp)
      open(unit=12,file="emaitza.dat",status="replace",action="write")
      do i=1,nx
      x(i)=a+h*real(i-1,dp)
      end do

      tau=(tb-ta)/real(nt-1,dp)
      do j=1,nt
      t(j)=ta+tau*real(j-1,dp)
      end do
      alpha=tau**2/(1.0_dp-tau)
      ganma=(1.0_dp+h)/h**2
      beta=1.0_dp/h+2.0_dp/h**2+1.0_dp/tau-2.0_dp/tau**2
      lambda=1.0_dp/h**2
      chi=1.0_dp/tau**2

    !  phi(:,:)=0.0_dp
      do i=1,nx
      phi(1,i)=exp(-x(i)**2)
      phi(2,i)=2.0_dp*tau*x(i)*exp(-x(i)**2)+phi(1,i)
      end do
      
     ! phi(:,1)=
    ! 1 phi(:,nx)=
      
     ! do k=1,1000
      do j=3,nt
      phi(j,1)=0.0_dp
      phi(j,nx)=0.0_dp
        do i=2,nx-1
                phi(j,i)=alpha*(phi(j-1,i+1)*ganma-phi(j-1,i)*beta+phi(j-1,i-1)*lambda-&
                        chi*phi(j-2,i))
           !     phi(1,i)=exp(-x(i)**2)
            !    phi(2,i)=2.0_dp*tau*x(i)*exp(-x(i)**2)+phi(1,i)
        end do
     !   phi(j,1)=alpha*(phi(j-1,2)*ganma-phi(j-1,1)*beta-&
       !                 chi*phi(j-2,i))
      !  phi(j,nx)=alpha*(-phi(j-1,i)*beta+phi(j-1,i-1)*lambda-&
        !                chi*phi(j-2,i))
      end do
      phi(1,nx)=0.0_dp
      phi(2,nx)=0.0_dp
     ! end do
      do j=1,nt
        do i=1,nx
        write(unit=12,fmt="(3f20.12)"),x(i),t(j),phi(j,i)
        end do
      end do 
     open(unit=11,file="funtzioak.dat",status="replace",action="write")
     do i=1,nx
        write(unit=11,fmt="(3f20.12)"),x(i),phi(j/2,i),phi(nt,i)
     end do
    close(unit=12)
    close(unit=11) 
end program partziala      
