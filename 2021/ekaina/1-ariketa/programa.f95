program diferentzia
      use mcf_tipos
      use mcf_slineales
      real(kind=dp),allocatable,dimension(:)::A,B,C,D,x,y0,y1
      real(kind=dp)::h
      integer,parameter::n=1000
      integer::k,i
      real(kind=dp),parameter::xa=0.0_dp,xb=10.0_dp,eps=1.0E-6_dp
      h=(xb-xa)/real(n-1,dp)
      allocate(x(n),y0(n),y1(n),A(n),B(n),C(n),D(n))
      do i=1,n
      x(i)=xa+h*real(i-1,dp)
      end do
      open(unit=11,file="hasiera.dat",status="replace",action="write")
      do i=2,n-1
      D(i)=-2.0_dp/h**2
      A(i)=1.0_dp/h**2
      C(i)=1.0_dp/h**2
      end do
      D(1)=1.0_dp
      D(n)=1.0_dp
      A(1)=0.0_dp
      C(1)=0.0_dp
      A(n)=0.0_dp
      C(n)=0.0_dp
      B(2:n-1)=0.0_dp
      B(1)=0.0_dp
      B(n)=0.04347274616886_dp

      call TRIDAG(A,D,C,B,y0,n)
      do i=1,n
      write(unit=11,fmt="(2f20.14)"),x(i),y0(i)
      end do
!      print*,y0      
    !  do k=1,100

      do i=2,n-1
        B(i)=-(y0(i+1)-y0(i))/(h*x(i))-(x(i)**2-1.0_dp)*y0(i)/x(i)**2
      end do
      
      B(1)=0.0_dp
      B(n)=0.04347274616886_dp
      call TRIDAG(A,D,C,B,y1,n)
 !     print*,y1
     ! if(sum(abs(y1-y0))/n<eps) then
     !         exit
!      else
      !y0=y1
     ! print*,y1
 !     end if
     ! end do
      open(unit=12,file="emaitza.dat",status="replace",action="write")

      do i=1,n
      write(unit=12,fmt="(2f20.14)"),x(i),y1(i)
      end do
      close(unit=11)
      close(unit=12)
      deallocate(x,y0,y1,A,B,C,D)
end program diferentzia      
