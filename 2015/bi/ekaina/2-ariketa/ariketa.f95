program kubo
      use mcf_tipos
      integer,parameter::n=101
      real(kind=dp)::h
      real(kind=dp),parameter::L=2.0_dp
      integer::i,j,m,k,np,nz
      real(kind=dp),allocatable,dimension(:,:,:)::V2,V,Q
      real(kind=dp),allocatable,dimension(:)::x,y
      h=L/real(n-1,dp)
      allocate(x(n),y(n),V(n,n,n),V2(n,n,n),Q(n,n,n))

     ! do i=1,n
     ! x(i)=-L/2.0_dp+h*real(i-1,dp)
     ! y(i)=-L/2.0_dp+h*real(i-1,dp)
     ! z(i)=-L/2.0_dp+h*real(i-1,dp)
     ! end do
      Q(:,:,:)=0.0_dp
      np=ceiling(real(n)/2.0_dp)
      Q(np,np,np)=10.0_dp
      V2(:,:,:)=0.0_dp
      do m=1,100
      V(:,:,:)=0.0_dp
      do k=2,n-1
        do j=2,n-1
                do i=2,n-1
                V(i,j,k)=(V2(i+1,j,k)+V2(i-1,j,k)+V2(i,j+1,k)+&
                        V2(i,j-1,k)+V2(i,j,k+1)+V2(i,j,k-1)+h**2*Q(i,j,k))/6.0_dp
                end do
        end do
      end do
      V2=V

      end do
      nz=ceiling(3.0_dp*real(n)/4.0_dp)

      open(unit=11,file="finkatua.dat",status="replace",action="write")

      do j=1,n
      y(j)=-L/2.0_dp+h*real(j-1,dp)
        do i=1,n
        x(i)=-L/2.0_dp+h*real(i-1,dp)
        write(unit=11,fmt="(3f20.12)"),x(i),y(j),V(i,j,nz)
        end do
      end do
      close(unit=11)
      deallocate(V,V2,Q,x,y)
end program kubo      
