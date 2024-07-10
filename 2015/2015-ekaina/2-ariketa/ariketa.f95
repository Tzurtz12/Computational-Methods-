program potentzial
      use mcf_tipos
       real(kind=dp),allocatable,dimension(:,:,:)::Q,V2,V
       real(kind=dp),allocatable,dimension(:)::x,y
       real(kind=dp)::L,karga,h,nn
       integer,parameter::n=101
       integer::m,i,j,k,nq,nz

      open(unit=12,file="potentzialz.dat",status="replace",action="write")
      allocate(Q(n,n,n),V(n,n,n),V2(n,n,n),x(n),y(n))
      L=2.0_dp
      karga=10.0_dp

      h=L/real(n-1,dp)
      nn=real(n)

      !Karga tentsorea:
      Q(:,:,:)=0.0_dp
      nq=ceiling(nn/2.0_dp)
      Q(nq,nq,nq)=karga
      V2(:,:,:)=0.0_Dp

      do m=1,100
        V(:,:,:)=0.0_dp
        do i=2,n-1
                do j=2,n-1
                        do k=2,n-1
                        V(i,j,k)=(V2(i+1,j,k)+V2(i-1,j,k)+V2(i,j+1,k)+V2(i,j-1,k)+&
                        V2(i,j,k+1)+V2(i,j,k-1)+h**2*Q(i,j,k))/6.0_dp
                        end do
                end do  
        end do
        V2=V
      end do
!z=L/4 puntua irudikatzeko
nz=ceiling(3.0_dp*nn/4.0_dp)
do i=1,n        
        x(i)=-L/2.0_dp+h*real(i-1,dp)
        do j=1,n
        y(j)=-L/2.0_dp+h*real(j-1,dp)
        write(unit=12,fmt="(3f20.12)"),x(i),y(j),V(i,j,nz)
        end do
end do        
deallocate(V,V2,Q,x,y)
close(unit=12)

 end program potentzial      
