program ariketa
      use mcf_tipos
      use mcf_slineales
      use mcf_spline
      real(kind=dp),allocatable,dimension(:)::A,B,C,D,x,y0,y1,ys,xs,y2,yi
      real(kind=dp)::h,xinterp,yinterp,batu1,batu2,batu3,DI
      real(kind=dp),parameter::L=1.0_Dp,eps=1.0E-8_dp
      integer::i
      integer,parameter::ny=1000
      
      allocate(x(ny),y0(ny),y1(ny),A(ny),B(ny),C(ny),D(ny))
      !Espazioa diskretizatu

      h=2.0_dp*L/real(ny-1,dp)
      do i=1,ny
      x(i)=h*real(i-1,dp)
      end do
      do i=2,ny-1
      A(i)=1.0_dp/h**2
      C(i)=1.0_dp/h**2
      D(i)=-2.0_dp/h**2
      end do

      !Mugalde baldintzak 
      D(1)=1.0_dp
      D(ny)=1.0_dp
      A(1)=0.0_dp
      A(ny)=0.0_dp
      C(1)=0.0_dp
      C(ny)=0.0_dp

      B(2:ny-1)=0.0_dp
      B(1)=0.0_dp
      B(ny)=1.0_dp
      
      !lehen hurbilketa

      call TRIDAG(A,D,C,B,y0,ny)
      do
      do i=2,ny-1
        B(i)=5.0_dp*sqrt(1.0_dp+((y0(i+1)-y0(i))/h)**2)
      end do
      B(1)=0.0_dp
      B(ny)=1.0_dp
      call TRIDAG(A,D,C,B,y1,ny)
      if(sum(abs(y1-y0))/ny<eps) then
              exit
      else
      y0=y1
      end if
      end do
      open(unit=11,file="emaitzak.dat",status="replace",action="write")

      do i=1,ny
        write(unit=11,fmt="(2f20.12)"),x(i),y1(i)
      end do
      close(unit=11)
!--------------------SPLINE-------------------------
      allocate(xs(5),ys(5),y2(5),yi(ny))
      open(unit=12,file="spline.dat",status="replace",action="write")
      do i=1,5
      xs(i)=0.0_dp +L*real(i-1,dp)/2.0_dp
      end do
      ys(1)=y1(1)
      ys(2)=y1(250)
      ys(3)=y1(500)
      ys(4)=y1(750)
      ys(5)=y1(ny)      
      
      call spline(xs,ys,5,y2)
      do i=1,ny
      xinterp=+2.0_dp*L*real(i,dp)/real(ny,dp)
      call splint(xs,ys,y2,5,xinterp,yinterp)
      write(unit=12,fmt="(2f20.12)"),xinterp,yinterp
      yi(i)=yinterp
      end do
 !---------------INTEGRALA---------------------
      open(unit=13,file="integrala.dat",status="replace",action="write")
      batu1=0.0_dp
      batu2=0.0_dp
      !Trapezioaren erregelarekin

      do i=1,ny-1
      batu1=batu1+((yi(i+1)-y1(i+1))**2+(yi(i)-y1(i))**2)*0.5_dp*h
      batu2=batu2+(abs(y1(i+1))+abs(y1(i)))*0.5_dp*h
      end do

      batu3=sqrt(batu1)
      DI=batu3/batu2

      write(unit=13,fmt="(4f20.12)"),batu1,batu2,batu3,DI


      deallocate(A,B,C,D,x,y0,y1,xs,ys,y2,yi)
end program ariketa      
