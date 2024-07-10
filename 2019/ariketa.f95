program mintza
      use mcf_tipos
      integer,parameter::nr=160,nt=10000
      real(kind=dp)::tau,h,rx,ry,vx,vy,v
      real(kind=dp),parameter::L=10.0_dp,sigma=1.0_dp,Tn=1.0_dp,xa=0.0_dp,ya=0.0_dp
      real(kind=dp),dimension(nr)::x,y
      real(kind=dp),dimension(nt)::t
      real(kind=dp),dimension(nt,nr,nr)::phi
      integer::i,j,k

      !Espazioa diskretizatu
      

      v=sqrt(Tn/sigma)
      vx=v/sqrt(2.0_dp)
      vy=vx
      h=L/real(nr-1,dp)
      
      do i=1,nr
      x(i)=xa+h*real(i-nr,dp)
      y(i)=ya+h*real(i-nr,dp)
      end do
        
      !denbora diskretizatu

      tau=10.0_dp/real(nt-1,dp)

      do i=1,nt
      t(i)=tau*real(i-1,dp)
      end do

      !parametrizazioa
      rx=tau*vx/h
      ry=tau*vy/h
        
      if (rx>0.25) then
              print*,"parametizazio ezegokia rx",rx
              stop
      else if (ry>0.25) then
              print*,"parametrizazio ezegokia ry",ry
              stop
      end if

      !phi(:,:,:) hiru zutabeko matrizea, denbora,x,y hurrenez hurren
      !hasierako baldintzak
!      phi(:,:,:)=0.0_dp
!      phi(1,:,:)=0.0_dp
!      phi(2,:,:)=0.0_dp
!      phi(:,1,:)=0.0_dp
!      phi(:,nr,:)=0.0_dp
!      phi(:,:,1)=0.0_dp
!      phi(:,:,nr)=0.0_dp
      !hasierako aldiunean mintza:
      do i=2,nr-1
        do j=2,nr-1
        phi(:,i,j)= 0.0001_dp
        if (40<j .and.40<i .and. i<120 .and. j<120 ) then
                       ! if (i<120 .and. j<120) then
                        phi(:,i,j)=0.0_dp
                       ! end if
        end if
        end do
      end do
     ! phi(1,:,:)=0.0_dp
     ! phi(2,:,:)=0.0_dp
      phi(:,1,:)=0.0_dp
      phi(:,nr,:)=0.0_dp
      phi(:,:,1)=0.0_dp
      phi(:,:,nr)=0.0_dp
      open(unit=12,file="hasiera.dat",status="replace",action="write")
      do i=1,nr
        do j=1,nr
        write(unit=12,fmt="(3f20.12)"),x(i),y(j),phi(1,i,j)
        end do
      end do
      close(unit=12)


      do k=2,nt-1
      
        do i=2,nr-1
                do j=2,nr-1
                if (40<j .and.40<i .and. i<120 .and. j<120 ) then
                       ! if (i<120 .and. j<120) then
                        phi(k+1,i,j)=0.0_dp
                else
                        phi(k+1,i,j)=2.0_dp*phi(k,i,j)-phi(k-1,i,j)+rx**2*(phi(k,i+1,j)-&
                        2.0_dp*phi(k,i,j)+phi(k,i-1,j))+ry**2*(phi(k,i,j+1)-&
                        2.0_dp*phi(k,i,j)+phi(k,i,j-1))+tau**2*p(t(k),i,j)/h
              !  if (40<j .and.40<i .and. i<120 .and. j<120 ) then
                       ! if (i<120 .and. j<120) then
               !         phi(k+1,i,j)=0.0_dp
                       ! end if
                end if
               ! phi(1,i,j)=
               ! phi(nt,i,j)=
              !  phi(k,1,j)=0.0_dp
              !  phi(k,nr,j)=0.0_dp
                end do
               ! phi(k,i,1)=0.0_dp
               ! phi(k,i,nr)=0.0_dp
        end do 
       ! phi(k,1,nr)=
       ! phi(k,nr,
      end do       

      open(unit=11,file="amaiera.dat",status="replace",action="write")
      do i=1,nr
        do j=1,nr
        write(unit=11,fmt="(3f20.12)"),x(i),y(j),phi(nt,i,j)
        end do
      end do
      close(unit=11)
contains
      function p(t,i,j)
              real(kind=dp),intent(in)::t
              integer,intent(in)::j,i
              real(kind=dp)::p
              if (k==20 .and.i==20) then
                      p=0.1_dp*sin(t)
              else
                      p=0.0_dp
              end if
      end function p
end program mintza
