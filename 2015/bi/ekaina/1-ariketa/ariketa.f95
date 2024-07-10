program integral
      use mcf_tipos 
      use mcf_interpoli
      real(kind=dp),dimension(3)::x,y,w
      real(kind=dp)::h,xinterp,yinterp,errorea
      integer,parameter::m=1000
      real(kind=dp),dimension(m)::f
      real(kind=dp),parameter::a=0.0_dp,b=2.0_dp
      integer::i,j
      
      !pisuak ondorioztatu
      h=(b-a)/real(m-1,dp)

     ! do i=1,m
     ! x(i)=a+h*real(i-1,dp)
     ! end do

      x(1)=0.0_dp
      x(2)=1.0_dp
      x(3)=2.0_dp
      y=0.0_dp
      do j=1,3
        y(j)=1.0_dp
        do i=1,m
        xinterp=x(1)+h*real(i-1,dp)
        call polint(x,y,3,xinterp,yinterp,errorea)
        f(i)=yinterp
        end do
!trapezioaren erregela
        w(j)=0.0_dp

        do i=1,m-1
        w(j)=w(j)+(f(i+1)+f(i))*h*0.5_dp
        end do

        y=0.0_dp
     end do


    do i=1,3
        print*,"w(",i,")=",w(i)
    end do
end program integral      
