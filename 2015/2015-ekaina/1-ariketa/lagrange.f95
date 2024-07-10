program lagrange
      use mcf_tipos
      use mcf_interpoli

       real(kind=dp),dimension(3)::x,y,w
       integer,parameter::m=1000,n=3
       real(kind=dp),dimension(m)::f
       real(kind=dp)::errorea,xinterp,yinterp,h,hm,a,b,w0,w1,w2
       integer::i,j

      !Lagrangen polinomioekin interpolatuz erraz lor daiteke kalkulu
      !analitikorik egin gabe

      print*,"Hasierako puntua:"
      read*,a
      print*,"Amaierako puntua:"
      read*,b

      open(unit=12,file="integrala.dat",status="replace",action="write")

      hm=(b-a)/real(m-1,dp)
      h=(b-a)/2.0_dp
      x(1)=a
      x(2)=a+h
      x(3)=b
      do i=1,3
      y(i)=1.0_dp
        do j=1,m
                xinterp=a+hm*real(j-1,dp)
                call polint(x,y,n,xinterp,yinterp,errorea)
                f(j)=yinterp
        end do
        !Polinomioa integratuz
        do j=1,m-1
        w(i)=w(i)+(f(j)+f(j+i))*hm*0.5_dp
        end do
        y=0.0_dp
      end do
      w0=w(1)
      w1=w(2)
      w2=w(3)
      print*,w0
      print*,w1
      print*,w2
end program lagrange
