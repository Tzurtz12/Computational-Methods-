program Newtoncotes10
        use mcf_tipos
        use mcf_interpoli
        integer,parameter::n=11,m=1000
        real(kind=dp),dimension(n)::x,y,w
        real(kind=dp),dimension(m)::f
        real(kind=dp)::h,hm,xinterp,yinterp,errorea
        integer::i,j
        real(kind=dp),parameter::a=0.0_dp,b=1.0_dp



        !n+1 puntu datu behar ditugu, lagrangen polinomioekin
        !hurbiltzeko

        !h=(b-a)/10 dugu
        h=(b-a)/real(n-1,dp)
        hm=(b-a)/real(m-1,dp)!interpolaziorako
        open(unit=12,file="pisuak.dat",status="replace",action="write")
        !x bektorea
        do i=1,n
        x(i)=a+real(i-1,dp)*h
        end do
        y=0.0_dp
        do i=1,n
        y(i)=1.0_dp 
                do j=1,m
                xinterp=a+real(j-1,dp)*hm
                call polint(x,y,n,xinterp,yinterp,errorea)
                f(j)=yinterp
                end do


                do j=1,m-1
                w(i)=w(i)+(f(j+1)+f(j))*hm*0.5_dp!Trapezio ekuazioa
                end do
                y=0.0_dp
        end do
        do i=1,n-1
        print*,"w(",i-1,")=",w(i)
        write(unit=12,fmt="(2f20.12,a)"),w(i),w(i)/h,"h"
        end do
end program  Newtoncotes10      
