program ariketa1
      use mcf_tipos


      do i=1,nx
      x(i)=xa+(xb-xa)*real(i-1,dp)/real(nx-1,dp)
      rho0(i)=P0*exp(-(x(i)**2/4*L**2))
      end do

      !Erlaxazio metodoa aplikatuz
      k=0.001_dp

      do j=1,nt
      t(j)=ta+(tb-ta)*real(j-1,dp)/real(nt-1,dp)
        do i=2,nx-1
        rho1(i)=tau*k/h**2*(rho0(i+1)-2.0_dp*rho0(i)+rho0(i-1))+&
                tau*k*p(i,t(j))+rho0(i)
        end do
        rho0=rho1
      end do
      !Presioaren balioa daukagu t=0 aldiunerarte
       
      !Uhin ekuazioan sartuz

      do j=1,nt
        do i=2,nx-1
                rho2(i)=(v*tau/h)**2*(rho1(i+1)-2.0_dp*rho1(i)+rho1(i-1))+tau**2*rho1(i)-&
                        rho0(i)
        end do
        rho0=rho1
        rho1=rho2
      end do
        


contains
        function p(k,t)
          integer,intent(in)::,k
          real(kind=dp)::p,P0,w
          real(kind=dp),intent(in)::t
          P0=
          w=

          p(75)=P0*sin(w*t)
        end function p







end program ariketa1      
