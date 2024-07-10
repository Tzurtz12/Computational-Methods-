program gauss
      use mcf_tipos
      use funtzioa
      use gauss_legendre
      real(kind=dp)::a,b,I
      open(unit=11,file="integrala.dat",status="replace",action="write")
      a=0.0_dp
      b=1.0_dp/sqrt(3.0_dp)
      call qgauss(f,a,b,I,3)

      write(unit=11,fmt="(a,1f20.12)"),"I=",I
      close(unit=11)

end program gauss      
