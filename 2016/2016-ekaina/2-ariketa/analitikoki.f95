program analitiko
      use mcf_tipos
      use funtzioa
      use mcf_interpoli
      use improper_cuadratura
      real(kind=dp)::a,b,x1,x2,w1,w2,I,I2,er
        x1=(-1.0_dp+sqrt(3.0_dp))/6.0_dp
        x2=(1.0_dp+sqrt(3.0_dp))/6.0_dp
        w1=1.0_dp/(2.0_dp*sqrt(3.0_dp))
        w2=1.0_dp/(2.0_dp*sqrt(3.0_dp))
        open(unit=11,file="emaitza.dat",status="replace",action="write") 
        a=0.0_dp
        b=1/sqrt(3.0_dp)
        I=w1*f(x1)+w2*f(x2)

        call romberg_improper(f,a,b,I2,2)
        er=abs(I-I2)
        write(unit=11,fmt="(3f20.12)"),I,I2,er
end program analitiko      
