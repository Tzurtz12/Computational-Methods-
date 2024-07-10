program tds
use mcf_tipos
use mcf_slineales
integer :: i,j, jj
real(kind=dp) :: h
real(kind=dp), parameter :: a=0.0_dp, d=1.0_dp, L=50.0_dp, &
       k=4.0_dp ,x0=-15.0_dp, tau=0.0001, v0=0.9*k**2
integer, parameter :: n=1000
complex, allocatable, dimension(:) :: phi0, phi1, b, extr, diag
real(kind=dp), allocatable, dimension(:) ::  x
complex :: r


open(unit=123, file="potentzial_irudia.dat", status="old", action="write")


allocate   (phi0(n), phi1(n), b(n), x(n), extr(n), diag(n))

do i=1,n
 x   (i) =  - L + 2 * L * (i-1)/(n-1)
 phi0 (i) = exp(-(x(i)-x0)**2) * exp( (0.0_dp,1.0_dp)* k *x(i) )
 write(unit=123, fmt="(4es18.10)")x(i), pot (x(i),a,d,v0), phi0 (i)
enddo

h=2*L/(n-1)
r= (0.0_dp,1.0_dp) *tau/(2*h**2)

do j=1,1000
do jj=1,100

do i=1,n
 extr(i)=       -r/2
 diag(i)=(1.0_dp+r) + pot (x(i),a,d,v0) * r*h**2
 if (i==1) then
   b   (i)=                   ((1-r) - pot (x(i),a,d,v0)*r*h**2) * phi0 (i) + r/2 * phi0(i+1)
 else if (i==n) then
   b   (i)= r/2 * phi0(i-1) + ((1-r) - pot (x(i),a,d,v0)*r*h**2) * phi0 (i)        
 else 
   b   (i)= r/2 * phi0(i-1) + ((1-r) - pot (x(i),a,d,v0)*r*h**2) * phi0 (i) + r/2 * phi0(i+1)       
 end if 

enddo !i
call CTRIDAG(extr,diag,extr,b,phi1,n)
phi0=phi1
end do !jj

do i=1,n
 write(unit=100000+j,fmt="(100f12.5)") x(i),abs(phi1(i))
enddo

end do !j 

deallocate (phi0,    phi1   )

close(unit=123)

contains


function pot (x,a,d,v0)
real(kind=dp), intent(in) :: x,a,d, v0
real(kind=dp)             :: pot

!pot=x**2
pot=0.0_dp
if (abs(x-a)<d) then
 pot = pot + v0
end if

end function pot

end program tds
