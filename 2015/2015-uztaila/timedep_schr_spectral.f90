program shrodinger
use mcf_tipos
use mcf_diagonalizacion


integer,parameter  :: n=1000
real(kind=dp), parameter    :: L=50.0
real(kind=dp), parameter    :: a=0.0_dp, s=1.0_dp, &
                               x0=-15.0_dp, k=4.0_dp, v0=0.9*k**2
real(kind=dp), dimension(n)   :: D, E
complex(kind=dp), dimension(n)   :: phi0, phit, Koef 
real(kind=dp), dimension(n,n) :: Z

integer :: i,j 
real(kind=dp) :: xi, vi, h, t, tau

h=2*L/n

print*, h

open(unit=110,file="pot.dat",status="replace",action="write")

do i=1, n
 xi = -L + 2*L*real(i-1)/(n-1)
 vi = pot (xi,a,s,v0) 
 phi0 (i) = exp(-(xi-x0)**2) * exp( (0.0_dp,1.0_dp)*k*xi)
 D(i) = (vi + 1.0_dp/h**2) 
 E(i) = - 1.0_dp/(2*h**2)
 write(unit=110,fmt="(10(f18.6))")xi, vi, d(i), e(i) 

enddo

close(unit=110)

z=0.0
do i=1,n
z(i,i) = 1.0
enddo

call  TQLI (D,E,Z)
call  EIGSRT(D,Z)

open(unit=110,file="uhin_funtzioak.dat",status="replace",action="write")

do i=1, n
 xi = -L +2*L*real(i-1)/(n-1)
 write(unit=110,fmt="(1000f18.6)") xi, ( Z(i,j), j=1, n)
enddo

close(unit=110)

do j=1,n
  koef(j)=(0.0_dp,0.0_dp )
 do i=1,n
  koef(j) = koef(j) + (Z(i,j) * phi0 (i))
 end do
enddo

t=0.0_dp
tau=0.01_dp

do i=1,1000
  phit=(0.0_dp,0.0_dp)
  t=t+tau

  do j=1,n
   phit(:) = phit(:) + exp(-(0.0_dp,1.0_dp)*d(j)*t) * koef(j) * Z(:,j)  
  enddo

  do j=1,n
   xi = -L + 2*L*real(j-1)/(n-1)
   write(unit=200000+i,fmt="(100f12.5)") xi,abs(phit(j))
  enddo
 
enddo


contains


function pot (x,a,d,v0)
real(kind=dp), intent(in) :: x,a,d, v0
real(kind=dp)             :: pot

pot=0.0_dp
if (abs(x-a)<d) then
 pot = pot + v0
end if

end function pot


end program shrodinger
 

