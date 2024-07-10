program t 
integer :: i
character(len=10) :: num

open(unit=100,file="irudikatu_gnup",status="old",action="write")

do i=100001,100000+999
 write(num,"(i6)")i
 write(unit=100,fmt="(a)")trim("plot 'fort."//trim(num)//"' u 1:2 w l" )
enddo


close(unit=100)


end program t
