set terminal pdf
set output "hasiera.pdf"
splot "hasiera.dat" u 1:2:3 w l
