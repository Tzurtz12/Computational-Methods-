set terminal pdf
set output "hasiera.pdf"
plot "hasiera.dat" u 1:2 w l
