set terminal pdf
set output "rk4.pdf"
plot "rk4.dat" u 1:2 w l
