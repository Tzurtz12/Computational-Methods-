set terminal pdf
set output "funtzioak.pdf"
plot "funtzioak.dat" u 1:2 w l, "funtzioak.dat" u 1:3 w l
