set terminal pdf
set output "interpolatua.pdf"
plot "spline.dat" u 1:2 w l
