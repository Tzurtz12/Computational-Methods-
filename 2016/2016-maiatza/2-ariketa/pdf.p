set terminal pdf
set output "emaitza.pdf"
plot("emaitza.dat") u 1:2 w l
