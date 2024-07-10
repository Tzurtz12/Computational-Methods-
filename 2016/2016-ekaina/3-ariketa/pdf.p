set terminal pdf
set output "emaitza1.pdf"
plot("emaitzakb.dat") u 1:2 w l
