set terminal pdf
set output "enoluzioa.pdf"
splot "emaitza.dat" u 1:2:3 w l
