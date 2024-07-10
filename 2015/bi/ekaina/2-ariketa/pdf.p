set terminal pdf
set output "emaitza.pdf"
splot "finkatua.dat" u 1:2:3 w l
