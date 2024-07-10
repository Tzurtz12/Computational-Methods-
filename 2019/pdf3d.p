set terminal pdf
set output "mintzabukaeran.pdf"
splot "amaiera.dat" u 1:2:3 w l
