
# Scale font and line width (dpi) by changing the size! It will always display stretched.
set terminal png size 1920,1080 
set output 'GraficoGrupos_Inicial.png'


# Key means label...

set xrange [0.5:1];
set yrange [0.4:1];

set key inside bottom right
set xlabel 'X1'
set ylabel 'X2'
set title 'Conjunto inicial de pontos'
plot  "dataset01.txt" using 1:2 title 'Deslocamento' w points pt 7 pointsize 4 lc rgb "blue" 