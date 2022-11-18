
# Scale font and line width (dpi) by changing the size! It will always display stretched.
set terminal png size 1920,1080 
set output 'DeslocXtempo.png'


# Key means label...


set key inside bottom right
set xlabel 'Tempo'
set ylabel 'Deslocamento'
set title 'Onda Elástica 1d'
plot  "data.txt" using 1:2 title 'Deslocamento' with lines