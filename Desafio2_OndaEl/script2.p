
# Scale font and line width (dpi) by changing the size! It will always display stretched.
set terminal png size 1920,1080
set output 'VelocidadeXtempo.png'


# Key means label...
set key inside bottom right
set xlabel 'Tempo'
set ylabel 'Velocidade'
set title 'Onda Elástica 1d'
plot  "data.txt" using 1:3 title 'Velocidade' with lines