set terminal png size 1920,1080
set output 'outputErro.png'

set key inside bottom right
set pointsize 10
set xlabel 'X'
set ylabel 'Y'
set title 'Nuvem de pontos'

plot 'epocaXerro.txt' using 1:2 title 'Erro em função das épocas' w line
