set terminal png size 1920,1080
set output 'output.png'

set key inside bottom right
set pointsize 10
set xlabel 'X'
set ylabel 'Y'
set title 'Nuvem de pontos'

f(x) =  1.8016483035417004 * x + 1.1487254240724718

plot 'dataset02.txt' using 1:2 title 'pontos' w points pt 7 pointsize 5, f(x) title 'Reg Grad. Descendente'
