set terminal png size 1920,1080
set output 'outputSegmentado.png'

set key inside bottom right
set pointsize 10
set xlabel 'X'
set ylabel 'Y'
set title 'Nuvem de pontos'

f(x) = 0.35170205649088904  * x + 1.3018744452721576 
g(x) = -1.3161039190316788  * x + 2.6908146145405576
n(x) =  1.3577667421541055 * x + 1.6287641565727426 

m(x) = 1.8016483035417004 * x + 1.1487254240724718

plot 'dataJan1.txt' using 1:2 title 'Janela1' w points pt 7 pointsize 5 lc rgb "black", [2.07956E-002:0.388 ] f(x) lc rgb "black" , 'dataJan2.txt' using 1:2 title 'Janela2' w points pt 7 pointsize 5 lc rgb "blue",[0.397:0.708] g(x) lc rgb "blue", 'dataJan3.txt' using 1:2 title 'Janela3' w points pt 7 pointsize 5 lc rgb "purple",[0.729:0.940] n(x) lc rgb "purple", m(x) lc rgb "red"
