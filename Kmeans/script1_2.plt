
# Scale font and line width (dpi) by changing the size! It will always display stretched.
set terminal png size 1920,1080 
set output 'GraficoGrupos_Comparacao.png'


# Key means label...



set key inside bottom right
set pointsize 10
set xlabel 'X1'
set ylabel 'X2'
set title 'Classificação Kmeans'
plot  "grupo1.txt" using 1:2 title 'Grupo1' w points pt 7 pointsize 4 lc rgb "blue" , "grupo2.txt" using 1:2 title 'Grupo2' w points pt 7 pointsize 4 lc rgb "orange" , "grupo3.txt" using 1:2 title 'Grupo3' w points pt 7 pointsize 4 lc rgb "purple" , "centrosFinal.txt" using 1:2 title 'CentrosCodigo' w points pt 7 pointsize 7 lc rgb "red" , "centrosSklearn.txt" using 1:2 title 'CentrosBiblioteca' w points pt 7 pointsize 7 lc rgb "black" 
 
