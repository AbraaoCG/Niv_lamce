
# Scale font and line width (dpi) by changing the size! It will always display stretched.
set terminal png size 1920,1080 
set output 'GraficoElbow.png'


# Key means label...



set key inside bottom right
set pointsize 10
set xlabel 'número de Grupos'
set ylabel 'Within Cluster Sum of Squares'
set title 'Análise de Cotovelo'
plot  "elbowData.txt" using 1:2 w line