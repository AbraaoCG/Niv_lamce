
# Scale font and line width (dpi) by changing the size! It will always display stretched.
set terminal svg size 1920,1080 enhanced fname 'arial' background rgb 'white'
set output 'out.svg'


# Key means label...
set key inside bottom right
set xlabel 'Tempo'
set ylabel 'Período'
set title 'Periodo x Tempo no Pêndulo Amortecido - 179 Graus - Qsi= 0.5%'
plot  "data2.txt" using 1:2 title 'Theta' with p

