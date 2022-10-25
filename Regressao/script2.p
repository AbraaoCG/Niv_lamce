# Scale font and line width (dpi) by changing the size! It will always display stretched.
set terminal svg size 1920,1080 enhanced fname 'arial' background rgb 'white'
set output 'out.svg'

# Key means label...
set key inside bottom right
set xlabel 'Aquisicoes'
set ylabel 'Y (Petr3) / Y_Pred (Petr3 - Predicao)'
set title 'Regressao Multivariada - Ibov + OIl --> Petr3 '
plot  "output.txt" using 1:2 title 'Y' with line, "output.txt" using 1:3 title 'Y - Pred' with line