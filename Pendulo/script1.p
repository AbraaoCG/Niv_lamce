
# Scale font and line width (dpi) by changing the size! It will always display stretched.
set terminal svg size 1920,1080 enhanced fname 'arial' background rgb 'white'
set output 'out.svg'


# Key means label...
set key inside bottom right
set xlabel 'Tempo'
set ylabel 'Theta(rad) / Omega(rad/s) / Tração(N)'
set title 'Pendulo Simples'
plot  "data.txt" using 1:2 title 'Theta' with lines, "data.txt" using 1:3 title 'Omega' with line, "data.txt" using 1:4 title 'Tracao' with line

