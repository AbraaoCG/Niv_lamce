import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from math import e,pi

#Define a array de dataset
dataframe = pd.read_csv("AirPassengers.csv",sep = ",")

dados_mes = dataframe["Month"].values.tolist()
dados_passageiros = dataframe["#Passengers"].values.tolist()

dados_x = np.arange(1,len(dados_mes)+1)
dados_passageiros = np.array(dados_passageiros)

##Define o passo de tempo
t_inicial = 108
t_final = 132
passo_temporal = t_final/t_final # ???? sempre 1

##Atualiza a array de dados de passageiros
dados_passageiros = dados_passageiros[t_inicial-1:t_final-1]

##Atualiza a array de dados x
dados_x = dados_x[t_inicial-1:t_final-1]

##Plota o gráfico do problema
plt.figure("Passageiros x Mês")
plt.plot(dados_x, dados_passageiros)
plt.xlabel("Mês")
plt.ylabel("Passageiros")
plt.grid(True)
#plt.show()

##Retira a tendencia do dataset
tendencias =[]
for x in dados_x:
    tendencia = 4.8136682*e**(0.0100483*x)
    list.append(tendencias,tendencia)
tendencias = np.array(tendencias)

dados_passageiros = dados_passageiros - tendencias
    
##Plota o gráfico do problema sem tendencia
plt.figure("Passageiros Sem Tendência x Mês")
plt.plot(dados_x, dados_passageiros)
plt.xlabel("Mês")
plt.ylabel("Passageiros")
plt.grid(True)
#plt.show()

##Define o numero de pontos
numero_pontos = (t_final-t_inicial)/passo_temporal
numero_pontos = int(numero_pontos)

##Discretiza o dominio do tempo
dados_t = np.linspace(t_inicial,t_final,numero_pontos)

##Define o dominio das frequencias
frequencia_aquisicao = numero_pontos/t_final
frequencia_resolucao = frequencia_aquisicao/numero_pontos

print(numero_pontos , t_final)

dados_frequencia_lista = []
for inteiro in range(numero_pontos):
    frequencia = frequencia_resolucao*inteiro
    list.append(dados_frequencia_lista,frequencia)
dados_frequencia = np.array(dados_frequencia_lista)

##Aplica a FFT
fft_passageiros = np.fft.fft(dados_passageiros)
amplitude_sinal = 2*np.abs(fft_passageiros)/numero_pontos
mascara_freq = dados_frequencia >= 0      

# Calcula Fase
fase = np.angle(fft_passageiros)

##Plota o gráfico da sinal da FFT
plt.figure("FFT Sinal Sem Tendência")
plt.xlim(left = dados_frequencia[0], right = frequencia_aquisicao/2)
plt.plot(dados_frequencia,amplitude_sinal)
plt.xlabel("Frequência (1/mês)")
plt.ylabel("Amplitude")
plt.grid(True)
#plt.show()

##Acha as frequencias de pico do sinal original
frequencias_uteis = []
for frequencia in dados_frequencia:
    if frequencia <= frequencia_aquisicao/2:
        list.append(frequencias_uteis,frequencia)
frequencias_uteis = np.array(frequencias_uteis)
print("Frequencias uteis : " , frequencias_uteis)
print("\n")
print("amplitude_sinal : " , amplitude_sinal[:len(frequencias_uteis)])
        
#dados_frequencia_indice = np.arange(1,len(frequencias_uteis)+1)
#print(str.format("A primeira frequencia de pico é {} 1/mes", np.nanmax(dados_frequencia[0.07:0.1])))
#print(str.format("A segunda frequencia de pico é {} 1/mes", np.nanmax(dados_frequencia[0.1:0.2])))
#print(str.format("A terceira frequencia de pico é {} 1/mes", np.nanmax(dados_frequencia[0.2:0.3])))
#print(str.format("A quarta frequencia de pico é {} 1/mes", np.nanmax(dados_frequencia[0.3:0.4])))
#print(str.format("A quinta frequencia de pico é {} 1/mes", np.nanmax(dados_frequencia[0.4:0.5])))

#82.9610168 -> 0.01515152 #3
#36.7291042 -> 0.03030303 #5
#16.3767609 -> 0.04545455 #7
#13.3303184 -> 0.07575758 #11

x_modelo = np.arange(108,132,1)
y_modelo = []
for x in x_modelo:
    y = 4.8136682*e**(0.0100483*x)+82.9610168*np.cos(2*pi*0.01515152*x )+36.7291042*np.cos(2*pi*0.03030303*x)+16.3767609*np.cos(2*pi*0.04545455*x)+13.3303184*np.cos(2*pi*0.07575758*x)
    list.append(y_modelo,y)
y_modelo = np.array(y_modelo)

plt.figure("Predicao")
plt.plot(x_modelo,y_modelo)
plt.xlabel("Meses")
plt.ylabel("Passageiros")
plt.grid(True)
#plt.show()




















