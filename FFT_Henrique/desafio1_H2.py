import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from math import e,pi
from sklearn.linear_model import LinearRegression

#Define a array de dataset
dataframe = pd.read_csv("AirPassengers.csv",sep = ",")

dados_mes = dataframe["Month"].values.tolist()
dados_passageiros = dataframe["#Passengers"].values.tolist()

dados_x = np.arange(1,len(dados_mes)+1)
dados_passageiros = np.array(dados_passageiros)

n = len(dados_passageiros)

##Define o passo de tempo
t_inicial = 108
t_final = 132
if (len(dados_x > 1)): passo_temporal = dados_x[1] - dados_x[0]

##Atualiza a array de dados de passageiros


dados_passageiros_treino = dados_passageiros[t_inicial :t_final]
dados_passageiros_teste = dados_passageiros[t_final :n]

##Atualiza a array de dados x

dados_x = dados_x[t_inicial :t_final]

print(len( dados_x))

##Plota o gráfico do problema
plt.figure("Passageiros x Mês")
#plt.plot(dados_x, dados_passageiros_treino)
plt.xlabel("Mês")
plt.ylabel("Passageiros")
plt.grid(True)
#plt.show()

#Regressão exponencial
def exponencial(X,y): # y(x) = A0 * eˆ(A1 * x)  --- > y_ln (x) = Ao_ln + A1 * x
  y_ln =  np.log(y)
  model2 = LinearRegression()
  model2.fit(X,y_ln)

  return ( [ model2, np.exp( model2.predict(X) ) ] )

##Retira a tendencia do dataset
tendencias =[]
for x in dados_x:
    tendencia = 4.8136682*e**(0.0100483*x)
    list.append(tendencias,tendencia)
#tendencias = np.array(tendencias)

#print(dados_passageiros_treino)
treino = pd.DataFrame( { 'tempo' : dados_x , 'passageiros' : dados_passageiros_treino}) ; treino.index = treino.index + t_inicial

X = treino.drop('passageiros', inplace=False, axis=1)

resultExp  = exponencial(X , treino['passageiros'])


y_treino_Exp = resultExp[1]
y_treino_Exp_df = pd.DataFrame( {'Exp' : y_treino_Exp}) ; y_treino_Exp_df.index = y_treino_Exp_df.index + t_inicial
treino = pd.concat([treino , y_treino_Exp_df] , axis = 1)

##Plota o gráfico residuos e exponencial do residuo
plt.figure("Passageiros x Mês")
plt.plot(treino['tempo'], treino['passageiros'])
plt.plot(treino['tempo'], treino['Exp'])
plt.xlabel("Mês")
plt.ylabel("Passageiros")
plt.grid(True)
#plt.show()

dados_passageiros_residuos = dados_passageiros_treino - y_treino_Exp
pass_resid_treino_df = pd.DataFrame( {'ResiduoExp' : dados_passageiros_residuos}) ; pass_resid_treino_df.index = pass_resid_treino_df.index + t_inicial
treino = pd.concat([treino , pass_resid_treino_df] , axis = 1)


##Plota o gráfico do problema sem tendencia
plt.figure("Passageiros Sem Tendência x Mês")

plt.plot(treino['tempo'], treino['ResiduoExp'])
plt.xlabel("Mês")
plt.ylabel("Passageiros")
plt.grid(True)
plt.show()

##Define o numero de pontos
numero_pontos = (t_final-t_inicial)/passo_temporal
numero_pontos = int(numero_pontos)

##Discretiza o dominio do tempo
dados_t = np.linspace(t_inicial,t_final,numero_pontos)

##Define o dominio das frequencias
frequencia_aquisicao = numero_pontos/ ( t_final - t_inicial)
frequencia_resolucao = frequencia_aquisicao/numero_pontos

dados_frequencia_lista = []
for inteiro in range(numero_pontos ):
    frequencia = frequencia_resolucao * inteiro * numero_pontos
    list.append(dados_frequencia_lista,frequencia)
dados_frequencia = np.array(dados_frequencia_lista)

##Aplica a FFT

fft_residuo_passageiros = np.fft.fft(treino['ResiduoExp'])
amplitude_sinal = (2/ numero_pontos) * np.abs(fft_residuo_passageiros)

mascara_freq = dados_frequencia >= 0      

#print("amp: ", len(amplitude_sinal) )

#print("freq : " , len(dados_frequencia) )

# Calcula Fase
fase = np.angle(fft_residuo_passageiros)

print(treino)

# Cria Tabela FFT

tabela_FFT = pd.DataFrame( {'Amplitude' : amplitude_sinal , 'frequência' : dados_frequencia , 'fase' : fase } )
n_fft = len(tabela_FFT)

tabela_FFT_sorted = tabela_FFT.sort_values('Amplitude' , ascending=False )
print(tabela_FFT_sorted)

##Plota o gráfico da sinal da FFT
plt.figure("FFT Sinal Sem Tendência")
plt.xlim(left = dados_frequencia[0], right = (len(dados_frequencia) / 2 ) + 1)
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
#print("Frequencias uteis : " , frequencias_uteis)
#print("\n")
#print("amplitude_sinal : " , amplitude_sinal[:len(frequencias_uteis)])
        
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

x_modelo = treino['tempo']
periodo = 12
tempos = []
y_fft_pred = []
for x in range(0, len(x_modelo) ):
    t = x / ( periodo )
    
    pred_fft_residuo = 74.566102 * np.cos(2*pi* 1 *t - 3.095300  ) + 42.775958 *np.cos(2*pi* 2 *t - 0.626404 )  + 15.829363  * np.cos(2*pi* 3 *t + 2.024186  ) # + 14.265569*np.cos(2 * pi * 5* t -1.466283 )
    tempos.append( (x+1) + t_inicial)
    list.append(y_fft_pred, pred_fft_residuo)

plt.figure("Predicao")

plt.plot(x_modelo,dados_passageiros_residuos)

plt.plot(tempos,y_fft_pred , color = "red")

plt.xlabel("Meses")
plt.ylabel("Passageiros")
plt.grid(True)
plt.show()




















