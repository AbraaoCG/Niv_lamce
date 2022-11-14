# Segunda tentativa - FFT no gráfico recente sem tendêcia ( Exponencial) - Utilizar parte para validação - Regressao Multiv. da exponencial + senoides - Plot dos senóides encima do gráfico sem tendência


import pandas as pd
import numpy as np
import scipy as scipy
import matplotlib.pyplot as plt
import cmath as cmath

from sklearn.linear_model import LinearRegression
fft = np.fft



data = pd.read_csv('AirPassengers.csv') # Le direto do Colab, sem uso de drive

# serie de datas -- > serie temporal (meses ; passageiros)

data.drop('Month' , inplace = True , axis = 1) 
data.rename( columns = { "#Passengers" : "passageiros"} , inplace = True)
tmp = 0
tmp =[]
n = len(data['passageiros'])

for x in range(n):
  tmp.append(x)
tmp_df = pd.DataFrame( {'meses' : tmp} )
data = pd.concat([data, tmp_df] , axis = 1)
print(data)


yBase = data['passageiros']
tBase = data['meses']

Recorte = [108,132] # Recorte inicio Treino / Validacao
ressonancia = 0
# recorte - R2
# 108 --> Bom começ




# FIT Inicial (y = ax + b  )

X = pd.DataFrame(data['meses'])
y = pd.DataFrame(data['passageiros'])
model = LinearRegression()
model.fit(X, y) 
data['pred_passageiros_linreg'] = model.predict(X)
data['residuals'] = data['passageiros'] - data['pred_passageiros_linreg']


predInicial = data['pred_passageiros_linreg']
ResInicial = data['residuals']


# Plot de y , ypred e resíduo
print('Slope: {:.2e}, Intercept: {:.2f}'.format(model.coef_[0,0], model.intercept_[0]))
print('R-squared: {:.2e}'.format(model.score(X,y)))
plt.plot(X,y)
plt.plot(X ,predInicial, ResInicial)
print( " Azul : Dados \n Laranja : Modelo \n Verde : Resíduos")

#Regressão exponencial
def exponencial(X,y): # y(x) = A0 * eˆ(A1 * x)  --- > y_ln (x) = Ao_ln + A1 * x
  y_ln =  np.log(y)
  model2 = LinearRegression()
  model2.fit(X,y_ln)

  return ( [ model2, np.exp( model2.predict(X) ) ] )

# FIT para avaliacao de tendencia (y = A0 * exp(A1 * t)  )

X = pd.DataFrame(data['meses'])
y = pd.DataFrame(data['passageiros'])
result = exponencial(X,y)
data['pred_passageiros_ExpReg'] =  result[1]
model2 = result[0] # Armazeno modelo de fit exponencial.

data['residuals_exp'] = data['passageiros'] - data['pred_passageiros_ExpReg']

predInicial = data['pred_passageiros_ExpReg']
ResInicial = data['residuals_exp']

# Calculo R2

y_pred_avg = sum( predInicial ) / len(predInicial)
R2 = 1 - (   sum( ( data['residuals_exp'] )**2 ) / sum( (data['passageiros'] - y_pred_avg)**2 )    )

print ("Coef de Determinacao , R2 = " , R2)

# Plot de y , ypred e resíduo , realizando um recorte de interesse nos dados.
recorte = 0
plt.plot(X[recorte:],y[recorte:])
plt.plot(X[recorte:] ,predInicial[recorte:], ResInicial[recorte:])
print( " Azul : Dados \n Laranja : Modelo \n Verde : Resíduos")

# Realizar recorte dos dados para uso dos mais recentes e validacao
# t0 = 118 (118 - 143) . Validacao = 135-143
                 
tTreino = Recorte[0]
tValida = Recorte[1]

data.drop( range(0,tTreino) ,inplace = True)

Valida = data.copy()
Valida.drop(range(tTreino,tValida), inplace = True) # Usado para treino

data.drop (range(tValida,n), inplace = True) # Usado para Validacao do modelo

import scipy.signal
print(len( data['meses'] ))

def FFT(Dados):
  t_seno = data['meses']
  y_seno = data['residuals_exp']

  fft= np.fft
  n = len(y_seno)


  fft_output = fft.fft(y_seno)
  freq = fft.fftfreq(n)
  freq = freq * n
  power = (2 /  n ) * abs(fft_output) 

  fase = np.angle(fft_output)
  # Cria Dataframe e, como fft é simétrico, então recorta-se primeira metade dele.
  fft_df = pd.DataFrame({ 'power' : power , 'freq' : freq , 'fase' : fase} )
  fft_df.drop(range( (n // 2) + 1 , n) ,inplace = True)

  fft_df_sorted = fft_df.sort_values('power' , ascending=False )
  fft_df_sorted = fft_df_sorted.reset_index()

  return [fft_df , fft_df_sorted]

data['residuals_exp']
[fft_output , fft_sorted] = FFT( data ) # FFT com DataFrame de treino

#plt.figure(figsize = (16,8) )
#plt.xlim([0, 50])

#plt.plot( fft_output['freq'] , fft_output['power'], 'ro' )
plt.plot(data['meses'] , data['passageiros'])

print(yBase[100:] )

# ANALISE DE ONDAS

# FFun_Table = Tabela das funções derivadas da transformada de fourier (Fourier Functions Table.)
FFun_Table = pd.DataFrame() 
FFun_Table['tempo'] = data['meses']
FFun_Table['exp'] = data['pred_passageiros_ExpReg']

# A segunda FFun_Table será para predição com modelo ( sem y ), com objetivo de validação, recebendo 'tempos' de validação.
FFun_Table2 = pd.DataFrame()
FFun_Table2['tempo'] = Valida['meses']
FFun_Table2['exp'] = Valida['pred_passageiros_ExpReg']

n_Valida = len(Valida)
n_treino = len(data)
print("n Treino :" , n_treino)

senn1 = np.zeros(n_treino)
senn2 = np.zeros(n_Valida)


numFun = 3

#for i in range(numFun):
for i in range(numFun):
    a_i = fft_sorted['power'][i] 
    f_i = fft_sorted['freq'][i]
    p_i = fft_sorted['fase'][i]
    print(a_i , f_i , p_i , "\n")
    Senoid_Func1 = []
    Senoid_Func2 = []
    if (1): 
      for passo in range (0,n_treino + n_Valida):
        funcao_passo = 0
        tempo = passo / n_treino

        funcao_passo = a_i * np.cos( 2 * np.pi * f_i * tempo + p_i  )
              
        if (passo < n_treino):
          Senoid_Func1.append(funcao_passo)
          senn1[passo] += funcao_passo 
        else:
          Senoid_Func2.append(funcao_passo)
          senn2[passo - n_treino] += funcao_passo

      Senoid_Func1 = np.array(Senoid_Func1)  ; FFun_Table[str(i)]  = Senoid_Func1.tolist()
      Senoid_Func2 = np.array(Senoid_Func2)  ; FFun_Table2[str(i)] = Senoid_Func2.tolist()
    


tmpDataF = pd.DataFrame( { "SumSenoides" : senn1} ) ; tmpDataF.index = tmpDataF.index + tTreino # Conserto index para treino
FFun_Table = pd.concat( [FFun_Table,tmpDataF] , axis=1)

tmpDataF = pd.DataFrame( { "SumSenoides" : senn2} ) ; tmpDataF.index = tmpDataF.index + tValida # Conserto index para validacao
FFun_Table2 = pd.concat( [FFun_Table2,tmpDataF] , axis=1)

#if ( n_treino % 12 == 0 ):
plt.plot(data['meses'] , data['residuals_exp'] , color = 'blue')
plt.plot(data['meses'] , senn1, color = 'red')
plt.plot(Valida['meses'] , senn2 , color = 'red')
plt.plot(Valida['meses'] , Valida['residuals_exp'] , color = 'blue')




# Plot das ondas selecionadas manualmente.

# FFun_Table = Tabela das funções derivadas da transformada de fourier (Fourier Functions Table.)
FFun_Table = pd.DataFrame() 
FFun_Table['tempo'] = data['meses']
FFun_Table['exp'] = data['pred_passageiros_ExpReg']
# A segunda FFun_Table será para predição com modelo ( sem y ), com objetivo de validação, recebendo 'tempos' de validação.
FFun_Table2 = pd.DataFrame()
FFun_Table2['tempo'] = Valida['meses']
FFun_Table2['exp'] = Valida['pred_passageiros_ExpReg']

#n_Valida = len(Valida)
n_treino_fixo = 12 # Período reconhecido da onda.

senn1 = np.zeros(n_treino)
senn2 = np.zeros(n_Valida)

numFun = 2
Senoid_Func1 = []
Senoid_Func2 = []

for passo in range (0,n - tTreino):
  funcao_passo = 0
  tempo = passo / ( n_treino_fixo )
  # Reconstrução do sinal com diferentes parâmetros obtidos em treino.

  #funcao_passo = a_i * np.cos( 2 * np.pi * f_i * tempo + p_i  )
  # Treino 108 - 120 ---> Bom
  #funcao_passo = 70.92850686655498  * np.cos( 2 * np.pi * 1.0   * tempo -3.0425752424178842  ) +  46.57684429670864  * np.cos( 2 * np.pi * 2.0 * tempo -0.6341566384918065  ) +  16.9576747774063 * np.cos( 2 * np.pi * 0.0 * tempo + 3.141593 )
  # Treino 108 - 132 ---> Bom
  funcao_passo = 74.34044358242753  * np.cos( 2 * np.pi * 1.0   * tempo -3.0784817488227474   ) +  43.37449175819999  * np.cos( 2 * np.pi * 2.0 * tempo -0.6327924364631238   ) +  19.536695061821135 * np.cos( 2 * np.pi * 0.0 * tempo + 3.141592653589793  )
  # Treino 120 - 132 ---> Mediano
  #funcao_passo = 77.83975545374135   * np.cos( 2 * np.pi * 1.0   * tempo -3.111198989098018    ) +  40.17223281117008  * np.cos( 2 * np.pi * 2.0 * tempo -0.6312107411522593   ) +  22.115715346235966 * np.cos( 2 * np.pi * 0.0 * tempo + 3.141592653589793  )
  # Treino 110 - 122 --> Bom para Teste > 130 
  #funcao_passo = 78.69342584381933    * np.cos( 2 * np.pi * 1.0   * tempo -2.030323023014732    ) +  39.32600662756102  * np.cos( 2 * np.pi * 2.0 * tempo + 1.4322114156654833    ) +  25.437231346370243 * np.cos( 2 * np.pi * 0.0 * tempo + 3.141592653589793  )
  # Treino 110 - 132 --> Ótimo
  #funcao_passo = 78.27917175819726    * np.cos( 2 * np.pi * 1.0   * tempo -2.0496353981792734     ) +  39.62801910120266  * np.cos( 2 * np.pi * 2.0 * tempo + 1.4565407337608174     ) +  23.86280888895812 * np.cos( 2 * np.pi * 0.0 * tempo + 3.141592653589793  )

  



  if (passo < n_treino):
    Senoid_Func1.append(funcao_passo)
    senn1[passo] += funcao_passo 
  else:
    Senoid_Func2.append(funcao_passo)
    senn2[passo - n_treino] += funcao_passo

Senoid_Func1 = np.array(Senoid_Func1)  ; FFun_Table[str(i)]  = Senoid_Func1.tolist()
Senoid_Func2 = np.array(Senoid_Func2)  ; FFun_Table2[str(i)] = Senoid_Func2.tolist()
    
tmpDataF = pd.DataFrame( { "SumSenoides" : senn1} ) ; tmpDataF.index = tmpDataF.index + tTreino # Conserto index para treino
FFun_Table = pd.concat( [FFun_Table,tmpDataF] , axis=1)

tmpDataF = pd.DataFrame( { "SumSenoides" : senn2} ) ; tmpDataF.index = tmpDataF.index + tValida # Conserto index para validacao
FFun_Table2 = pd.concat( [FFun_Table2,tmpDataF] , axis=1)


plt.plot(data['meses'] , data['residuals_exp'] , color = 'blue')
plt.plot(data['meses'] , senn1, color = 'red')
plt.plot(Valida['meses'] , senn2 , color = 'red')
plt.plot(Valida['meses'] , Valida['residuals_exp'] , color = 'blue')
#print(FFun_Table)

# Preparacao da tabela Var para servir de X ( matrix com variaveis x1,x2,...,xn)
Var = FFun_Table.copy()
Var.drop('tempo', inplace=True, axis=1) # Retiro tempo -- > Regressão sem tendência.
Var.drop("SumSenoides", inplace=True, axis=1) # Retiro tempo -- > Regressão sem tendência.
 
X = Var
y = pd.DataFrame(data['passageiros'])

#Criação do modelo, achando os coeficientes A0 + A1 * x1 + ... + An * Xn + Teste com dados do próprio treino.
model = LinearRegression()
model.fit(X, y)
y_Reg_treino = model.predict(X) # Também usado mais a frente para plotar os senoides encima do gráfico sem tendência

# Criar DataFrame para Ypred_Treino
tmpy_pred_RegMV = []
for x in y_Reg_treino:
  tmpy_pred_RegMV.append ( x[0])
y_Reg_treino_DF = pd.DataFrame( {'y_Reg_treino' : tmpy_pred_RegMV } )

# Ajeitar indices do dataFrame
first_i = y_Reg_treino_DF.first_valid_index()
if (first_i == 0 ): y_Reg_treino_DF.index += tTreino

# Plot do modelo com tendência encima do período de treino.
#plt.plot(FFun_Table['tempo'] , y_Reg_treino )
#plt.plot(FFun_Table['tempo'] , data['passageiros'])

coef = model.coef_
A0 = model.intercept_

#Predição

# Preparar regressão ( Utilizando exponencial.)

Var = FFun_Table2.copy()
Var.drop('tempo', inplace=True, axis=1)
Var.drop('SumSenoides', inplace=True, axis=1)

#print(Var)
X = Var

y_pred_valida = model.predict(X)

# Criar DataFrame para Ypred
tmpy_pred_RegMV = []
for x in y_pred_valida:
  tmpy_pred_RegMV.append ( x[0])
y_Reg_valida_DF = pd.DataFrame( {'y_Reg_valida' : tmpy_pred_RegMV } )

# Ajeitar indices do dataFrame
first_i = y_Reg_valida_DF.first_valid_index()
if (first_i == 0 ): y_Reg_valida_DF.index += tValida

# Plot de gráfico parcial + treino + y_pred de validação
y_pred_valida_SumSen = FFun_Table2['SumSenoides'] + Valida['pred_passageiros_ExpReg']

plt.plot( tBase[100:], yBase[100:])
plt.plot(FFun_Table2['tempo'] , y_pred_valida )

plt.plot( FFun_Table['tempo'] , FFun_Table['SumSenoides'] + data['pred_passageiros_ExpReg'])



#Plot de senoides após regressão encima do gráfico sem tendência

senoides_Treino = y_Reg_treino_DF['y_Reg_treino'] - data['pred_passageiros_ExpReg']
senoides_valida = y_Reg_valida_DF['y_Reg_valida'] - Valida['pred_passageiros_ExpReg'] # Gráfico sem tendência.

# Graf sem tendência --> Preto
# Senoides com regressão sem tendência --> Verde

plt.plot(data['meses'] , data['residuals_exp'] , color='black')
plt.plot(Valida['meses'] , Valida['residuals_exp'] , color='black')

plt.plot(data['meses'] , senoides_Treino , color='green')
plt.plot(Valida['meses'] , senoides_valida ,color='green')
 

# Calculo de R2
y_pred = y_Reg_valida_DF['y_Reg_valida']
y_pred_avg = sum(y_pred) / len(y_pred_valida)
residuals = y_pred - Valida['passageiros']

#R2 = (   sum( (y_pred - y_pred_avg )**2 ) / sum( (Valida['passageiros'] - y_pred_avg)**2 )    )
R2 = 1 - (   sum( (residuals )**2 ) / sum( (data['passageiros'] - y_pred_avg)**2 )    )
print("R2 : " , R2 )
