import pandas as pd

agrup1 = pd.read_csv('agrupamento_iris.txt' , sep = ';') # agrupamento código criado

agrup1['Grupo'].replace(3,'X' , inplace = True) # X--> 1
agrup1['Grupo'].replace(1,'Y' , inplace = True) # Y--> 2
agrup1['Grupo'].replace(2,0 , inplace = True) ; agrup1['Grupo'].replace('X',1 , inplace = True) ;agrup1['Grupo'].replace('Y',2 , inplace = True)


agrup2 = pd.read_csv('agrupamento_iris_Sklearn.txt' , sep = '\t') # Agrupamento biblioteca Sklearn

agrup3 = pd.read_csv('agrupamento_iris_original.txt' , sep = '\t')

agrup3.replace( {'versicolor': 2 , 'setosa' : 1 , 'virginica': 0} , inplace = True)
agrup3.rename( columns={'species' : 'Grupo'} , inplace= True)

#print(agrup3['Grupo'])



notEqual1 = []
for x in range(len(agrup1)):
    g1 = agrup1['Grupo'][x]
    g2 = agrup2['Grupo'][x]
    #print(g1,g2)
    if (  g1 == g2 ):
        pass
    else:
        notEqual1.append(x)

print(notEqual1)
print("Número de diferenças entre biblioteca e implementação: ")
print(len(notEqual1))
