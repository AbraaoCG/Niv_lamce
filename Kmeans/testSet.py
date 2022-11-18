# Arquivo para geração de conjunto de pontos aleatórios.

import numpy as np
import random
with open('testData.txt', 'w') as f:
    n = 100
    dimensions = 2
    for point in range(n):

        for dim in range(dimensions):
            f.write( str(random.random()) )
            f.write('\t')
        f.write('\n')
