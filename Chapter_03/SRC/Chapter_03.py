import os
os.chdir("C:/Users/tprabhan/Documents/My_Books/RSADBE_2.0/R_and_Python_Programs/Python/Chapter_03")
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
MyPath/Python/Chapter_03/
'''
Bar Charts
'''
sc_data = pd.read_csv("Data/SC.csv",delimiter=',')
BT = sc_data.Bug_Type
BFreq = sc_data.Frequency
Ypos = np.arange(len(BT))
plt.barh(Ypos,BFreq,align='center',alpha=0.5)
plt.yticks(Ypos, BT)
plt.xlabel('Severity Counts')
plt.title('Bar graph for the Bug Metrics dataset')
plt.show()

'''
Dot Chart
'''
plt.plot(BFreq,Ypos,'o')
plt.yticks(Ypos, BT)
plt.xlabel('Severity Counts')
plt.title('Dot Chart for the Bug Metrics dataset')
plt.show()


'''
Boxplots
'''
resistivity = pd.read_csv("Data/Resistivity.csv",delimiter=',')
P1 = resistivity['Process.1']
P2 = resistivity['Process.2']
resistivity.boxplot(column=['Process.1','Process.2'])
plt.show()
resistivity.boxplot(column=['Process.1','Process.2'],notch=True)
plt.show()

morley = pd.read_csv("Data/Morley.csv",delimiter=',')
morley.boxplot(column='Speed',by='Expt')
plt.show()


'''
Histograms
'''
Galton = pd.read_csv("Data/Galton.csv",delimiter=',')
Galton.shape
Galton.head(10)
Galton.quantile([0,0.25,0.5,0.75,1])
Galton.mean()
Galton.median()

plt.subplot(1,2,1)
plt.hist(Galton['parent'],bins='fd')
plt.xlim(60,75)
plt.title("Height of Parent")
plt.subplot(1,2,2)
plt.hist(Galton['child'],bins='fd')
plt.xlim(60,75)
plt.title("Height of Child")
plt.show()

plt.subplot(1,2,1)
plt.hist(Galton['parent'],bins='sturges')
plt.xlim(60,75)
plt.title("Height of Parent")
plt.subplot(1,2,2)
plt.hist(Galton['child'],bins='sturges')
plt.xlim(60,75)
plt.title("Height of Child")
plt.show()

Octane = pd.read_csv("Data/Octane.csv",delimiter=',')
plt.subplot(2,2,1)
plt.hist(Octane['Method_1'],color="magenta")
plt.xlabel("Ratings Under Method I")
plt.title("Histogram of Octane Ratings for Method I")
plt.subplot(2,2,2)
plt.hist(Octane['Method_2'],color="cyan")
plt.xlabel("Ratings Under Method II")
plt.title("Histogram of Octane Ratings for Method II")
Samplez = pd.read_csv("Data/Samplez.csv",delimiter=',')
plt.subplot(2,2,3)
plt.hist(Samplez['Sample_1'],color="magenta")
plt.xlabel("Sample 1")
plt.title("Histogram of Sample 1")
plt.subplot(2,2,4)
plt.hist(Samplez['Sample_2'],color="cyan")
plt.xlabel("Sample 2")
plt.title("Histogram of Sample 2")
plt.show()


'''
Matrix of Scatter Plots
'''
Gasoline = pd.read_csv("Data/Gasoline.csv",delimiter=',')
Gasoline2 = Gasoline
del Gasoline2['x11']
from pandas.tools.plotting import scatter_matrix
scatter_matrix(Gasoline2)
plt.show()



