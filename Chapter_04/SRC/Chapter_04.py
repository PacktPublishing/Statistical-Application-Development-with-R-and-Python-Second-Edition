'''
EDA Summaries in Python 
'''
import os
os.chdir("C:/Users/tprabhan/Documents/My_Books/RSADBE_2.0/R_and_Python_Programs/Python/Chapter_04/")
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

TW = pd.read_csv("Data/TheWALL.csv",delimiter=',')
TW['Score'].quantile([0,0.25,0.5,0.75,1])
np.diff(TW['Score'].quantile([0,0.25,0.5,0.75,1]))
TW['Score'].quantile(np.arange(0,1.1,.1))
np.diff(TW['Score'].quantile(np.arange(0,1.1,.1)))
TW['HA_Ind'].value_counts()
TW.boxplot(column='Score',by='HA_Ind')
plt.show()
TW.query('Score<200').boxplot(column='Score',by='HA_Ind')
plt.show()
