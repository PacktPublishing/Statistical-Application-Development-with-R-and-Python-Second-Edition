'''
Regression Models with Regularization
'''
import os
os.chdir("C:/Users/tprabhan/Documents/My_Books/RSADBE_2.0/R_and_Python_Programs/Python/Chapter_08")
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np 
import pylab
import statsmodels.formula.api as smf
import sklearn.linear_model as slm 

# Illustrating Curse of Higher Order Polynomial Regression 
OF = pd.read_csv("Data/OF.csv",delimiter=',')
X,Y = OF.X,OF.Y
m1 = np.polyfit(X,Y,1)
m2 = np.polyfit(X,Y,2)
m3 = np.polyfit(X,Y,3)
m4 = np.polyfit(X,Y,4)
m5 = np.polyfit(X,Y,5)
m6 = np.polyfit(X,Y,6)
m7 = np.polyfit(X,Y,7)
m8 = np.polyfit(X,Y,8)
m9 = np.polyfit(X,Y,9)
np.round(m1,4)
np.round(m2,4)
np.round(m3,4)
np.round(m4,4)
np.round(m5,4)
np.round(m6,4)
np.round(m7,4)
np.round(m8,4)
np.round(m9,4)

# Piece-wise Linear Regression
PW_Illus = pd_read_csv("Data/PW_Illus.csv",delimiter=',')
X,Y = PW_Illus.X,PW_Illus.Y
PW_Illus['XL14'] = X*(X<14)
PW_Illus['XGT14LT30'] = X*(X>=14)*(X<30)
PW_Illus['XGT30'] = X*(X>=30)
PW_Final = smf.ols(formula='Y~XL14+XGT14LT30+XGT30',data=PW_Illus).fit()
print(PW_Final.params)

# Regularization linear model
from sklearn.preprocessing import PolynomialFeatures
from sklearn.linear_model import Ridge
poly3 = PolynomialFeatures(degree=3)
X3 = poly3.fit_transform(X)
ridge = Ridge(fit_intercept=True, alpha=0.5)
ridge.fit(X3,Y)



# Logistic Regression and Regularization
from sklearn.linear_model import LogisticRegression    
sat = pd.read_csv("Data/Sat.csv",delimiter=',')
y = np.array(sat[["Pass"]]); X = np.array(sat[["Sat"]])
model = LogisticRegression()
f1 = model.fit(X,y)
f1.intercept_, f1.coef_
model2 = LogisticRegression(penalty='l2',C=1e10)
f2 = model2.fit(X,y)
f2.intercept_, f2.coef_


