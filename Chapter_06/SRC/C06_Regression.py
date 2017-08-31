'''
Regression Concepts and Code in Python
'''

import os
os.chdir("C:/Users/tprabhan/Documents/My_Books/RSADBE_2.0/R_and_Python_Programs/Python/Chapter_06")
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import statsmodels.api as sm
import statsmodels.formula.api as smf
from statsmodels.formula.api import ols
from statsmodels.regression.linear_model import OLS 
import scipy.stats as st

IOT = pd.read_csv("Data/IO_Time.csv",delimiter=',')
IO = IOT.No_of_IO
CPUT = IOT.CPU_Time
plt.plot(IO,CPUT)
plt.xlabel("No of IO"); plt.ylabel("CPU Time")
plt.show()

y = CPUT
X = sm.add_constant(IO)
CPU_lm = sm.OLS(y,X).fit()
predict_CPU = CPU_lm.predict(X)
CPU_lm.summary()

'''
Without Intercept Model
###### Do Not RUN #######
CPU_lm = sm.OLS(CPUT,IO).fit()
predict_CPU = CPU_lm.predict(IO)
CPU_lm.summary()
'''

# Getting ANOVA and Confidence intervals
CPU_lm2 = ols('CPU_Time~No_of_IO',data=IOT).fit()
print(sm.stats.anova_lm(CPU_lm2,typ=2))
print(CPU_lm.conf_int(0.05))



'''
Model Validataion and Residuals
'''
CPU_Resid = CPU_lm.resid
plt.subplot(3,2,1)
plt.plot(IO,CPU_Resid)
plt.xlabel("Predictor Variable")
plt.ylabel("Residuals")
plt.title("Plot of Residuals Vs Predictor Variable")
plt.subplot(3,2,2)
plt.plot(IO,abs(CPU_Resid))
plt.xlabel("Predictor Variable")
plt.ylabel("Absolute Residuals")
plt.title("Plot of Absolute Residual Values Vs Predictor Variable")
plt.subplot(3,2,3)
plt.plot(IO,CPU_Resid**2)
plt.xlabel("Predictor Variable")
plt.ylabel("Squared Residuals")
plt.title("Plot of Squared Residual Values Vs Predictor Variable")
plt.subplot(3,2,4)
plt.plot(predict_CPU,CPU_Resid)
plt.xlabel("Fitted Values")
plt.ylabel("Residuals")
plt.title("Plot of Residuals Vs Predicted Values")
plt.subplot(3,2,5)
plt.plot(np.arange(1,len(CPU_Resid)+1),CPU_Resid)
plt.xlabel("Sequence Number")
plt.ylabel("Residuals")
plt.title("Sequence Plot of the Residuals")
plt.subplot(3,2,6)
plt.boxplot(CPU_Resid)
plt.title("Box Plot of the Residuals")
plt.show()


'''
Multiple Linear Regression Model
'''
Gasoline = pd.read_csv("Gasoline.csv",delimiter=',')
Mileage_lm = smf.ols(formula='y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11',data=Gasoline).fit()
Mileage_lm.params
Mileage_lm.summary()
print(sm.stats.anova_lm(Mileage_lm,typ=2))
print(Mileage_lm.conf_int(0.05))

plt.subplot(2,1,1)
plt.plot(Mileage_lm.fittedvalues,Mileage_lm.resid,'.')
plt.xlabel("Fitted Values")
plt.ylabel("Residuals")
plt.title("Plot of Residuals Vs Fitted Values")
plt.subplot(2,1,2)
plt.plot(Mileage_lm.fittedvalues,Mileage_lm.resid_pearson,'.')
plt.xlabel("Fitted Values")
plt.ylabel("Pearson Residuals")
plt.title("Plot of Pearson Residuals Vs Fitted Values")
plt.show()

# Alternate way of fitting linear regression model
X = Gasoline[['x1', 'x2', 'x3', 'x4', 'x5', 'x6', 'x7', 'x8', 'x9', 'x10']]
y = Gasoline[['y']]
X = sm.add_constant(X)
X = X.drop(X.index[[18]])
y = y.drop(y.index[[18]])
Gasoline_lm = sm.OLS(y,X)

# http://www.statsmodels.org/dev/_modules/statsmodels/stats/outliers_influence.html


'''
Hatvalues Function
'''

def hatvalues(fitted_lm):
	X=fitted_lm.exog
	hat_mat = np.dot(X,np.dot(np.linalg.inv(np.dot(X.T,X)),X.T))
	hat_values = np.diagonal(hat_mat)
	return(hat_values)
hatvalues(Gasoline_lm)

'''
Cook's distance
'''
Gasoline_lm_ols = smf.ols(formula='y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11',data=Gasoline)
Gasoline_fitted = Gasoline_lm_ols.fit()
CD = Gasoline_fitted.get_influence()
(cook,p) = CD.cooks_distance
cook
(dffits,p) = CD.dffits
dffits
CD.dfbetas

'''
Variance Inflatino Factor
'''
X2 = X
del X2['const']
del X2['x11']
from statsmodels.stats.outliers_influence import variance_inflation_factor
vif = pd.DataFrame()
vif["VIF Factor"] = [variance_inflation_factor(X2.values, i) for i in range(X2.shape[1])]
vif["features"] = X2.columns
vif.round(1)


'''
Subset Selection
http://www.science.smith.edu/~jcrouser/SDS293/labs/lab8/Lab%208%20-%20Subset%20Selection%20in%20Python.pdf
'''
import os
os.chdir("C:/Users/tprabhan/Documents/My_Books/RSADBE_2.0/R_and_Python_Programs")
import pandas as pd
%matplotlib inline
import numpy as np
import itertools
import time
import statsmodels.api as sm
import matplotlib.pyplot as plt

df = pd.read_csv('Hitters.csv',delimiter=',')
df.head()

print(df["Salary"].isnull().sum())
print(df.shape)
df = df.dropna().drop('Player', axis=1)
print(df.shape)
print(df["Salary"].isnull().sum())
dummies = pd.get_dummies(df[['League', 'Division', 'NewLeague']])
y = df.Salary
X_ = df.drop(['Salary', 'League', 'Division', 'NewLeague'], axis=1).astype('float64')
X = pd.concat([X_, dummies[['League_N', 'Division_W', 'NewLeague_N']]], axis=1)
def processSubset(feature_set):
    # Fit model on feature_set and calculate RSS
    model = sm.OLS(y,X[list(feature_set)])
    regr = model.fit()
    RSS = ((regr.predict(X[list(feature_set)]) - y) ** 2).sum()
    return {"model":regr, "RSS":RSS}
def getBest(k):
    tic = time.time()
    results = []
    for combo in itertools.combinations(X.columns, k):
        results.append(processSubset(combo))
    # Wrap everything up in a nice dataframe
    models = pd.DataFrame(results)
    # Choose the model with the highest RSS
    best_model = models.loc[models['RSS'].argmin()]
    toc = time.time()
    print("Processed ", models.shape[0], "models on", k, "predictors in", (toc-tic), "seconds.")
    # Return the best model, along with some other useful information about the model
    return best_model

models = pd.DataFrame(columns=["RSS", "model"])
tic = time.time()
for i in range(1,8):
    models.loc[i] = getBest(i)
toc = time.time()
print("Total elapsed time:", (toc-tic), "seconds.")


