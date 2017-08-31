'''
Logistic Regression Model
'''
import os
os.chdir("MyPath/Python/Chapter_07")
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import statsmodels.formula.api as smfa 
import statsmodels.api as sm
import pylab as pl
import pysal
from sklearn.metrics import roc_curve, auc

# The Probit Model
sat = pd.read_csv("Sat.csv",delimiter=',')
y = np.array(sat[["Pass"]]); X = np.array(sat[["Pass"]])
pass_probit = pysal.spreg.probit.Probit(y,X)
pass_probit.betas
print(pass_probit.summary)

# Fitting a Logistic Regression Model
sat['Intercept'] = 1.0
Pass_logit = sm.Logit(sat['Pass'],sat[['Intercept','Sat']])
PL_fit = Pass_logit.fit()
print(PL_fit.summary())
PL_fit.conf_int()

# Residual Plot
plt.subplot(1,3,1)
plt.plot(PL_fit.fittedvalues,PL_fit.resid_response,color='red')
plt.xlabel("Fitted Values")
plt.ylabel("Residuals")
plt.title("Fitted Vs Residuals Plot")
plt.subplot(1,3,2)
plt.plot(PL_fit.fittedvalues,PL_fit.resid_pearson,color='blue')
plt.xlabel("Fitted Values")
plt.ylabel("Pearson Residuals")
plt.title("Fitted Vs Pearson Residuals Plot")
plt.subplot(1,3,3)
plt.plot(PL_fit.fittedvalues,PL_fit.resid_dev,color='green')
plt.xlabel("Fitted Values")
plt.ylabel("Deviance Residuals")
plt.title("Fitted Vs Deviance Residuals Plot")
plt.show()

# Diagnostics
def hatvalues(fitted_lm):
	X=fitted_lm.exog
	hat_mat = np.dot(X,np.dot(np.linalg.inv(np.dot(X.T,X)),X.T))
	hat_values = np.diagonal(hat_mat)
	return(hat_values)
hatvalues(Pass_logit)

# ROC Curve
# https://stackoverflow.com/questions/28719067/roc-curve-and-cut-off-point-python
sat['pred'] = PL_fit.predict(sat[['Intercept','Sat']])
fpr, tpr, thresholds = roc_curve(sat['Pass'], sat['pred'])
roc_auc = auc(fpr, tpr)
print("Area under the ROC Curve is : %f" % roc_auc)

plt.plot(fpr,tpr,color='red',lw=2,label='ROC Curve')
plt.plot([0,1],[0,1],color='blue',lw=2,linestyle='--')
plt.xlabel('FPR')
plt.ylabel('TPR')
plt.title('ROC Curve')
plt.show()
