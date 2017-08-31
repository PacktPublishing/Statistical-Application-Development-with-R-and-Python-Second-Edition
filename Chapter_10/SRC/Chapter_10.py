'''
CART and Beyond
'''

import os 
# os.chdir("MyPath/PyLab/Chapter09")
os.chdir("C:/Users/tprabhan/Documents/My_Books/RSADBE_2.0/R_and_Python_Programs/Python/Chapter_09/")
import numpy as np
import pandas as pd
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import BaggingClassifier
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import classification_report
import sklearn.metrics
from sklearn import tree
from sklearn.cross_validation import train_test_split
from sklearn.preprocessing import label_binarize
from sklearn.metrics import roc_curve, auc
import matplotlib.pyplot as plt

# Bagging
# The German Credit Classification Tree
GC = pd.read_csv("Data/GC.csv",delimiter=',')
y = GC[['good_bad']]
X = GC.iloc[:,0:20]
y = label_binarize(y, classes=[0, 1])
n_classes = y.shape[1]

# Partition the data
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=.2, random_state=0)
cart = DecisionTreeClassifier(max.depth=7,min_samples_split=10)
num_trees = 100
model = BaggingClassifier(X=X_train,y=y_train,base_estimator=cart, n_estimators=num_trees)

y_train_pred = model.predict(X_train)
y_test_pred = model.predict(X_test)
fpr_train, tpr_train, thresholds = roc_curve(y_train,y_train_pred)
fpr_test, tpr_test, thresholds = roc_curve(y_test,y_test_pred)
roc_auc_train = auc(fpr_train,tpr_train)
roc_auc_test = auc(fpr_test,tpr_test)
print("Area under the ROC Curve for Train Data is : %f" % roc_auc_train)
print("Area under the ROC Curve for Test Data is : %f" % roc_auc_test)

plt.plot(fpr_train,tpr_train,color='red',lw=2,label='ROC Curve')
plt.plot(fpr_test,tpr_test,color='green',lw=2)
plt.plot([0,1],[0,1],color='blue',lw=2,linestyle='--')
plt.show()

# Random Forest
model = RandomForestClassifier(n_estimators=100, max_depth=6, min_samples_split=10)
model_RF = model(X_train,Y_train)

y_train_pred = model_RF.predict(X_train)
y_test_pred = model_RF.predict(X_test)
fpr_train_RF, tpr_train_RF, thresholds_RF = roc_curve(y_train,y_train_pred)
fpr_test_RF, tpr_test_RF, thresholds = roc_curve(y_test,y_test_pred)
roc_auc_train_RF = auc(fpr_train_RF,tpr_train_RF)
roc_auc_test_RF = auc(fpr_test_RF,tpr_test_RF)
print("Area under the ROC Curve for Train Data is : %f" % roc_auc_train_RF)
print("Area under the ROC Curve for Test Data is : %f" % roc_auc_test_RF)

plt.plot(fpr_train_RF,tpr_train_RF,color='red',lw=2,label='ROC Curve')
plt.plot(fpr_test_RF,tpr_test_RF,color='green',lw=2)
plt.plot([0,1],[0,1],color='blue',lw=2,linestyle='--')
plt.show()


