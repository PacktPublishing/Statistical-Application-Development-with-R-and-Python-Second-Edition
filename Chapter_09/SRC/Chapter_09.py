'''
Classification and Regression Trees
'''

import os 
# os.chdir("MyPath/PyLab/Chapter09")
os.chdir("C:/Users/tprabhan/Documents/My_Books/RSADBE_2.0/R_and_Python_Programs/Python/Chapter_09/")
import numpy as np
import pandas as pd
from sklearn.tree import DecisionTreeClassifier
from sklearn.metrics import classification_report
import sklearn.metrics
from sklearn import tree
from sklearn.cross_validation import train_test_split
from sklearn.preprocessing import label_binarize
from sklearn.metrics import roc_curve, auc
import matplotlib.pyplot as plt

'''
For an CART implementation from scratch, refer
http://machinelearningmastery.com/implement-decision-tree-algorithm-scratch-python/
'''	

# The CART Dummy Dataset
CD = pd.read_csv("Data/CART_Dummy.csv",delimiter=',')
CD = CD.dropna()
CD_X = CD[['X1','X2']]
CD_Y = CD[['Y']]

# Simple classifier
classifier = DecisionTreeClassifier()
treefit = classifier.fit(CD_X,CD_Y)
tree_pred = treefit.predict(CD_X)
print(treefit)

# Printing the TREE
'''
The function "tree_to_code" is taken from 
https://stackoverflow.com/questions/20224526/how-to-extract-
      the-decision-rules-from-scikit-learn-decision-tree
The author thanks the creator of funtion whose ID is paulkernfeld on 
behalf of everybody who is benefited by it. 
'''
def tree_to_code(tree, feature_names):
    from sklearn.tree import _tree
    tree_ = tree.tree_
    feature_name = [
        feature_names[i] if i != _tree.TREE_UNDEFINED else "undefined!"
        for i in tree_.feature
    ]
    print("def tree({}):".format(", ".join(feature_names)))
    def recurse(node, depth):
        indent = "  " * depth
        if tree_.feature[node] != _tree.TREE_UNDEFINED:
            name = feature_name[node]
            threshold = tree_.threshold[node]
            print("{}if {} <= {}:".format(indent, name, threshold))
            recurse(tree_.children_left[node], depth + 1)
            print("{}else:  # if {} > {}".format(indent, name, threshold))
            recurse(tree_.children_right[node], depth + 1)
        else:
            print("{}return {}".format(indent, tree_.value[node]))
    recurse(0, 1)

tree_to_code(treefit,['X1','X2'])


# VISUALIZING the TREE
tree.export_graphviz(treefit,out_file='tree1.doc')
'''
Download and install graphivz software from 
       http://www.graphviz.org/Download_windows.php
The graphviz-2.38.msi file would be installed as 
GVEdit software
Open 'tree1.doc' in the graphviz software and it would 
 automatically display the tree structure
'''

'''
Alternate way is the following. 
Setup the path following installation of the Graphviz software. 
After running the Python code in the next two cells, go to your 
working directory and open the pdf file for visual display of the
classification tree. 
'''

import os
os.environ["PATH"] += os.pathsep + 'C:/Program Files (x86)/Graphviz2.38/bin/'

# First run the below line in Anaconda prompt or gnome-terminal 
# pip install pydot
dot_data = StringIO()
tree.export_graphviz(treefit,out_file=dot_data)
graph = pydotplus.graph_from_dot_data(dot_data.getvalue())
graph.write_pdf("CART_Illus.pdf")

# The CPUS Dataset - Regression Tree
cpus = pd.read_csv("Data/cpus.csv",delimiter=',')
perf10 = np.log10(cpus[['perf']])
cpusX = cpus[['syct','mmin','mmax','cach','chmin','chmax']]
regressor = tree.DecisionTreeRegressor()
cpus_fit = regressor.fit(cpusX,perf10)
cpus_data = StringIO()
tree.export_graphviz(cpus_fit,out_file=cpus_data,feature_names=list(cpusX))
cpus_graph = pydotplus.graph_from_dot_data(cpus_data.getvalue())
cpus_graph.write_pdf("CPUS_Tree.pdf")

# The Kyphosis Tree
kyphosis = pd.read_csv("Data/kyphosis.csv",delimiter=',')
Ky = kyphosis[['Kyphosis']]
KX = kyphosis[['Age','Number','Start']]
classifier = DecisionTreeClassifier()
ky_fit = classifier.fit(KX,Ky)
ky_data = StringIO()
tree.export_graphviz(ky_fit,out_file=ky_data,feature_names = list(KX))
ky_graph = pydotplus.graph_from_dot_data(ky_data.getvalue())
ky_graph.write_pdf("Kyphosis_Tree.pdf")

# The German Credit Classification Tree
GC = pd.read_csv("Data/GC.csv",delimiter=',')
y = GC[['good_bad']]
X = GC.iloc[:,1:21]
y = label_binarize(y, classes=[0, 1])
n_classes = y.shape[1]

# Partition the data
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=.2, random_state=0)
classifier = DecisionTreeClassifier(max.depth=7,min_samples_split=10)
GC_Fit = classifier.fit(X_train, y_train)
y_train_pred = GC_Fit.predict(X_train)
y_test_pred = GC_Fit.predict(X_test)

GC_data = StringIO()
tree.export_graphviz(GC_Fit,out_file=GF_data,feature_name = list(X))
GC_graph = pydotplus.graph_from_dot_data(GC_data.getvalue())
GC_graph.write_pdf("GC_Tree.pdf")

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
