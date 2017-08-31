import os 
import numpy as np
import pandas as pd
import pymysql as pysql
import pymysql.cursors 

os.chdir('/home/pranathi/Documents/RPySADBE/Python/Chapter_02')

# Simple Operations
x = 8
y = 15
a = 10
b = -4
a,b=10,-4 # equivalently
x + y
a*x + b*y
(a+b)*x == a*x + b*x
x*y
round(x/y,4)
x**2 

# Python Vectors
x = np.array([1,4,3])
y = np.array([2,9,14])
x+y
x-y
x*y
x/y
x-[1,2,3,4]
np.dot(x,y)
np.sqrt((x*x).sum())
# Equivalently
np.sqrt(sum(x*x))

# Python Matrix
X = np.array([[10,20,30],[5,16,28],[8,2,19]])
Y = np.array([4,5,9])
X+45
np.transpose(X)
np.cross(X,Y)
np.cross(np.linalg.inv(np.cross(np.transpose(X),X)),np.cross(np.transpose(X),Y))


# Doing it in Python
SCV = pd.read_csv("SCV.csv")
SCV.shape
print(SCV)
SCV_M = pd.read_csv("SCV_Modified.csv")
SCV_U = pd.read_csv("SCV_Usual.csv")
comb_scv = [SCV_M,SCV_U]
SCV_Combined = pd.concat(comb_scv)
SCV_Combined

# Connecting with MySQL
connection = pysql.connect(host="localhost",user='root',password='statmath',
                          db='test',cursorclass=pymysql.cursors.DictCursor)
cur = connection.cursor()
cur.execute("SELECT * FROM IO_Time")
print(cur.description)
for row in cur:
    print(row)
cur.close()
connection.close()
