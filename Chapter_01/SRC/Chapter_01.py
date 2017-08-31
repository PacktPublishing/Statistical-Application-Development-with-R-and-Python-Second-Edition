'''
Data Characteristics 
'''

import os
os.chdir("C:/Users/tprabhan/Documents/My_Books/RSADBE_2.0/R_and_Python_Programs/Python/Chapter_01")
import matplotlib.pyplot as plt
import numpy as np 
import pylab

# Discrete Uniform Distribution
M = 10
mylabels = list(range(1,M+1))
prob_labels = [1/M]*M
plt.xlim([0.9,10.1])
plt.ylim([0,0.11])
plt.plot(mylabels,prob_labels,marker='o',linestyle='none')
plt.xlabel("Labels")
plt.ylabel("Probability")
plt.title("Probability of Discrete Uniform RV")
plt.show()

# Binomial Distribution
n = 10; p = 0.5
from scipy.stats import binom
p_x = binom.pmf(range(1,n+1),n,p)
plt.plot(range(1,n+1),p_x,marker='o',linestyle='none')
plt.show()

# Binomial Probabilities
n = 83; p = 0.01
from scipy.stats import binom
binom.pmf(10,n,p)
binom.pmf(20,n,p)
binom.pmf(30,n,p)
xr = range(0,84)
sum(binom.pmf(xr,n,p))

# Hypergeometric Distribution
N = 200; M = 20
n = 10
k = range(0,12)
from scipy.stats import hypergeom
np.round(hypergeom.pmf(k,N,M,n),3)

# Poisson Probabilities
from scipy.stats import poisson
poisson.pmf(0,3)
poisson.pmf(5,3)
poisson.pmf(20,3)

# Continuous Uniform Distribution
from scipy.stats import uniform
uniform.cdf(0.58)-uniform.cdf(0.35)

# Exponential Densities
from scipy.stats import expon
xr = np.arange(0,10.2,0.20)
f_x = expon.pdf(xr,scale=1)
pylab.plot(xr,f_x,'r',label='Rate=1')
f_x1 = expon.pdf(xr,scale=1/0.2)
pylab.plot(xr,f_x1,'g',label='Rate=0.2')
f_x2 = expon.pdf(xr,scale=1/0.5)
pylab.plot(xr,f_x2,'b',label='Rate=0.5')
f_x3 = expon.pdf(xr,scale=1/0.7)
pylab.plot(xr,f_x3,'y',label='Rate=0.7')
f_x4 = expon.pdf(xr,scale=1/0.85)
pylab.plot(xr,f_x4,'purple',label='Rate=0.85')
pylab.legend(loc='upper right')
plt.show()

# Exponential Densities Continued
from scipy.stats import expon
xr = np.arange(0,0.5,0.02)
f_x = expon.pdf(xr,scale=1/50)
pylab.plot(xr,f_x,'r',label='Rate=50')
f_x1 = expon.pdf(xr,scale=1/10)
pylab.plot(xr,f_x1,'g',label='Rate=10')
f_x2 = expon.pdf(xr,scale=1/20)
pylab.plot(xr,f_x2,'b',label='Rate=20')
f_x3 = expon.pdf(xr,scale=1/30)
pylab.plot(xr,f_x3,'y',label='Rate=30')
f_x4 = expon.pdf(xr,scale=1/40)
pylab.plot(xr,f_x4,'purple',label='Rate=40')
pylab.legend(loc='upper right')
pylab.show()

# Shady Normal Probabilities
from scipy.stats import norm
xr = np.arange(-4,4.1,0.1)
fx = norm.pdf(xr)
pylab.plot(xr,fx)
p1 = np.arange(0,4.02,0.02)
pylab.fill_between(p1,norm.pdf(p1))
pylab.show()
xr = np.arange(-4,4.1,0.1)
fx = norm.pdf(xr)
pylab.plot(xr,fx)
p2 = np.arange(-1.96,1.961,0.001)
pylab.fill_between(p2,norm.pdf(p2))
pylab.show()
xr = np.arange(-4,4.1,0.1)
fx = norm.pdf(xr)
pylab.plot(xr,fx)
p3 = np.arange(-2.58,2.581,0.001)
pylab.fill_between(p3,norm.pdf(p3))
pylab.show()
