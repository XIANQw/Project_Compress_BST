import numpy as np
import matplotlib.pyplot as plt  
x1=[100,150,500,750,1000,10000]
y1=[20210,4986,20350,34714,2002074,674402]
x2=[100,150,500,750,1000,10000]
y2=[38617,18943,126339,249258,3420936,26525109]
x3=[100,150,500,750,1000,10000]
y3=[43854,23064,139560,268473,3584447,26754277]
x=[100,150,500,750,1000,10000]
y=range(0, 55000, 5000)
plt.xlim((0, 11000))
plt.ylim((0, 30000000))
l1=plt.plot(x1,y1,'r--',label='ast')
l2=plt.plot(x2,y2,'g--',label='compression_list')
l3=plt.plot(x3,y3,'b--',label='compression_map')
plt.plot(x1,y1,'ro-',x2,y2,'g+-',x3,y3,'b^-')
plt.title('Complexite en espace memoire')
plt.xlabel('nombre de valeur')
plt.ylabel('minor_words')
plt.legend()
plt.show()