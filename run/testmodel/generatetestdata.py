import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from pylab import meshgrid
import pylab
import random

def modelFunction(x,y,scale=1.0):
	#Two peaks
	value = 3.0/((x+5.0)**2 + 0.5*y**2 + 2) + 2.0/((x-5.0)**2 + (y-5.0)**2 + 3)
	return value*scale

def integrate(xmin,ymin,xmax,ymax,step):
	sum = 0.0
	
	if(abs(xmax-xmin)>=0.000001):
		k = (ymax-ymin)/(xmax-xmin)
		stepy = step*np.cos(np.arctan(k))
		stepx = step*np.sin(np.arctan(k))
#		print("sanity check, should be 1.0: "+str(step/np.sqrt(stepx**2 + stepy**2))) 
	else:
		stepx = 0
		stepy = step

	x = xmin
	y = ymin
	while(True):
		sum += step*modelFunction(x,y)
		x += stepx
		y += stepy
		if( x>= xmax or y >= ymax ):
			break
	return sum

def main():
	random.seed(16167)
	step = 0.0001
	xmin = -10.0
	xmax = 10.0
	ymin = -10.0
	ymax = 10.0
	
	x = np.arange(-10,10,00.1)
	y = np.arange(-10,10,00.1)
	X,Y = meshgrid(x,y)

	z = modelFunction(X,Y)

	a = integrate(0.0,0.0,10.0,10.0,0.1)
	
	for i in range(100):
		x1 = random.uniform(xmin,xmax)	
		y1 = random.uniform(ymin,ymax)
		x2 = random.uniform(xmin,xmax)
		y2 = random.uniform(ymin,ymax)
		if(x1<=x2):
			integral = integrate(x1,y1,x2,y2,step)
			print(str(x1)+"\t"+str(y1)+"\t"+str(x2)+"\t"+str(y2)+"\t"+str(integral))
		else:
			integral = integrate(x2,y2,x1,y1,step)
			print(str(x2)+"\t"+str(y2)+"\t"+str(x1)+"\t"+str(y1)+"\t"+str(integral))

	fig = plt.figure()
	ax = fig.gca(projection='3d')
	surf = ax.plot_surface(X,Y,z)
	plt.show()
#	pylab.pcolor(X,Y,z)
#	pylab.colorbar()
#	pylab.show()

main()
