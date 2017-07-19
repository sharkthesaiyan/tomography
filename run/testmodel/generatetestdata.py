import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from pylab import meshgrid
import pylab
import random
import sys

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
	
	try:
		n = int(sys.argv[1])
	except:
		n = 100

	random.seed(16167)
	step = 0.00001
	xmin = 0.0
	xmax = 10.0
	ymin = 0.0
	ymax = 10.0
	
	x = np.arange(0,10,00.1)
	y = np.arange(0,10,00.1)
	X,Y = meshgrid(x,y)

	z = modelFunction(X,Y)

	a = integrate(0.0,0.0,10.0,10.0,0.1)
	
	try:
		outfile = sys.argv[2]
	except:
		outfile = "testoutfile%i.dat" %(n)
	
	with open(outfile,"w") as f: 
		f.write(str(n)+"\n")
		for j in range(3):
			for k in range(3):
				for i in range(100):
					startangle = np.pi*1.0/2.0 - j*1.0472

					r = 0.45 - 0.1*k

					angle = startangle + i*np.pi/150.0 + 20*np.pi/120.0
					x1 = 0.5*xmax + r*xmax*np.cos(angle)
					y1 = 0.5*ymax + r*ymax*np.sin(angle)

					angle2 = startangle - i*np.pi/150.0 - 20*np.pi/120.0
					x2 = 0.5*xmax + r*xmax*np.cos(angle2)
					y2 = 0.5*ymax + r*ymax*np.sin(angle2)

					if(x1<=x2):
						integral = 100.0*integrate(x1,y1,x2,y2,step)
						toPrint = "%15.9f \t %15.9f \t %15.9f \t %15.9f \t %15.9f" %(x1,y1,x2,y2,integral)
						f.write(toPrint+"\n")
					else:
						integral = 100.0*integrate(x2,y2,x1,y1,step)
						toPrint = "%15.9f \t %15.9f \t %15.9f \t %15.9f \t %15.9f" %(x2,y2,x1,y1,integral)
						f.write(toPrint+"\n")
	

#	fig = plt.figure()
#	ax = fig.gca(projection='3d')
#	surf = ax.plot_surface(X,Y,z)
#	plt.show()
	pylab.pcolor(X,Y,z)
	pylab.colorbar()
	pylab.savefig("model.png")

main()
