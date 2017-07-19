import numpy as np
from PIL import Image
import matplotlib.pyplot as plt
import matplotlib.image as mpimg
import matplotlib.pylab as plb

def main():
	data = np.loadtxt("asd.txt")

	plb.matshow(np.abs(data))
        plb.colorbar()
	plb.show()

main()
