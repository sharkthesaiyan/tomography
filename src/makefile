SHELL:=/bin/bash
flags=-g
compile=gfortran

tomography: parameters.o inputparameterhandler.o filehandler.o systemmatrixhandler.o nnlsmodule.o main.o
	$(compile) $(flags) -L/usr/lib/ -o tomography parameters.o inputparameterhandler.o filehandler.o systemmatrixhandler.o nnlsmodule.o main.o -lblas -llapack

parameters.o: parameters.f95
	$(compile) $(flags) -c parameters.f95

inputparameterhandler.o: inputparameterhandler.f95
	$(compile) $(flags) -c inputparameterhandler.f95

filehandler.o: filehandler.f95
	$(compile) $(flags) -c filehandler.f95

systemmatrixhandler.o: systemmatrixhandler.f95
	$(compile) $(flags) -c systemmatrixhandler.f95

nnlsmodule.o: nnlsmodule.f95
	$(compile) $(flags) -c nnlsmodule.f95

main.o: main.f95
	$(compile) $(flags) -c main.f95

clean:
	rm -f *.o 
	rm -f *.mod
	if [ -e "tomography" ]; then rm tomography; fi

