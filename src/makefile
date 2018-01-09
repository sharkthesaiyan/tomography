SHELL:=/bin/bash
flags=-O2 -fcheck=bounds
compile=gfortran

tomography: parameters.o mt19937-64.o inputparameterhandler.o filehandler.o systemmatrixhandler.o customsolver.o main.o
	$(compile) $(flags) -o tomography parameters.o mt19937-64.o inputparameterhandler.o filehandler.o systemmatrixhandler.o customsolver.o main.o

parameters.o: parameters.f95
	$(compile) $(flags) -c parameters.f95

mt19937-64.o: mt19937-64.f95
	$(compile) $(flags) -c mt19937-64.f95

inputparameterhandler.o: inputparameterhandler.f95
	$(compile) $(flags) -c inputparameterhandler.f95

filehandler.o: filehandler.f95
	$(compile) $(flags) -c filehandler.f95

systemmatrixhandler.o: systemmatrixhandler.f95
	$(compile) $(flags) -c systemmatrixhandler.f95

customsolver.o: customsolver.f95
	$(compile) $(flags) -c customsolver.f95

main.o: main.f95
	$(compile) $(flags) -c main.f95

clean:
	rm -f *.o 
	rm -f *.mod
	if [ -e "tomography" ]; then rm tomography; fi
