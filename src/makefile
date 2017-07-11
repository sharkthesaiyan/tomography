SHELL:=/bin/bash
flags=-O2 -Wall
compile=f95

tomography: main.f95 parameters.o inputparameterhandler.o
	$(compile) $(flags) -o tomography main.f95 parameters.o inputparameterhandler.o

parameters.o: parameters.f95
	$(compile) $(flags) -c parameters.f95

inputparameterhandler.o: inputparameterhandler.f95
	$(compile) $(flags) -c inputparameterhandler.f95

clean:
	rm -f *.o 
	rm -f *.mod
	if [ -e "tomography" ]; then rm tomography; fi

