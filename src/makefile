SHELL:=/bin/bash
flags=-O2
compile=f95

tomography: main.f95 parameters.o inputparameterhandler.o filehandler.o
	$(compile) $(flags) -o tomography main.f95 parameters.o inputparameterhandler.o filehandler.o

parameters.o: parameters.f95
	$(compile) $(flags) -c parameters.f95

inputparameterhandler.o: inputparameterhandler.f95
	$(compile) $(flags) -c inputparameterhandler.f95

filehandler.o: filehandler.f95
	$(compile) $(flags) -c filehandler.f95

clean:
	rm -f *.o 
	rm -f *.mod
	if [ -e "tomography" ]; then rm tomography; fi

