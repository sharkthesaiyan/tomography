SHELL:=/bin/bash
flags=-O2 -Wall
compile=f95

tomography: main.f95
	$(compile) $(flags) main.f95 -o tomography

clean:
	rm -f *.o 
	rm -f *.mod
	if [ -e "tomography" ]; then rm tomography; fi

