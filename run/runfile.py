#!/usr/bin/python3
import json
import subprocess as sp
import os
import sys

def errorPrint(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)

configFile = "runparameters.json"
programName = "tomography"

def main():
	with open(configFile,"r") as f:
		config = json.load(f) 

	print(str(os.path.abspath(config["inputDataFile"]))+", "+ str(os.path.abspath(config["outputFile"])))

	if(os.path.isfile("../src/"+programName)):
		sp.call(["mpirun","-n",str(config["ncores"]),"../src/"+programName,str(config["rows"]),str(config["columns"]),str(config["xmax"]),str(config["ymax"]),os.path.abspath(config["inputDataFile"]),os.path.abspath(config["outputFile"]), str(config["points"]),str(config["rounds"])])
	else:
		errorPrint("program: " + programName + " not found in:"+"../src/.", "Remember to compile the program first.")		


main()

