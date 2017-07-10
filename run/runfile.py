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

	if(os.path.isfile("../src/"+programName)):
		sp.call("../src/"+programName)
	else:
		errorPrint("program: " + programName + " not found in:"+"../src/.", "Remember to compile the program first.")		


main()

