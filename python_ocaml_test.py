import os
import subprocess
import hashlib

def test():
	a = subprocess.check_output(["/home/andante/Bitcoin/UI/comm", "add", "100", "100"])
	print a

def hashParams(params):
	digest = ""
	for el in params:
		digest+=el
	return hashlib.sha256(digest).hexdigest()
	

def iCommmunicate(name, params):
	params = [name]+params
	print hashParams(params)
	params = params + [hashParams(params)]
	a = subprocess.check_output(["/home/andante/Bitcoin/UI/comm"] +params)
	print a

iCommmunicate("add",["100","100"])

