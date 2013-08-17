#!/usr/bin/python

import re
import sys
import os
import subprocess

def main():
	if len(sys.argv) < 3:
		sys.stderr.write('100 Invalid usage, please specify a file and the solver')
		sys.exit(100)
	if not os.path.exists(sys.argv[1]):
		sys.stderr.write('101 File "' + sys.argv[1] + '" could not be found')
		sys.exit(101)
	os.system("./" + sys.argv[2] + " " + sys.argv[1] + " > /tmp/check.tmp")
	os.system("touch /tmp/save.tmp")
	write = open("/tmp/save.tmp", "w")
	with open("/tmp/check.tmp", "r") as read:
		varlist = getvars(read)
	with open("/tmp/save.tmp", "w") as write:
		with open(sys.argv[1], "r") as read:
			replacewith(read, write, varlist)
	with open("/tmp/save.tmp", "r") as read:
		checkfile(read)
	print "000 The solution given by the solver is correct for " + sys.argv[1]
	sys.exit(0)

def getvars(stream):
	solution = False
	varlist = []
	for l in stream:
		if l.startswith("c "):
			continue
		elif l.startswith("s "):
			if solution:
				print "401 Doubled solution line for " + sys.argv[1]
				sys.exit(401)
			if l.startswith("s SATISFIABLE"):
				solution = True
			elif l.startswith("s UNSATISFIABLE"):
				print "301 Returned unsatisfiable for " + sys.argv[1]
				sys.exit(301)
			elif l.startswith("s UNKNOWN"):
				print "302 Returned unknown for " + sys.argv[1]
				sys.exit(302)
			else:
				print "402 Unknown solution line " + s + " for " + sys.argv[1]
				sys.exit(402)
		elif l.startswith("v "):
			varlist = varlist + [int(l[2:])]
	return varlist

def replacewith(instream, outstream, varlist):
	count = 0
	length = 0
	for l in instream:
		if count > 0 and count >= length:
			break
		if l.startswith("c"):
			continue
		elif l.startswith("p cnf"):
			text = l[6:]
			length = int(re.split(r"\s+", text)[1])
		else:
			for v in varlist:
				l = re.sub(r"^" + str(v) + "\s", "+ ", l)
				l = re.sub(r"\s" + str(v) + "\s", " + ", l)
				l = re.sub(r"^" + str(-v) + "\s", "- ", l)
				l = re.sub(r"\s" + str(-v) + "\s", " - ", l)
			l = l.replace("+", "1")
			l = l.replace("-", "0")
			outstream.write(l)
			count += 1

def checkfile(instream):
	idx = 0
	for l in instream:
		idx += 1
		if not "1" in l:
			print "303 Unsatisified clause " + str(idx)
			sys.exit(303)
main()
