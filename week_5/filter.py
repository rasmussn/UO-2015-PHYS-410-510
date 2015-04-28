'''
This program prints the first column when the third column
is greater than 1400.
'''

import sys

if (len(sys.argv) < 2):
   print("usage: filter filename")
   quit()

for line in open(sys.argv[1]):
   words = line.split()
   if int(words[3]) > 1400: print(words[0])



