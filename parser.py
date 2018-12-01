# -*- coding: utf-8 -*-


import unicodedata

def transform(s):
   new = ""
   for l in s:
      if not (l == ')' or l == '.' or l == '!'):
         if l == 'é' or l == 'è' or l == 'ë' or l == 'ê':
            new = new + 'e'
         elif l == 'î' or l == 'ï':
            new = new + 'i'
         elif l == 'â' or l == 'à' or l == 'ä':
            new = new + 'a'
         elif l == 'û' or l == 'ü' or l == 'ù':
            new = new + 'u'
         elif l == 'ô':
            new = new + 'o'
         elif l == 'ç':
            new = new + 'c'
         elif l == '\x9c':
            new = new + 'o'
            new = new + 'e'
         else:
            new = new + l
   return new

def singleWord(s):
    if '-' in s:
        return False
    if ' ' in s:
        return False
    if '\'' in s:
        return False
    return True


def parseFile():
   file = open('dictionnaire.txt', encoding = 'ISO-8859-1')
   content = file.readlines()
   file.close()
   lst = sorted(set([transform(elem[:-1].lower()) for elem in content if singleWord(elem)]))

   file = open('dico.txt', 'w', encoding = 'utf-8')
   for line in lst:
        #print(line)
        file.write(line)
        file.write('\n')
   file.close()

parseFile()
   
