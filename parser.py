# -*- coding: utf-8 -*-

import unicodedata

def correct(s):
    if '-' in s:
        return False
    if ' ' in s:
        return False
    if '\'' in s:
        return False
    if ')' in s:
        return False
    if '.' in s:
        return False
    if '!' in s:
        return False
    if len(s) > 17:
       return False
    return True


def parseFile():
   file = open('dictionnaire_min.txt', 'r')
   content = file.readlines()
   file.close()
   lst = sorted(set([elem[:-1] for elem in content if correct(elem)]))

   file = open('dico.txt', 'w', encoding = 'utf-8')
   for i in range(len(lst)):
        if i == len(lst) - 1:
            file.write(lst[i])
        else:
            file.write(lst[i])
            file.write('\n')
   file.close()

parseFile()
   
