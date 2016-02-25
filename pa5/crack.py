
from misc import *
from time import time
import crypt
import re

def load_words(filename,regexp):
    """Load the words from the file filename that match the regular
       expression regexp.  Returns a list of matching words in the order
       they are in the file."""
    
    f = open(filename, 'r')
    words = []
    for line in f:
      if re.match(regexp,line):
        words.append(line.strip())

    f.close()    
    return words

def transform_reverse(stri):
  return [ stri, stri[::-1] ]

def transform_capitalize(stri):

    result = []

    if stri == "":
      result.append(stri)
    else:
      stri = stri.lower()
      current = stri[:1]
      
      if stri.isalpha():
        for sub_stri in transform_capitalize(stri[1:]):
          result.append(current.upper() + sub_stri)
          result.append(current.lower() + sub_stri)
      else:
        for sub_stri in transform_capitalize(stri[1:]):
          result.append(current + sub_stri)

    return result

def transform_digits(stri):

    dict_pairs = ( (('o','O'), 0), (('i','I','l','L'), 1), (('z','Z'), 2), 
                   (('e','E'), 3), (('g','G','q','Q'), 9), (('a','A'), 4),
                   (('s','S'), 5), (('t','T'), 7), (('b','B'), [6,8])       )

    digits = { key : value for keys, value in dict_pairs for key in keys }

    result = []

    if stri == "":
      result.append(stri)
    else:
      current = stri[:1]
      
      if current.isalpha() and digits.has_key(current):
          for sub_stri in transform_digits(stri[1:]):
            if current == "b" or current == "B":
              result.append(str(digits[current][0]) + sub_stri)
              result.append(str(digits[current][1]) + sub_stri)
              result.append(current + sub_stri)
            else:
              result.append(str(digits[current]) + sub_stri)
              result.append(current + sub_stri)
      else:
        for sub_stri in transform_digits(stri[1:]):
          result.append(current + sub_stri)

    return result


def check_pass(plain,enc):
    """Check to see if the plaintext plain encrypts to the encrypted
       text enc"""
    salt = enc[:2]
    return crypt.crypt(plain,salt) == enc

def load_passwd(filename):
    """Load the password file filename and returns a list of
       dictionaries with fields "account", "password", "UID", "GID",
       "GECOS", "directory", and "shell", each mapping to the
       corresponding field of the file."""
    
    #dict_template = {}.fromkeys(
      #["account","password","UID","GID","GECOS","directory","shell"])
    
    f = open(filename, 'r')
    dictionaries = []
    
    for line in f:

      dict_template = {}.fromkeys(
      ["account","password","UID","GID","GECOS","directory","shell"] )

      line = line.strip()
      #print line
      ct = 0
      values = line.split(':')
      #print values
    
      for key in dict_template:
        if key == 'UID' or key == 'GID':
          dict_template[key] = int(values[ct])
        else:
          dict_template[key] = values[ct]
        ct += 1
    
      #print dict_template
      #print "\n" 
      dictionaries.append(dict_template)
    
    f.close()
    return dictionaries

def load_passwd2(filename):

  f = open(filename, 'r')
  combos = []

  for line in f:
    line = line.strip()
    values = line.split(':')
    combos.append( (values[0],values[1]) )

  f.close()
  return combos

def crack_pass_file(pass_filename,words_filename,out_filename):
    """Crack as many passwords in file fn_pass as possible using words
       in the file words"""
    start = time()
    #password = open(pass_filename, 'r')
    #words    = open(words_filename, 'r')
    out_file = open(out_filename, 'w')
    combos   = load_passwd2(pass_filename)
    words    = load_words(words_filename, '.')
    print('===== %s DONE @ %0.3fs' % ("READ", time() - start))

    #check if plain password
    for item in combos:
      #print "Trying user: ",item[0],"..."
      for word in words:
        #print "Trying word: ",word,"..."
        if check_pass(word,item[1]):
          #print "Written combo: ",item[0],"=",item[1]
          print('===== %s HIT @ %0.3fs' % (item[0], time() - start))
          out_file.write(item[0]+"="+item[1]+"\n")

    print('===== %s DONE @ %0.3fs' % ("PLAIN", time() - start))      
    out_file.close()

