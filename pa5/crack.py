
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

def transform_reverse2(stri):
  return stri[::-1]

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
    
    f = open(filename, 'r')
    dictionaries = []
    
    for line in f:

      dict_template = {}.fromkeys(
      ["account","password","UID","GID","GECOS","directory","shell"] )

      line = line.strip()
      ct = 0
      values = line.split(':')
    
      for key in dict_template:
        if key == 'UID' or key == 'GID':
          dict_template[key] = int(values[ct])
        else:
          dict_template[key] = values[ct]
        ct += 1
    
      dictionaries.append(dict_template)
    
    f.close()
    return dictionaries

def load_passwd2(filename):
  """Like load_passwd but returns a simple list with user and password"""

  f = open(filename, 'r')
  combos = []

  for line in f:
    line = line.strip()
    values = line.split(':')
    combos.append( (values[0],values[1]) )

  f.close()
  return combos

def find_password(users,words,transformation,flag,out_file,start):

  remaining_users = []

  for user in users:
    
    pass_found = False

    if flag:
      for word in words:
        for variant in transformation(word):
          if check_pass(variant,user[1]):
            print('===== %s HIT @ %0.3fs' % (user[0], time() - start))
            out_file.write(user[0]+"="+variant+"\n")
            out_file.flush()
            pass_found = True
            break
    else:
      for word in words:
        if check_pass(word,user[1]):
          print('===== %s HIT @ %0.3fs' % (user[0], time() - start))
          out_file.write(user[0]+"="+word+"\n")
          out_file.flush()
          pass_found = True
          break
    
    if not pass_found:
      remaining_users.append(user)
    
  return remaining_users

def crack_pass_file(pass_filename,words_filename,out_filename):
    """Crack as many passwords in file fn_pass as possible using words
       in the file words"""
    start = time()
    out_file = open(out_filename, 'w')
    combos   = load_passwd2(pass_filename)
    words    = load_words(words_filename, r"^.{6,8}")

    print('===== %s DONE @ %0.3fs' % ("READ", time() - start))

    #check plain passwords
    remaining_users = find_password(combos,words,"Null",False,out_file,start)
    print('===== %s DONE @ %0.3fs' % ("PLAIN", time() - start))

    #cache reversed strings
    words_r = map(transform_reverse2,words)

    #check reversed passwords
    remaining_users = find_password(remaining_users,words_r,"Null",False,out_file,start)
    print('===== %s DONE @ %0.3fs' % ("REVERSE", time() - start))

    #cache didn't work... too ham!
    #words_c = [ wordc for elem in map(transform_capitalize,words) for wordc in elem ]
    #words_d = map(transform_digits,words)

    #check digits passwords
    remaining_users = find_password(remaining_users,words,transform_digits,True,out_file,start)
    print('===== %s DONE @ %0.3fs' % ("DIGITS", time() - start))

    print('===== %s DONE @ %0.3fs' % ("EVERYTHING", time() - start))      
    out_file.close()

    #return words_r

