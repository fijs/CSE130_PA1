
from misc import *
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
    raise Failure("to be written")

def load_passwd(filename):
    """Load the password file filename and returns a list of
       dictionaries with fields "account", "password", "UID", "GID",
       "GECOS", "directory", and "shell", each mapping to the
       corresponding field of the file."""
    raise Failure("to be written")

def crack_pass_file(pass_filename,words_filename,out_filename):
    """Crack as many passwords in file fn_pass as possible using words
       in the file words"""
    raise Failure("to be written")

