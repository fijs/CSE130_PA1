#Fernando Jaime, A11643783

from misc import *
#from time import time
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
  """Return the string and it's reverse in a list."""
  return [ stri, stri[::-1] ]

def transform_capitalize(stri):
    """This function takes a string and returns a list with every 
     possible capitalization variation of the word. To build the 
     list, it recursively slices the string char by char until 
     the empty string is reached. Then, we append the empty 
     string to the results list and recurse back, building a tree
     that effectively contains both the uppercase and lowercase 
     variations of each character in the original string. """
    
    result = []

    # base case
    if stri == "":
      result.append(stri)
    else:
      stri = stri.lower()
      current = stri[:1]
      # if the character is a letter, append both the lowercase 
      # and lowercase versions of it to the tree we are building.
      if stri.isalpha():
        for sub_stri in transform_capitalize(stri[1:]):
          result.append(current.upper() + sub_stri)
          result.append(current.lower() + sub_stri)
      # else if the char is not a letter, simply append it on 
      # our way back from the recursion (no variation of it needed)
      else:
        for sub_stri in transform_capitalize(stri[1:]):
          result.append(current + sub_stri)

    return result

def transform_digits(stri):
    """This function works mostly the same as transform_capitalize,
         but instead of building a tree with capitalization variations 
         of the input string, it builds the tree with variations of
         certains characters replaced by digits according to the 
         harcoded mapping. """

    # the harcoded mapping of chars -> digits in tuple form
    dict_pairs = ( (('o','O'), 0), (('i','I','l','L'), 1), (('z','Z'), 2), 
                   (('e','E'), 3), (('g','G','q','Q'), 9), (('a','A'), 4),
                   (('s','S'), 5), (('t','T'), 7), (('b','B'), [6,8])       )

    # a dictionary that maps multiple keys (the characters) to a given digit
    digits = { key : value for keys, value in dict_pairs for key in keys }

    result = []

    # our base case
    if stri == "":
      result.append(stri)
    else:
      current = stri[:1]
      # if the character is a letter and it exists in the digits dict,
      # we need to handle two cases. For every char in the dict except
      # b | B, we simply append (replace) the char for it's digit mapping.
      # If it's b | B, we need to append (replace) twice, once for each
      # possible value of b (6 and 8). In both scenarios we also append
      # a string without the modification, to get all possible combos
      if current.isalpha() and digits.has_key(current):
        for sub_stri in transform_digits(stri[1:]):
          if current == "b" or current == "B":
            result.append(str(digits[current][0]) + sub_stri)
            result.append(str(digits[current][1]) + sub_stri)
            result.append(current + sub_stri)
          # here we handle every letter in the dic that is not b | B
          else:
            result.append(str(digits[current]) + sub_stri)
            result.append(current + sub_stri)
      # if the char is not a letter in the dict mapping, we let it be
      else:
        for sub_stri in transform_digits(stri[1:]):
          result.append(current + sub_stri)

    return result


def check_pass(plain,enc):
    """Check to see if the plaintext plain encrypts to the encrypted
       text enc"""
    return crypt.crypt(plain,enc[:2]) == enc


def load_passwd(filename):
    """Load the password file filename and returns a list of
       dictionaries with fields "account", "password", "UID", "GID",
       "GECOS", "directory", and "shell", each mapping to the
       corresponding field of the file."""
    
    f = open(filename, 'r')
    # our empty list of dictionaries
    dictionaries = []
    # a list with the correct order of keys for the dictionaries we build
    keys = ["account","password","UID","GID","GECOS","directory","shell"]
    
    # for every record in the file
    for line in f:
      # create new dictionary object with corresponding keys
      dict_template = {}.fromkeys(
      ["account","password","UID","GID","GECOS","directory","shell"] )

      line = line.strip()
      ct = 0
      # get the key values by splitting each line using colons
      values = line.split(':')
    
      # map every key in the dictionary template to it's value using ct as index
      for key in dict_template:
        # if the key is either of these two options cast to int
        if key == 'UID' or key == 'GID':
          dict_template[keys[ct]] = int(values[ct])
        else:
          dict_template[keys[ct]] = values[ct]
        ct += 1
    
      dictionaries.append(dict_template)
    
    f.close()
    return dictionaries



def find_password(users,words,FLAG,out_file,start):
  """
     This function serves as a way to organize running different versions of
     the users -> words -> transformations loop needed to crack the passwords.
     From crack_pass_file() we get a list with users/passwords combos, a list
     with all the words in the dictionary file, and a FLAG variable to know 
     which/how many transformation functions to use in a given loop version.
     Here is the behavior corresponding to the value of FLAG:

     FLAG = 0 FOR PLAIN AND REVERSE
     FLAG = 1 FOR DIGITS (ON PLAIN AND REVERSE)
     FLAG = 2 FOR CAPITALIZE (ON PLAIN AND REVERSE)
     FLAG = 3 FOR CAPITALIZE + DIGITS (ON PLAIN AND REVERSE)

     The optional parameter start is for use with the time() operations for 
     testing purposes.

  """

  remaining_users = []

  # start of every version of the loop
  for user in users:

    pass_found = False

    # Heaviest version of the loop, get all them transformations
    if FLAG == 3:
      #for every word
      for word in words:
        #plain and reverse
        for reverse in transform_reverse(word):
          #apply capitalize
          for caps in transform_capitalize(reverse):
            #and then digits
            for variant in transform_capitalize(caps):
              #then check if the transformed string matches the password
              if check_pass(variant,user['password']):
                #print('===== %s HIT @ %0.3fs' % (user['account'], time() - start))
                out_file.write(user['account']+"="+variant+"\n")
                out_file.flush()
                pass_found = True
                break
    
    # Just them capitalizations
    elif FLAG == 2:
      #for every word
      for word in words:
        #plain and reverse
        for reverse in transform_reverse(word):
          #apply capitalize
          for variant in transform_capitalize(reverse):
            #then check if the transformed string matches the password
            if check_pass(variant,user['password']):
              #print('===== %s HIT @ %0.3fs' % (user['account'], time() - start))
              out_file.write(user['account']+"="+variant+"\n")
              out_file.flush()
              pass_found = True
              break

    # Be more chill and get just the digit variants
    elif FLAG == 1:
      #for every word
      for word in words:
        #plain and reverse
        for reverse in transform_reverse(word):
          #apply digits
          for variant in transform_digits(reverse):
            #then check if the transformed string matches the password
            if check_pass(variant,user['password']):
              #print('===== %s HIT @ %0.3fs' % (user['account'], time() - start))
              out_file.write(user['account']+"="+variant+"\n")
              out_file.flush()
              pass_found = True
              break

    # Just the plains and reverse cuz they cheap and easy
    else:
      #for every word
      for word in words:
        #plain and reverse
        for variant in transform_reverse(word):
          #check if the encrypted word matches the password
          if check_pass(variant,user['password']):
            #print('===== %s HIT @ %0.3fs' % (user['account'], time() - start))
            out_file.write(user['account']+"="+variant+"\n")
            out_file.flush()
            pass_found = True
            break
    
    # if we didn't crack the users password append to the list of users
    # to be returned and passed to the next function call
    if not pass_found:
      remaining_users.append(user)
    
  return remaining_users


def crack_pass_file(pass_filename,words_filename,out_filename):
    """Crack as many passwords in file fn_pass as possible using words
       in the file words"""
      
    # for testing purposes = time(), otherwise = 0  
    #start = time()
    start = 0
    # open the output file
    out_file = open(out_filename, 'w')

    # load the users info into memory
    users = load_passwd(pass_filename)
    # load the dictionary file words into memory
    words = load_words(words_filename, r"^.{6,8}$")

    #print('===== %s DONE @ %0.3fs' % ("READ", time() - start))

    # check plain passwords and their reverses
    remaining_users = find_password(users,words,0,out_file,start)
    #print('===== %s DONE @ %0.3fs' % ("PLAIN AND REVERSE", time() - start))

    # check digits passwords
    remaining_users = find_password(remaining_users,words,1,out_file,start)
    #print('===== %s DONE @ %0.3fs' % ("DIGITS", time() - start))

    # check capitalized passwords
    remaining_users = find_password(remaining_users,words,2,out_file,start)
    #print('===== %s DONE @ %0.3fs' % ("CAPITALIZE", time() - start))

    # check capitalize + digits passwords
    remaining_users = find_password(remaining_users,words,3,out_file,start)
    #print('===== %s DONE @ %0.3fs' % ("CAPITALIZE + DIGITS", time() - start))

    #print('===== %s DONE @ %0.3fs' % ("EVERYTHING", time() - start))
  
    # close our output file      
    out_file.close()
