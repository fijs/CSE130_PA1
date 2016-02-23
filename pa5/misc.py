#PA 4

import re

"Miscellaneous functions to practice Python"

class Failure(Exception):
    """Failure exception"""
    def __init__(self,value):
        self.value=value
    def __str__(self):
        return repr(self.value)

# Problem 1

# data type functions

def closest_to(l,v):
    """Return the element of the list l closest in value to v.  In the case of
       a tie, the first such element is returned.  If l is empty, None is returned."""
    
    minVal = abs(v-l[0])
    closest = l[0]
    
    for element in l:
      if abs(v-element) < minVal:
        minVal = abs(v-element)
        closest = element

    return closest

def make_dict(keys,values):
    """Return a dictionary pairing corresponding keys to values."""
    
    #because you said we are allowed to use the built-in functions
    return dict(zip(keys,values))

# file IO functions
def word_count(fn):
  """Open the file fn and return a dictionary mapping words to the number
       of times they occur in the file.  A word is defined as a sequence of
       alphanumeric characters and _.  All spaces and punctuation are ignored.
       Words are returned in lower case"""
       
  # open file and get file as a string
  f = open(fn, 'r')
  readStr = f.read()
  # use regex to extract word list, use list list comprenhension to build a 
  # list with all the words in lower case
  wordList = [ x.lower() for x in re.split('\W+',readStr) ]
  # scrub empty string from list of words
  wordList = filter(None, wordList)
  # use the list method count() in a dict comprenhension to create the dict 
  # and return it
  return { word : wordList.count(word) for word in wordList }










