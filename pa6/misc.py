"""
This module helps define the class Failure, which we need to be able to 
use Failure exceptions.
"""

class Failure(Exception):
    """Failure exception"""
    def __init__(self,value):
        self.value=value
    def __str__(self):
        return repr(self.value)
