from misc import Failure

"""
This module implements the Vector class, which inherits a lot of operations from
the similar list data structure. However, we overload several operations, such as 
comparisons.
"""

class Vector(object):
	""" A class that implements a Vector object and supports operations such as 
		length, iter, add, and dot product. The class is initialized with either
		an (int or long) length argument, or with a sequence of elements that 
		become the elements of the Vector. """

	def __init__(self,param):
		""" The class constructor. Initializes the Vector to a given length with
			every element given 0.0 as a default value, or it Initializes the 
			vector with the elements of the sequence given as input. The vector
			length cannot be negative or anything other than a length or sequence. """

		if isinstance(param, int) or isinstance(param, long):
		
			if param < 0:
				raise ValueError("Vector length cannot be negative")
			elif param == 0:
				self.data = []
			else:
				self.data = [ 0.0 for i in range(param) ]
		
		else:

			try:
				typeCheck = iter(param)	
			except TypeError, e:
				raise TypeError("Vector class requires a length parameter or a sequence")
			else:
	 			self.data = [ x for x in param ]


	def __repr__(self):
		""" A simple string representation of the Vector to output results to the
			interpreter. """

		return "Vector(" + str(self.data) + ")"

	def __len__(self):
		""" A method to get the length of the Vector. """
		return len(self.data)

	def __iter__(self):
		""" A method to allow iteration over Vector objects. """
		for x in self.data: yield x

	def __add__(self,other):
		""" A method to add two Vector objects or a Vector with a sequence. They 
			must be of equal length. """
		if len(self) == len(other):
			return self.__class__([ x+y for x,y in zip(self,other) ])
		else:
			raise ValueError("The sequences to sum must be of the same length")

	def __iadd__(self,other):
		""" A method for a Vector object to add a Vector or a sequence to itself. 
			The operands must be of equal length. """
		return self.__add__(other)

	def __radd__(self,other):
		""" The reverse addition method to support left-hand addition of Vectors. """
		return self.__add__(other)

	def dot(self,other):
		""" A method to obtain the cross product of two Vectors or a Vector and
			a sequence. Operands must be of the same length. This operation is 
			undefined for non-numeric elements. """
		# need to check if other is a sequence (ie, Vector([6,8,2]).dot(4) )
		try:
			typeCheck = iter(other)	
		except TypeError, e:
			raise TypeError("The right operand must be a Vector or a sequence")
		else:
			if len(self) == len(other):
				return sum( [ x*y for x,y in zip(self,other)] )
			else:
				# or should they be treated as say, [6,8,2] * [4] = [6,8,2] * [4,0,0]
				raise ValueError("The operands must be of the same length")

	def __getitem__(self,key):
		""" This method implements element lookup by index in the Vector object. """
		if isinstance(key, slice):
			# print "len is: " + str(self.__len__())
			# if key.start or key.stop > self.__len__():
			# 	print "start is: " + str(key.start)
			# 	print "stop is: " + str(key.stop)
			# 	raise IndexError("Index out of range")
			# else:
			return self.data[key]
		else:
			if key > self.__len__():
				raise IndexError("Index out of range")
			elif key < 0:
				return self.data[self.__len__()-key]
			else:
				return self.data[key]

	def __setitem__(self,key,value):
		""" This method implements element setting by index in the Vector object. """
		if isinstance(key, slice):
			test = list(self.data)
			test[key] = value
			if len(test) > self.__len__():
				raise ValueError("This operation modifies the length of the Vector")
			else:
				self.data[key] = value
		else:
			if key > self.__len__():
				raise IndexError("Index out of range")
			elif key < 0:
				self.data[self.__len__()-key] = value
			else:
				self.data[key] = value

	def __eq__(self,other):
		""" This method implements eq comparison between objects of this class 
			and others. """
		if not isinstance(other, self.__class__):
			return False
		else:
			equal = True
			for x,y in zip(sorted(self,reverse=True),sorted(other,reverse=True)):
				if x != y:
					equal = False
					break
			return equal

	def __ne__(self,other):
		""" This method implements ne comparison between objects of this class 
			and others. """
		if not isinstance(other, self.__class__):
			return True
		else:
			n_equal = False
			for x,y in zip(self,other):
				#print "X is: ",x,"and y is: ",y,""
				if x != y:
					n_equal = True
					break
			return n_equal

	def __gt__(self,other):
		""" This method implements gt comparison between objects of this class 
			and others. """
		greater = False
		for x,y in zip(sorted(self,reverse=True),sorted(other,reverse=True)):
			if x > y:
				greater = True
				break
		return greater

	def __ge__(self,other):
		""" This method implements ge comparison between objects of this class 
			and others. """
		if self.__gt__(other):
			return True
		else:
			return self.__eq__(other)

	def __lt__(self,other):
		""" This method implements lt comparison between objects of this class 
			and others. """
		if self.__gt__(other):
			return False
		elif self.__eq__(other):
			return False
		else:
			return True

	def __le__(self,other):
		""" This method implements le comparison between objects of this class 
			and others. """
		if self.__lt__(other):
			return True
		else:
			return self.__eq__(other)








