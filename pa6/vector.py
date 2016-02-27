from misc import Failure

class Vector(object):

	def __init__(self,param):

		if type(param) is int or type(param) is long:
		
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

		return "Vector(" + str(self.data) + ")"

	def __len__(self):
		return len(self.data)

	def __iter__(self):
		for x in self.data: yield x

	def __add__(self,other):
		return Vector([ x+y for x,y in zip(self,other) ])

	def __iadd__(self,other):
		return Vector([ x+y for x,y in zip(self,other) ])

	def __radd__(self,other):
		return Vector([ x+y for x,y in zip(self,other) ])