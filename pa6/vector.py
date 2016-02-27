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

	def _len_(self):
		return len(self.data)
