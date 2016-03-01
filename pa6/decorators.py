from misc import Failure

class profiled(object):
    def __init__(self,f):
        self.__count=0
        self.__f=f
        self.__name__=f.__name__
    def __call__(self,*args,**dargs):
        self.__count+=1
        return self.__f(*args,**dargs)
    def count(self):
        return self.__count
    def reset(self):
        self.__count=0



class traced(object):
    """A decorator class to print the stack trace of a recursive function. 
       It prints out an ASCII art tree of the function calls and return values."""
    
    # Global variable counter for calls to multiple functions
    # Start at -1 since we want the first value to be zero for purposes 
    # of printing out the correct number of | characters
    count=-1

    def __init__(self,f):
        """Initialize the decorator class to have the values count, 
            function name and function itself. """
        self.__f=f
        self.__name__=f.__name__
    
    def __call__(self,*args,**dargs):
        """Handle printing the stack trace up and down before and after calling
           the recursive function. Increase and decrease count to handle printing
           pipe ( | ) characters. Calls the recursive function itself. """

        # Make string copy of args and dargs
        s = ", "
        args2  = s.join(map(str,args))
        dargs2 = s.join(map(str,dargs))

        # Increase count at every call to the function
        traced.count+=1

        # If args is not null
        #if args2 != "":
        # Print the "recurse down" stack trace
        print traced.count*"| "+",- "+self.__name__+"("+args2+dargs2+")"
        # Make the function call and catch return value
        retVal = self.__f(*args,**dargs)
        # Print the "recurse up" stack trace
        print traced.count*"| "+"`- "+str(retVal)

        # # If dargs is not null
        # elif dargs2 != "":
        #     # Print the "recurse down" stack trace
        #     print traced.count*"| "+",- "+self.__name__+"("+args2+dargs2+")"
        #     # Make the function call and catch return value
        #     retVal = self.__f(*args,**dargs)
        #     # Print the "recurse up" stack trace
        #     print traced.count*"| "+"`- "+str(retVal)

        # Decrease count!
        traced.count-=1
        # return the return value of the function
        return retVal



class memoized(object):

    def __init__(self,f):
        """A decorator class to instantaneously return previously computed 
           values with the given arguments. """
        # A variable to the class to hold the arguments/return values combos
        # obviously it is a dictionary.
        self.__memoTable = {}
        self.__f         = f
        self.__name__    = f.__name__
    
    def __call__(self,*args,**dargs):
        """Handle checking to see if the function has been called with the given
           arguments. If the function has been called, extract values from the 
           memory table and return them. If it hasn't been called, call the 
           function and save the arguments+return value in the memo table. """

        # To save the key args:values into a list that gets saved into memo table
        #keyArgs = []
        args2  = str(args)
        dargs2 = str(dargs)

        # Check that args is not Null
        if args2 != "":

            #print args
            #print args2

            # Check to see if the function has been called before with these args
            if args2 not in self.__memoTable:
                # If it hasn't call it and store results and args in memoTable
                self.__memoTable[args2] = self.__f(*args,**dargs)    
                return self.__memoTable[args2]
            else:
                return self.__memoTable[args2]

        # Check that dargs is not Null
        elif dargs2 != "":

            if dargs2 not in self.__memoTable:
                # If it hasn't call it and store results and args in memoTable
                self.__memoTable[dargs2] = self.__f(*args,**dargs)
                return self.__memoTable[dargs2]
            else:
                return self.__memoTable[dargs2]

        else:

            raise ValueError("arguments are invalid in some way")


# run some examples.  The output from this is in decorators.out
def run_examples():
    for f,a in [(fib_t,(7,)),
                (fib_mt,(7,)),
                (fib_tm,(7,)),
                (fib_mp,(7,)),
                (fib_mp.count,()),
                (fib_mp,(7,)),
                (fib_mp.count,()),
                (fib_mp.reset,()),
                (fib_mp,(7,)),
                (fib_mp.count,()),
                (even_t,(6,)),
                (quicksort_t,([5,8,100,45,3,89,22,78,121,2,78],)),
                (quicksort_mt,([5,8,100,45,3,89,22,78,121,2,78],)),
                (quicksort_mt,([5,8,100,45,3,89,22,78,121,2,78],)),
                (change_t,([9,7,5],44)),
                (change_mt,([9,7,5],44)),
                (change_mt,([9,7,5],44)),
                ]:
        print "RUNNING %s(%s):" % (f.__name__,", ".join([repr(x) for x in a]))
        rv=f(*a)
        print "RETURNED %s" % repr(rv)

@traced
def fib_t(x):
    if x<=1:
        return 1
    else:
        return fib_t(x-1)+fib_t(x-2)

@traced
@memoized
def fib_mt(x):
    if x<=1:
        return 1
    else:
        return fib_mt(x-1)+fib_mt(x-2)

@memoized
@traced
def fib_tm(x):
    if x<=1:
        return 1
    else:
        return fib_tm(x-1)+fib_tm(x-2)

@profiled
@memoized
def fib_mp(x):
    if x<=1:
        return 1
    else:
        return fib_mp(x-1)+fib_mp(x-2)

@traced
def even_t(x):
    if x==0:
        return True
    else:
        return odd_t(x-1)

@traced
def odd_t(x):
    if x==0:
        return False
    else:
        return even_t(x-1)

@traced
def quicksort_t(l):
    if len(l)<=1:
        return l
    pivot=l[0]
    left=quicksort_t([x for x in l[1:] if x<pivot])
    right=quicksort_t([x for x in l[1:] if x>=pivot])
    return left+l[0:1]+right

@traced
@memoized
def quicksort_mt(l):
    if len(l)<=1:
        return l
    pivot=l[0]
    left=quicksort_mt([x for x in l[1:] if x<pivot])
    right=quicksort_mt([x for x in l[1:] if x>=pivot])
    return left+l[0:1]+right

class ChangeException(Exception):
    pass

@traced
def change_t(l,a):
    if a==0:
        return []
    elif len(l)==0:
        raise ChangeException()
    elif l[0]>a:
        return change_t(l[1:],a)
    else:
        try:
            return [l[0]]+change_t(l,a-l[0])
        except ChangeException:
            return change_t(l[1:],a)

@traced
@memoized
def change_mt(l,a):
    if a==0:
        return []
    elif len(l)==0:
        raise ChangeException()
    elif l[0]>a:
        return change_mt(l[1:],a)
    else:
        try:
            return [l[0]]+change_mt(l,a-l[0])
        except ChangeException:
            return change_mt(l[1:],a)

@traced
def foo(a,b):
    if a==0: return b
    return foo(b=a-1,a=b-1)


