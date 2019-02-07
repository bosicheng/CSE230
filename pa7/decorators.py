#!/usr/bin/env python3
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
    def __init__(self,f):
        self.f = f
        self.__name__= f.__name__
        self.level=0

    def __call__(self,*args,**kwargs):
        arg_buffer,kwarg_buffer = [],[]
        # check whether it's the first loop (4,5)is *args (b=3, a=4)is **kwargs
        for value in args: 
            arg_buffer.append(str(value))
        for key in kwargs: 
            kwarg_buffer.append(str(key) + '=' + str(kwargs[key]))
        arg_str = ", ".join(arg_buffer)
        kwarg_str = ", ".join(kwarg_buffer)
        print (("| "*self.level) + ",- " + self.__name__ + "(" + arg_str+kwarg_str + ")")
        self.level += 1 
        try:
            rv = self.f(*args,**kwargs)
            self.level -= 1
            print (("| "*self.level) + "`- " + str(rv))
            return rv 
        # if an exception then the recursion simply stops and throws the exception  
        except Exception:
            self.level -= 1
            raise Exception("adjust to the appropriate level!")

    def count(self):
        return self.level

    def reset(self):
        self.level = 0


class memoized(object):
    def __init__(self,f):
        self.__name__= f.__name__
        self.f = f
        self.dict = {}
    
    # If the function last threw an exception when called with the given arguments, 
    # the same exception should be thrown again. 
    # If the function has not been called with the given arguments, 
    # then call it and record the return value or exception. 
    # Then return the return value or raise the thrown exception.

    # so use a dictionary to record everything happened to the key!
    def __call__(self,*args,**kwargs):
        key = str(args) + str(kwargs)
        # if the key has been inputed before just output its record
        if key in self.dict.keys():
            if isinstance(self.dict[key], Exception): 
                raise self.dict[key]
            else: 
                return self.dict[key]
        # if the key hasn't been inputed before then record the output
        else:
            try:
                rv = self.f(*args,**kwargs)
                self.dict[key] = rv
                return rv
            except Exception as e:
                self.dict[key] = e

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
        print("RUNNING %s(%s):" % (f.__name__,", ".join([repr(x) for x in a])))
        rv=f(*a)
        print("RETURNED %s" % repr(rv))

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


