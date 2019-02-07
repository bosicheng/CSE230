from misc import Failure

class Vector(object):
    def __init__(self,args):
        # first we need to judge if the argument is an int or a list
        # then define self.input
        if isinstance(args, list):
            self.input = args    
        else:
            if args<0:
                raise ValueError("Vector length cannot be negative")
            self.input = [0.0] * args 
        
    def __repr__(self):
        # simply return the string
        return "Vector("+repr(self.input)+")"

    def __len__(self):
        # self.input is a list so we can apply len() on it
        return len(self.input)
    
    def __iter__(self):
        # just a iteration implemented by yield
        for value in self.input:
            yield value
    
    def __add__(self,other):
        # we use an iterator to add all the elements in each list and then create a new list
        # then we put the new list in class Vector
        result=[]
        for i in range(len(self.input)):
            temp=list(self)[i]+list(other)[i]
            result.append(temp)
        return Vector(result)

    def __iadd__(self,other):
        # the same as the previous one
        result=[]
        for i in range(len(self.input)):
            temp=list(self)[i]+list(other)[i]
            result.append(temp)
        return Vector(result)

    def dot(self,other):
        # almost the same as add
        sum_of_two=0
        for i in range(len(self.input)):
            temp=list(self)[i]*list(other)[i]
            sum_of_two+=temp
        return sum_of_two

    def __getitem__ (self, i):
        # judge if the i is an int or a list
        # also implement slice, the difference is whether put it into a Vector class
        if isinstance(i,int):
            return self.input[i]
        elif isinstance(i,slice):
            return Vector(self.input[i])

    def __setitem__ (self, i, value): 
        # it changes the value of the self.input, but except that it is the same with getitem
        # but also we need to raise error
        origin_len=len(self.input)
        if isinstance(i,int):
            self.input[i]=value
            if len(self.input)!=origin_len:
                raise ValueError("slice would change the length of the vector!")
            return self.input[i]
        elif isinstance(i,slice):
            self.input[i]=value
            if len(self.input)!=origin_len:
                raise ValueError("slice would change the length of the vector!")
            return Vector(self.input[i])

    def __eq__(self, other):
        # the "other" must be a Vector! I noticed if I miss it there are one or two cases failed
        # compare every element in two lists
        if not isinstance(other, Vector):
            return False
        for i in range(len(other)):
            if self[i]!=other[i]:
                return False
        return True

    # the rest are almost the same
    # just change the order
    def __ne__(self, other):
        if not isinstance(other, Vector):
            return False
        for i in range(len(other)):
            if self[i]==other[i]:
                return False
        return True

    def __gt__(self, other):
        for i in range(len(self)):
            if sorted(self)[::-1][i]>sorted(other)[::-1][i]:
                return True
            elif sorted(self)[::-1][i]<sorted(other)[::-1][i]:
                return False
        return False

    def __ge__(self, other):
        for i in range(len(self)):
            if sorted(self)[::-1][i]>sorted(other)[::-1][i]:
                return True
            elif sorted(self)[::-1][i]<sorted(other)[::-1][i]:
                return False
        return True
    
    def __lt__(self, other):
        for i in range(len(self)):
            if sorted(other)[::-1][i]>sorted(self)[::-1][i]:
                return True
            elif sorted(other)[::-1][i]<sorted(self)[::-1][i]:
                return False
        return False

    def __le__(self, other):
        for i in range(len(self)):
            if sorted(other)[::-1][i]>sorted(self)[::-1][i]:
                return True
            elif sorted(other)[::-1][i]<sorted(self)[::-1][i]:
                return False
        return True