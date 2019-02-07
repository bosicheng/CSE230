def print_some(l):
    def decorator(f):
        def decorated(*args):
            for index in l:
                if 0<=index<len(args):
                    print("Arg "+str(index)+": "+str(args[index]))
                if index==-1:
                    decorated.flag=True
            # if decorated.flag:
                # print("-- "+str(f.__name__)+" called --")
            decorated.level+=1
            rv=f(*args)
            print("Return: "+str(rv))
            decorated.level-=1
            # if decorated.level==0:
            #     print(str(rv))
            return rv
        decorated.level=0
        decorated.flag=False
        return decorated
    return decorator

def derivative(delta):
    class decorated:
        def __init__(self,f):
            self.f=f
        def __call__(self,arg):
            new_arg=arg+delta
            res=(self.f(new_arg)-self.f(arg))/delta
            return round(res,2)
    return decorated