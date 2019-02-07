def test():
    arr=[1,2,3,4,5,6,7,8,9,10]
    return [x if x>5 else x+1 for x in arr ]

print(test())
