# A53271697
# Bosi Cheng

import re
import os

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
    # simple binary search
    # every loop we split the list to half and see the l[mid]
    # eventually the target must be l[mid] or l[start] or l[end]
    # time complexity O(logn)
    l=sorted(l)
    if len(l)==0:
        return
    start,end=0,len(l)-1
    while start+1<end:
        mid=(start+end)//2
        if (l[mid]==v):
            return l[mid]
        elif l[mid]>v:
            end=mid
        else:
            start=mid
    if abs(l[start]-v)<=abs(l[end]-v):
        return l[start]
    else:
        return l[end]


def make_dict(keys,values):
    # a simple hashmap and a iteration fixes it
    dict={}
    for i in range(len(keys)):
        dict[keys[i]]=values[i]
    return dict
   
# file IO functions
def word_count(fn):
    # first we use os to open the file
    # then read all the lines in it 
    # substitute all the chars that are not included in alpha with space
    # count the number of each word and put the keys and values into hashmap
    file = open(fn, 'r')
    word_dict={}
    alpha='qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM_'
    words = "".join([char if char in alpha else ' ' for char in file.read().lower()])
    for word in words.split():
        if word in word_dict.keys():
            word_dict[word] += 1
        else:
            word_dict[word] = 1
    file.close()
    return word_dict










