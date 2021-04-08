import string as s
import functools as ft
import re

with open("Data\Day07.txt") as file:
    input = [line.strip() for line in file]

def parse(line):
    ''' Parses on input line and returns the result as tuple 
        Example:        
        Input:  posh blue bags contain 5 plaid chartreuse bags, 3 plaid lime bags.
        Result: ('posh blue', [(5, 'plaid chartreuse'), (3, 'plaid lime')])
    '''
    separators = [" bags contain ", "no other bags.", " bags, ", " bag, ", " bags.", " bag."]
    for sep in separators:
        line = line.replace(sep, ";")
    
    splt = [x for x in re.split(";", line) if x != '']
   
    bagName = splt[0]
    containedBags = []
    for bag in splt[1:]:
        cntName = bag.split(' ', 1)
        cnt = int(cntName[0])
        name = cntName[1]
        containedBags.append((cnt, name))
    return (bagName, containedBags)

data = list(map(parse, input))

def solve1():
    bags = {'shiny gold'}

    while True:
        bagCount = len(bags)
        for (bag, contained_bags) in data:        
            for (_, contained_bag) in contained_bags:
                if (contained_bag in bags):
                    bags.add(bag)    
        if bagCount == len(bags):
            break

    return len(bags) - 1

def solve2():
    stack = [(1, 'shiny gold')]
    count = 0
    while len(stack) > 0:
        (cnt, bagName) = stack.pop()
        bag = next(filter(lambda entry: entry[0] == bagName, data))
        containedBags = [(c * cnt, name) for c, name in bag[1]]
        stack.extend(containedBags)
        count += cnt
    return count - 1

print(solve1())
print(solve2())