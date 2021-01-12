from itertools import cycle

with open("Data\Day03.txt") as file:
    input = [line.strip() for line in file]

def getitem(index, iterable):
    for idx, val in enumerate(iterable):
        if idx == index:
            return val

def istree(row, rowidx, rowStep, colStep):
    if rowidx % rowStep != 0:
        return False
    colidx = rowidx * colStep
    element = getitem(colidx, cycle(row))
    return element == '#'

def solve_part1(items):
    count = 0
    for rowidx, row in enumerate(input):
        count = count + 1 if istree(row, rowidx, 1, 3) else count
    return count

def solve_part2(items):
    steps = [(1, 1), (1, 3), (1, 5), (1, 7), (2, 0.5)]
    count = [0] * len(steps)
    for rowidx, row in enumerate(input):
        for idx, (rowstep, colstep) in enumerate(steps):
            count[idx] = count[idx] + 1 if istree(row, rowidx, rowstep, colstep) else count[idx]
    return count[0] * count[1] * count[2] *  count[3] * count[4] 

print(f"Part 1: {solve_part1(input)}")
print(f"Part 2: {solve_part2(input)}")