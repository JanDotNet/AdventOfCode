import itertools as it

with open("Data\Day10.txt") as file:
    data = [int(line.strip()) for line in file]
    
def solve_part1():
    idx_start = 0
    idx_current = 25
    while idx_current < len(data):
        current = data[idx_current]
        sum_of_pairs = set([x + y for (x, y) in it.product(data[idx_start:idx_current], repeat=2) if x != y])
        if current not in sum_of_pairs:
            return current
        idx_start += 1
        idx_current += 1

def solve_part2():
    idx = 0
    length = 1
    target = 10884537
    result = 0
    while True:
        items = data[idx:length]
        result = sum(items)
        if result > target:
            idx += 1
            length = 1
        if  result < target:
            length += 1
        if result == target:
            return min(items) + max(items)

print(f"Part 1: {solve_part1()}")
print(f"Part 2: {solve_part2()}")