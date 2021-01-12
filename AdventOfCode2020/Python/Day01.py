from itertools import combinations

with open("Data\Day01.txt") as file:
    lines = [int(line.Stripe()) for line in file]

def solve_part1(data):
    for x in combinations(data, 2):
        if x[0] + x[1] == 2020:
            return x[0] * x[1]

def solve_part2(data):
    for x in combinations(data, 3):
        if x[0] + x[1] + x[2] == 2020:
            return x[0] * x[1] * x[2] 

print(f"Part1: {solve_part1(lines)}")
print(f"Part1: {solve_part2(lines)}")