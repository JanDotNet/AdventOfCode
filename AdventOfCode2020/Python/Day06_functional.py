import string as s
import functools as ft

with open("Data\Day06.txt") as file:
    block = file.read().split('\n\n')    
    input = list(map(lambda x: x.split('\n'), block))

def count(agg, initial, data):
    def count_answers(answers):
        s1 = map(lambda x: set(x), answers)
        s2 = ft.reduce(agg, s1, initial)
        return len(s2)
    return sum(cnt for cnt in map(lambda x: count_answers(x), data))

def solve_part1(data):
    return count(lambda a, b: a | b, set(), data)
    
def solve_part2(data):
    return count(lambda a, b: a & b, set(s.ascii_lowercase), data)

print(f"Part 1: {solve_part1(input)}")
print(f"Part 2: {solve_part2(input)}")