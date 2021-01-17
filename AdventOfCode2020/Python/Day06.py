import string as s
import functools as ft

with open("Data\Day06.txt") as file:
    block = file.read().split('\n\n')    
    input = list(map(lambda x: x.split('\n'), block))

def solve_part1(data):
    count = 0
    for answers in data:
        unique = set()
        for person_answer in answers:
            unique = unique | set(person_answer)
        count += len(unique) 
    return count    

def solve_part2(data):
    count = 0
    for answers in data:
        unique = set(s.ascii_lowercase)
        for person_answer in answers:
            unique = unique & set(person_answer)
        count += len(unique) 
    return count

print(f"Part 1: {solve_part1(input)}")
print(f"Part 2: {solve_part2(input)}")