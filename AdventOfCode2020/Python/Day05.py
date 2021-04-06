import re

with open("Data\Day05.txt") as file:
    input = [line.strip() for line in file]

def parse_seat(line):
    line = re.sub('[BR]', '1', line) 
    line = re.sub('[FL]', '0', line) 
    row = int(line[0:7], 2)
    col = int(line[7:10], 2)
    return row * 8 + col

def solve_part1(lines):
    return max([parse_seat(line) for line in lines])
    
def solve_part2(lines):
    seat_ids = [parse_seat(line) for line in lines]
    seat_range = range(min(seat_ids), max(seat_ids) 
    return next([seat in seat_range if seat not in seat_ids])

print(f"Part 1: {solve_part1(input)}")
print(f"Part 2: {solve_part2(input)}")