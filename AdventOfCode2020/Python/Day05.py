with open("Data\Day05.txt") as file:
    input = [line.strip() for line in file]

def binstr_to_int(txt, oneChar, zeroChar):
    return int(txt.replace(oneChar, '1').replace(zeroChar, '0'), 2)

def parse_seat(line):
    row = binstr_to_int(line[0:7], 'B', 'F')
    col = binstr_to_int(line[7:10], 'R', 'L')
    return row * 8 + col

def solve_part1(data):
    return max(map(lambda l: parse_seat(l), data))
    
def solve_part2(data):
    seat_ids = list(map(lambda l: parse_seat(l), data))
    for seat in range(min(seat_ids), max(seat_ids)):
        if seat not in seat_ids:
            return seat

print(f"Part 1: {solve_part1(input)}")
print(f"Part 2: {solve_part2(input)}")