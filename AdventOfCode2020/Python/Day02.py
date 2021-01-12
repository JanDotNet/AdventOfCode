with open("Data\Day02.txt") as file:
    input = [line.strip() for line in file]

class Password:
    def __init__(self, min, max, char, password):
        self.min = int(min)
        self.max = int(max)
        self.char = char
        self.password = password

def parse(line):
    splitted = line.split()
    min_max = splitted[0].split("-")
    return Password(min_max[0], min_max[1], splitted[1][0], splitted[2])

def is_valid_part1(pw):
    count = pw.password.count(pw.char)
    return count <= pw.max and count >= pw.min

def is_valid_part2(pw):
    c1 = pw.password[pw.min-1]
    c2 = pw.password[pw.max-1]
    return (pw.char == c1) != (pw.char == c2)

def solve(lines, is_valid):
    passwords = map(parse, lines)
    valid_passwords = filter(is_valid, passwords)
    return sum(1 for _ in valid_passwords)

print(f"Part 1: {solve(input, is_valid_part1)}")
print(f"Part 2: {solve(input, is_valid_part2)}")