import re 

with open("Data\Day04.txt") as file:
    input = [line.strip() for line in file]

def split_pair(key_value):
    splitted = key_value.split(':')
    return splitted[0], splitted[1] 

def parse_passports(data):
    passports = []
    current = []
    for line in data:
        if line == '' and len(current) > 0:
            passports.append(current)
            current = []
        else:
            pairs = line.split(' ')
            key_value_pairs = map(lambda x: split_pair(x), pairs)
            current.extend(key_value_pairs)
    if len(current) > 0:
        passports.append(current)
    return passports

def has_required_fields(passport):
    required = {'byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid'}
    actual = set(map(lambda x: x[0], passport))
    return len(required - actual) == 0

def are_fields_valid(passport):
    def is_byr_valid(num):
        return num >= 1920 and num <= 2002

    def is_iyr_valid(num):
        return num >= 2010 and num <= 2020

    def is_eyr_valid(num):
        return num >= 2020 and num <= 2030

    def is_hgt_valid(txt):
        if not re.search("^[0-9]*(cm|in)$", txt):
            return False
        num = int(txt[0:-2])
        unit = txt[-2:]
        return num >= 150 and num <= 193 if unit == 'cm' else num >= 59 and num <= 76

    def is_hcl_valid(txt):
        return re.search("^#[0-9a-f]{6}$", txt)

    def is_ecl_valid(txt):
        return value in {'amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth'} 

    def is_pid_valid(txt):
        return re.search("^[0-9]{9}$", txt)
    
    for key, value in passport:
        if key == 'byr' and not is_byr_valid(int(value)): 
            return False
        if key == 'iyr' and not is_iyr_valid(int(value)): 
            return False
        if key == 'eyr' and not is_eyr_valid(int(value)): 
            return False
        if key == 'hgt' and not is_hgt_valid(value): 
            return False
        if key == 'hcl' and not is_hcl_valid(value): 
            return False
        if key == 'ecl' and not is_ecl_valid(value): 
            return False
        if key == 'pid' and not is_pid_valid(value): 
            return False    
    return True


def solve_part1(items):
    passports = parse_passports(items)
    return sum(1 for _ in filter(lambda x: has_required_fields(x), passports))

def solve_part2(items):
    passports = parse_passports(items)
    return sum(1 for _ in filter(lambda x: has_required_fields(x) and are_fields_valid(x), passports))

print(f"Part 1: {solve_part1(input)}")
print(f"Part 2: {solve_part2(input)}")

