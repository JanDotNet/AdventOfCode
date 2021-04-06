with open("Data\Day08.txt") as file:
    data = [line.strip().split() for line in file]
    data = [(el[0], int(el[1])) for el in data]
print(data)

def run(instructions):
    acc = 0
    idx = 0
    processed = set()
    while True:
        inst, num = instructions[idx]
        if inst == 'acc':
            acc += num
        elif inst == 'jmp':
            idx += num - 1
 
        idx += 1

        if idx in processed:
            return acc, 'break'

        if idx >= len(instructions):
            return acc, 'finished'
        
        processed.add(idx)

def solve_part1():
    return run(data)[0]

def solve_part2():
    map = { 'jmp': 'nop',
            'nop': 'jmp',
            'acc': 'acc' }

    for i in range(1, len(data)):
        cpy = data.copy()
        cpy[i] = (map[cpy[i][0]], cpy[i][1])
        acc, fin = run(cpy)
        if fin == 'finished':
            return acc

print(f"Part 1: {solve_part1()}")
print(f"Part 2: {solve_part2()}")