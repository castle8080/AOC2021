
with open("day_01_1.txt", "rt", encoding='utf-8-sig') as fh:
    numbers = [int(line.strip()) for line in fh]

p = None
increases = []
for i in numbers:
    if p is not None and i > p:
        increases.append((p, i))
    p = i

print(len(increases))
print(len(numbers))
