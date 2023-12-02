constrains = {
    "green" : 13,
    "blue" : 14,
    "red" : 12
}
acc = 0
def is_valid(game):
    s = 0
    num_to_color = game.split(',')
    for x in num_to_color:
        x = x[1:]
        num = int(x.split(' ')[0])
        color = x.split(' ')[1]
        s+=num
        if constrains[color] < num:
            return False
    return sum(list(constrains.values())) > s
with open("day2.txt", 'r') as f:
    print(constrains.values())
    for line in f.readlines():
        id = int(line.split(':')[0].split(' ')[1])
        games = line.split(':')[1].split(';')
        print(id, games)
        valid = True
        for game in games:
            valid &= is_valid(game.replace('\n', ''))
        if valid:
            acc+= id
    print(acc)