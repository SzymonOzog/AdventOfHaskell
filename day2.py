import numpy as np
constrains = {
    "green" : 13,
    "blue" : 14,
    "red" : 12
}
acc = 0
acc2 = 0
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

def get_min_valid(games):
    min_valid = {"green" : 0,
                 "blue" : 0,
                 "red" : 0}
    for game in games:
        num_to_color = game.split(',')
        for x in num_to_color:
            x = x[1:]
            num = int(x.split(' ')[0])
            color = x.split(' ')[1]
            min_valid[color] = max(min_valid[color], num)
    return min_valid 
    
with open("day2.txt", 'r') as f:
    for line in f.readlines():
        id = int(line.split(':')[0].split(' ')[1])
        games = line.replace('\n','').split(':')[1].split(';')
        valid = True
        for game in games:
            valid &= is_valid(game)
        if valid:
            acc+= id
        acc2 += np.prod(list(get_min_valid(games).values()))
    print(acc)
    print(acc2)