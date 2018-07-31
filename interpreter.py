import fileinput

class Ins:
    def __init__(self, ins, *args):
        self.ins = ins
        self.args = args

class Fun:
    def __init__(self, id):
        self.ins = []
        self.labels = dict()
        self.cnt = 0
        self.id = id

f = open("test.asm")

fns = dict()

for line in fileinput.input():
    t = line.split()
    if t[0] == "fun":
        curfn = Fun((t[1]))
        fns[(t[1])] = curfn
    elif t[0] == "label":
        tmp = Ins(t[0], *t[1:])
        curfn.labels[t[1]] = len(curfn.ins)
        curfn.ins.append(tmp)
    else:
        tmp = Ins(t[0], *t[1:])
        curfn.ins.append(tmp)


def interp(fn, localscope):
    callscope = dict()
    ins = fn.ins[0]
    pc = 0
    
    while True:
        #print("Executing", ins.ins)
        if ins.ins == "lit":
            if ins.args[0] == "integer":
                localscope[ins.args[1]] = int(ins.args[2])
            elif ins.args[0] == "real":
                localscope[ins.args[1]] = float(ins.args[2])
            elif ins.args[0] == "boolean":
                localscope[ins.args[1]] = ins.args[2] == "True"
        elif ins.ins == "set":
            localscope[ins.args[1]] = localscope[ins.args[2]]
        elif ins.ins == "bind":
            callscope[ins.args[1]] = localscope[ins.args[2]]
        elif ins.ins == "add":
            localscope[ins.args[1]] = (localscope[ins.args[2]]) + (localscope[ins.args[3]])
        elif ins.ins == "sub":
            localscope[ins.args[1]] = (localscope[ins.args[2]]) - (localscope[ins.args[3]])
        elif ins.ins == "mul":
            localscope[ins.args[1]] = (localscope[ins.args[2]]) * (localscope[ins.args[3]])
        elif ins.ins == "div":
            localscope[ins.args[1]] = (localscope[ins.args[2]]) / (localscope[ins.args[3]])
        elif ins.ins == "lt":
            localscope[ins.args[1]] = (localscope[ins.args[2]]) < (localscope[ins.args[3]])
        elif ins.ins == "gt":
            localscope[ins.args[1]] = (localscope[ins.args[2]]) > (localscope[ins.args[3]])
        elif ins.ins == "le":
            localscope[ins.args[1]] = (localscope[ins.args[2]]) <= (localscope[ins.args[3]])
        elif ins.ins == "ge":
            localscope[ins.args[1]] = (localscope[ins.args[2]]) >= (localscope[ins.args[3]])
        elif ins.ins == "eq":
            localscope[ins.args[1]] = (localscope[ins.args[2]]) == (localscope[ins.args[3]])
        elif ins.ins == "neq":
            localscope[ins.args[1]] = (localscope[ins.args[2]]) != (localscope[ins.args[3]])
        elif ins.ins == "not":
            localscope[ins.args[0]] = not localscope[ins.args[1]]
        elif ins.ins == "jmp":
            pc = fn.labels[(ins.args[0])]
        elif ins.ins == "jmpt":
            if localscope[ins.args[0]]:
                pc = fn.labels[(ins.args[1])]
        elif ins.ins == "call":
            localscope[ins.args[1]] = interp(fns[(ins.args[2])], callscope)
        elif ins.ins == "ret":
            return localscope['0']
        elif ins.ins == "label":
            pass
        else:
            print("unsupporetd instruction", ins.ins)
        pc += 1
        ins = fn.ins[pc]

print(interp(fns["main"], dict()))