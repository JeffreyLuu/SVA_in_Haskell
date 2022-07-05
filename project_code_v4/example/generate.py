N = 1000
values = [i/N for i in range(N+1)]

AN = 10
BN = 10
A = [i/AN for i in range(AN)]
B = [i/BN+0.01 for i in range(BN)]

total = (N+1) * len(A) * len(B)
TOT = 10 ** -5
cntA = 0
cntB = 0
for v in values:
    for a in A:
        for b in B:
            if a < v:
                if a > b+TOT: cntA += 1
                elif a > b-TOT: cntA += 0.5
            if b < v:
                if b > a+TOT: cntB += 1
                elif b > a-TOT: cntB += 0.5

print(cntA/total)
print(cntB/total)