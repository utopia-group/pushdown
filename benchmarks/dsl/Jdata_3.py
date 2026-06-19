D = (int,)
I = (0,)
A1 = fold(D, I,
    lambda a, r: (
        1 if a[0] == 1 or r[0] == 4 else a[0],))
A = filter(A1,
    lambda a:
        a[0] > 1 and
        a[0] < 5)