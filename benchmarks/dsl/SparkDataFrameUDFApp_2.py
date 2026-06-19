D = (int, int,)
I: (int, int,) = (0, 0,)
A1 = fold(D, I,
    lambda a, r: (
        a[0] + r[0] if r[1] >= 5 and r[1] <= 10 else a[0],
        a[1] + r[0] if r[1] >= 4 and r[1] <= 9 else a[1],))
A = filter(A1,
    lambda a:
        a[0] >= 9 and
        a[1] > 5 and
        not (a[1] >= 10 and a[1] <= 12))