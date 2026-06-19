D = (int, int,)
I: (int, int, int,) = (0, 0, 0,)
A1 = fold(D, I,
    lambda a, r: (
        (1 if r[0] <= 0 and r[1] >= 0 else a[0])
            if not (r[0] < 0 or r[0] > r[1]) else a[0],
        (1 if r[0] <= 1 and r[1] >= 1 else a[1])
            if not (r[0] < 0 or r[0] > r[1]) else a[1],
        (1 if r[0] <= 2 and r[1] >= 2 else a[2])
            if not (r[0] < 0 or r[0] > r[1]) else a[2],))
A = filter(A1,
    lambda a:
        a[0] >= 1 and
        a[0] < 10 and
        a[1] == 1 and
        a[2] == 1)