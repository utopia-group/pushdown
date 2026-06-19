D = (int, int, int, int, int,)
I: (int, int, int, int, int,) = (0, 0, 0, 0, 0,)
A1 = fold(D, I,
    lambda a, r: (
        r[0] if a[4] < r[4] else a[0],
        r[1] if a[4] < r[4] else a[1],
        r[2] if a[4] < r[4] else a[2],
        r[3] if a[4] < r[4] else a[3],
        r[4] if a[4] < r[4] else a[4],))
A = filter(A1,
    lambda a:
        a[0] >= 1 and
        (not (a[1] == 2) or
         a[3] > 59000) and
        a[4] >= 21)