D = (int,)
I: (int,) = (0,)
A1 = fold(D, I,
    lambda a, r:
        (a[0] + 1
            if r[0] == 0 or
               r[0] == 1 or
               r[0] == 2 or
               r[0] == 3
            else a[0],))
A = filter(A1,
    lambda a:
        a[0] > 0)