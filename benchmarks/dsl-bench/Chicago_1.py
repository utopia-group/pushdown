D = (int,)
I = (False, False,)
A1 = fold(D, I,
    lambda a, r: (
        a[0] or r[0] == 0,
        a[1] or r[0] == 1,))
A = filter(A1,
    lambda a:
        a[0] and
        a[1])