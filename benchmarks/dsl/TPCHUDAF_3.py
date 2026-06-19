D = (str,)
I: (int,) = (0,)
A1 = fold(D, I,
    lambda a, r: (
        a[0] + 1 if r[0] == "1-URGENT" or r[0] == "2-HIGH" else a[0],))
A = filter(A1,
    lambda a:
        not (a[0] < 0))