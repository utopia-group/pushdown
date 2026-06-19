D = (Optional[float],)
I = (0, 0,)
A1 = fold(D, I,
    lambda a, r: (
        match r[0]:
            case None: a[0]
            case _: a[0] + 1,
        match r[0]:
            case None: a[1]
            case r0:
                a[1] + 1 if r0 == 1. or r0 == 2. else a[1],))
A = filter(A1,
    lambda a:
        a[0] >= 1 and
        a[0] < 1300 and
        a[1] >= 0 and
        a[1] <= 200)