D = (Optional[int], int, int,)
I: (Optional[int], Optional[int], Optional[int],) = (None, None, None,)
A1 = fold(D, I,
    lambda a, r: (
        match r[0]:
            case None: a[0]
            case r0: r0 if r0 == 50 else a[0],
        match r[0]:
            case None: a[1]
            case r0: r[1] if r0 == 50 else a[1],
        match r[0]:
            case None: a[2]
            case r0: r[2] if r0 == 50 else a[2],))
A = filter(A1,
    lambda a:
        (match a[0]:
            case None: False
            case a0: a0 < 22 or a0 >= 51) and
        (match a[1]:
            case None: False
            case a1: a1 >= 22) and
        (match a[2]:
            case None: False
            case a2: a2 <= 44))