D = (int, int,)
I: (int, Optional[int], int, Optional[int],) = (0, None, 0, None,)
A1 = fold(D, I,
    lambda a, r: (
        match a[1]:
            case None: r[0]
            case a1: r[0] if r[1] < a1 else a[0],
        match a[1]:
            case None: r[1]
            case a1: r[1] if r[1] < a1 else a1,
        match a[3]:
            case None: r[0]
            case a3: r[0] if r[1] > a3 else a[2],
        match a[3]:
            case None: r[1]
            case a3: r[1] if r[1] > a3 else a3,))
A = filter(A1,
    lambda a:
        a[0] <= 100 and
        (match a[1]:
            case None: False
            case a1: a1 < 38 or a1 == 40) and
        a[2] >= 10 and
        (match a[3]:
            case None: False
            case a3: a3 == 53 or a3 >= 55))