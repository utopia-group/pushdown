D = (int, int, int,)
I: (Optional[int], Optional[int], Optional[int],) = (None, None, None,)
A1 = fold(D, I,
    lambda a, r: (
        match a[1]:
            case None: r[0]
            case a1:
                r[0] if r[1] < a1 else a[0],
        match a[1]:
            case None: r[1]
            case a1:
                r[1] if r[1] < a1 else a1,
        match a[1]:
            case None: r[2]
            case a1:
                r[2] if r[1] < a1 else a[2],))
A = filter(A1,
    lambda a:
        match a[1]:
            case None: False
            case a1: a1 > 10 and not (a1 == 35) and a1 < 60)