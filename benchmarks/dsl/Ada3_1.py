D = (int, int,)
I: (Optional[int], Optional[int],) = (None, None,)
A1 = fold(D, I,
    lambda a, r: (
        match a[0]:
            case None: r[0]
            case a0:
                r[0] if r[0] < a0 else a0,
        match a[1]:
            case None: r[1]
            case a1:
                r[1] if r[1] < a1 else a1,))
A = filter(A1,
    lambda a:
        (match a[0]:
            case None: False
            case a0: a0 < 50) and
        (match a[1]:
            case None: False
            case a1: a1 < 50))