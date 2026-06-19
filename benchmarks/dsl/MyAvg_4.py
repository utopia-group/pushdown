D = (Optional[float],)
I: (Optional[float], int,) = (None, 0,)
A1 = fold(D, I,
    lambda a, r: (
        match r[0]:
            case None: a[0]
            case r0:
                match a[0]:
                    case None: r0
                    case a0: r0 + a0,
        match r[0]:
            case None: a[1]
            case r0:
                match a[0]:
                    case None: 1
                    case _: a[1] + 1,))
A = filter(A1,
    lambda a:
        (match a[0]:
            case None: False
            case a0: a0 > 140000. and not (a0 == 175000.) and a0 <= 200000.) and
        a[1] >= 2000000 and
        not (a[1] == 2500000))