D = (int,)
I: (Optional[int], Optional[int],) = (None, None,)
A1 = fold(D, I,
    lambda a, r: (
        match a[0]:
            case None: r[0]
            case a0:
                r[0] if r[0] > a0 else a0,
        match a[0]:
            case None: a[0]
            case a0:
                (match a[1]:
                    case None: r[0]
                    case a1:
                        r[0] if r[0] > a1 else a1)
                    if not (r[0] > a0) else a0,))
A = filter(A1,
    lambda a:
        (match a[0]:
            case None: False
            case a0: a0 == 70 or a0 >= 100) and
        (match a[1]:
            case None: False
            case a1: a1 == 80 or a1 >= 90))