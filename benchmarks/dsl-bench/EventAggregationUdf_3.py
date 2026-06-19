D = (int, int,)
I: (int, int, Optional[int], Optional[int],) = (0, 0, None, None,)
A1 = fold(D, I,
    lambda a, r: (
        a[0] + 1 if r[0] == 1 else a[0],
        a[1] + 1 if r[0] == 2 else a[1],
        match a[2]:
            case None: r[1]
            case a2:
                match a[3]:
                    case None: r[1]
                    case a3:
                        r[1] if r[1] < a2 else a2,
        match a[2]:
            case None: r[1]
            case a2:
                match a[3]:
                    case None: r[1]
                    case a3:
                        r[1] if r[1] > a3 else a3,))
A = filter(A1,
    lambda a:
        a[0] >= 0 and
        a[1] > 0 and
        (match a[2]:
            case None: False
            case a2: a2 <= 5) and
        (match a[3]:
            case None: False
            case a3: a3 >= 10))