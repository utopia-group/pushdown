D = (str, int,)
I: (int, int, Optional[int], Optional[int],) = (0, 0, None, None,)
A1 = fold(D, I,
    lambda a, r: (
        a[0] + 1 if r[0] == "time" else a[0],
        a[1] + 1 if r[0] == "price" else a[1],
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
        (a[0] > 7 or
         a[1] > 18) and
        (match a[2]:
            case None: False
            case a2: a2 <= 5) and
        (match a[3]:
            case None: False
            case a3: a3 == 7 or a3 >= 10))