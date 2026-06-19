D = (int, int,)
I: (int, Optional[int],) = (-1, None,)
A1 = fold(D, I,
    lambda a, r: (
        r[0] if a[0] < r[0] else a[0],
        r[1] if a[0] < r[0] else a[1],))
A = filter(A1,
    lambda a:
        ((a[0] > 10 and
          a[0] < 34) or
         a[0] >= 35) and
        (match a[1]:
            case None: False
            case a1: a1 < 10 or a1 >= 21))