D = (int, int, int, int,)
I: (Optional[int], Optional[int], Optional[int], Optional[int],) = (None, None, None, None,)
A1 = fold(D, I,
    lambda a, r:
        (r[2] if r[1] > r[0] and r[1] < r[0] + r[3] else a[0],
         r[1] if r[1] > r[0] and r[1] < r[0] + r[3] else a[1],
         r[2] if r[1] > r[0] and r[1] < r[0] + r[3] else a[2],
         r[1] if r[1] > r[0] and r[1] < r[0] + r[3] else a[3],))
A = filter(A1,
    lambda a:
        (match a[0]:
            case None: False
            case a0: a0 < 10) or
        (match a[1]:
            case None: False
            case a1: a1 >= 50) or
        (match a[2]:
            case None: False
            case a2: a2 <= 99))