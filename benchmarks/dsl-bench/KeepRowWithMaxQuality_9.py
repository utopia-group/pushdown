D = (int, int, int, int, int, int, int, int, int,)
I: (Optional[int], Optional[int], Optional[int], Optional[int], Optional[int], Optional[int], Optional[int], Optional[int], int,) = (None, None, None, None, None, None, None, None, 0,)
A1 = fold(D, I,
    lambda a, r: (
        r[0] if a[8] < r[8] else a[0],
        r[1] if a[8] < r[8] else a[1],
        r[2] if a[8] < r[8] else a[2],
        r[3] if a[8] < r[8] else a[3],
        r[4] if a[8] < r[8] else a[4],
        r[5] if a[8] < r[8] else a[5],
        r[6] if a[8] < r[8] else a[6],
        r[7] if a[8] < r[8] else a[7],
        r[8] if a[8] < r[8] else a[8],))
A = filter(A1,
    lambda a:
        ((match a[0]:
            case None: False
            case a0: a0 > 1) or
         (match a[2]:
            case None: True
            case a2: a2 == 99) or
         (match a[4]:
            case None: False
            case a4: not (a4 == 200))) and
        (match a[6]:
            case None: False
            case a6: a6 >= 10) and
        (match a[7]:
            case None: False
            case a7: a7 >= 1) and
        a[8] > 90)