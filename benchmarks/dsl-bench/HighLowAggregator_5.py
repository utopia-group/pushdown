D = (# nfpTs
     int,
     # tickTs
     int,
     # price
     int,
     # x
     int,)
I: (# price
    Optional[int],
    # tickTs
    Optional[int], 
    # price
    Optional[int],
    # tickTs
    Optional[int],) = (None, None, None, None,)
A1 = fold(D, I,
    lambda a, r:
        (r[2] if r[1] > r[0] and r[1] < r[0] + r[3] else a[0],
         r[1] if r[1] > r[0] and r[1] < r[0] + r[3] else a[1],
         r[2] if r[1] > r[0] and r[1] < r[0] + r[3] else a[2],
         r[1] if r[1] > r[0] and r[1] < r[0] + r[3] else a[3],))
A = filter(A1,
    lambda a:
        (match a[0]:
            case None: True
            case a0: a0 <= 99) and
        (match a[2]:
            case None: True
            case a2: a2 >= 10) and
        ((match a[1]:
            case None: True
            case a1: a1 >= 99) or
         (match a[3]:
            case None: True
            case a3: a3 <= 10)))