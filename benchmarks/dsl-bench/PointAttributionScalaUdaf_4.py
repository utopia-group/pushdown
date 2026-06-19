D = (Optional[int],)
I = (0,)
A1 = fold(D, I,
    lambda a, r: (
        match r[0]:
            case None: a[0]
            case r0: r0 + a[0] if r0 < 3 else 3 + a[0],))
A = filter(A1,
    lambda a:
        (a[0] <= -10 and
         not (a[0] == -77)) or
        (not (a[0] == 100) and
         a[0] > 50))