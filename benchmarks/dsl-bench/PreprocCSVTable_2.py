D = (Optional[int], bool,)
I: (int,) = (0,)
A1 = fold(D, I,
    lambda a, r: (
        match r[0]:
            case None: a[0]
            case r0:
                a[0] + 1 if r[1] and (r0 == 0 or r0 == 1) and not (r0 == 2)
                else a[0],))
A = filter(A1,
    lambda a:
        a[0] >= 0 and
        not (a[0] == 2) and
        not (a[0] == 29))