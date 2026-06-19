D = (Optional[int], Optional[int],)
I: (int,) = (0,)
A1 = fold(D, I,
    lambda a, r: (
        match r[0]:
            case None: a[0]
            case r0:
                match r[1]:
                    case None:
                        a[0] + r0 if r0 > 400 else a[0]
                    case r1:
                        a[0] + r0 if (r1 == 0 or r1 == 1 or r1 == 2) and r0 > 1000
                        else a[0],))
A = filter(A1,
    lambda a:
        (a[0] > 1000 and
         not (a[0] == 1400)) or
        a[0] <= 400)