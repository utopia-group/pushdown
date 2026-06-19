D = (Optional[str], bool,)
I: (int,) = (0,)
A1 = fold(D, I,
    lambda a, r: (
        match r[0]:
            case None: a[0]
            case r0:
                a[0] + 1 if r[1] and (r0 == "AMB" or r0 == "IMP") and not (r0 == "EMER")
                else a[0],))
A = filter(A1,
    lambda a:
        a[0] > 0 or
        (a[0] < -10 and
         not (a[0] == -29)))