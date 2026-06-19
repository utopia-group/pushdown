D = [Optional[int], Optional[int], Optional[int]]
I: [Optional[int], Optional[int], Optional[int]] = [None, None, None]
A1 = fold(D, I,
    lambda a, r: [
        match r[0]:
            case None: a[0]
            case r0:
                match r[1]:
                    case None: a[0]
                    case r1:
                        match r[2]:
                            case None: a[0]
                            case r2:
                                match a[0]:
                                    case None: r0
                                    case a0: r0 if r0 < a0 else a0,
        match r[0]:
            case None: a[1]
            case r0:
                match r[1]:
                    case None: a[1]
                    case r1:
                        match r[2]:
                            case None: a[1]
                            case r2:
                                match a[1]:
                                    case None: r1
                                    case a1: r1 if r1 < a1 else a1,
        match r[0]:
            case None: a[2]
            case r0:
                match r[1]:
                    case None: a[2]
                    case r1:
                        match r[2]:
                            case None: a[2]
                            case r2:
                                match a[2]:
                                    case None: r2
                                    case a2: r2 if r2 < a2 else a2])
A = filter(A1,
    lambda a:
        (match a[0]:
            case None: False
            case a0: not (a0 == 10) and a0 <= 29) or
        (match a[1]:
            case None: False
            case a1: not (a1 == 0) and a1 < 10) or
        (match a[2]:
            case None: False
            case a2: not (a2 == 15) and a2 < 50))