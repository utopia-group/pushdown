D = (Optional[float],)
I: (float,) = (0.,)
A1 = fold(D, I,
    lambda a, r: (
        match r[0]:
            case None: a[0]
            case r0:
                a[0] + r0 if r0 > 500. else a[0],))
A = filter(A1,
    lambda a:
        a[0] <= 500. or
        ((not (a[0] == 1000.) or
          not (a[0] == 1500.)) and
         a[0] < 50000000000.))