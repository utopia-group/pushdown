D = (int, int,)
I: (int, int,) = (0, 0,)
A1 = fold(D, I,
    lambda a, r: (
        a[0] + r[0] if r[1] >= 5 and r[1] <= 10 else a[0],
        a[1] + r[0] if r[1] >= 4 and r[1] <= 9 else a[1],))
A = filter(A1,
    lambda a:
        (a[0] < 10 or
         a[0] >= 25) and
        ((a[1] <= 5 or
          a[1] > 100) and
         not (a[1] == 25)))