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
        a[8] < 0)