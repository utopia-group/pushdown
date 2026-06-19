D = (str,)
I = (False, False,)
A1 = fold(D, I,
    lambda a, r: (
        a[0] or r[0] == "M",
        a[1] or r[0] == "F",))
A = filter(A1,
    lambda a:
        a[0] or
        a[1])