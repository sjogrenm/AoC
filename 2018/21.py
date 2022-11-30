r0 = 0 #14833070
r1 = 0
r2 = 0
r3 = 0
r5 = 0

outer_loops = 0

seen = {}

last_added = None

while True:
    r5 = r1 | 65536
    r1 = 8586263
    while True:
        r2 = r5 & 255
        r1 = r1 + r2
        r1 = r1 & 16777215
        r1 = r1 * 65899
        r1 = r1 & 16777215
        if 256 > r5:
            break
        r2 = 0
        while True:
            r3 = r2 + 1
            r3 = r3 * 256
            if r3 > r5:
                r5 = r2
                break
            else:
                r2 = r2 + 1
#    print("r1: %d, r5: %d" % (r1,r5))
    if seen.has_key(r1):
        print("found %d again" % r1)
#        print("last added was %s" % last_added)
        break
    last_added = r1
    seen[r1] = 0
    if r0 == r1:
        break
    outer_loops += 1
#    print("loop: %d, r1=%d, r5=%d" % (outer_loops,r1,r5))
print last_added
print outer_loops
