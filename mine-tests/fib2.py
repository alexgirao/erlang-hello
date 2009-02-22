#!/usr/bin/env python

if __name__ == "__main__":
    acc, x, y = 0, 0, 1
    while 1:
        if x % 2 == 0:
            if acc + x < 1000000:
                acc += x
            else:
                break
        x, y = y, x + y
    print acc
