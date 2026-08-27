from __future__ import annotations

from bisect import bisect_left


def interp(x: float, xs, ys) -> float:
    x = min(max(x, xs[0]), xs[-1])
    i = min(max(bisect_left(xs, x), 1), len(xs) - 1)
    f = (x - xs[i - 1]) / (xs[i] - xs[i - 1])
    return ys[i - 1] + f * (ys[i] - ys[i - 1])


def interp2(x: float, y: float, grid) -> float:
    x = min(max(x, grid.x[0]), grid.x[-1])
    y = min(max(y, grid.y[0]), grid.y[-1])
    i = min(max(bisect_left(grid.x, x), 1), len(grid.x) - 1)
    j = min(max(bisect_left(grid.y, y), 1), len(grid.y) - 1)
    fx = (x - grid.x[i - 1]) / (grid.x[i] - grid.x[i - 1])
    fy = (y - grid.y[j - 1]) / (grid.y[j] - grid.y[j - 1])
    a = grid.values[i - 1][j - 1] * (1 - fx) + grid.values[i][j - 1] * fx
    b = grid.values[i - 1][j] * (1 - fx) + grid.values[i][j] * fx
    return a * (1 - fy) + b * fy


def solve_tridiagonal(lower, diagonal, upper, rhs):
    n = len(rhs)
    c, d = list(upper), list(rhs)
    b = list(diagonal)
    for i in range(1, n):
        if abs(b[i - 1]) < 1e-30:
            raise ArithmeticError("Нульовий діагональний елемент")
        q = lower[i - 1] / b[i - 1]
        b[i] -= q * c[i - 1]
        d[i] -= q * d[i - 1]
    x = [0.0] * n
    x[-1] = d[-1] / b[-1]
    for i in range(n - 2, -1, -1):
        x[i] = (d[i] - c[i] * x[i + 1]) / b[i]
    return x
