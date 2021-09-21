commaScale = Log[81 * 80, 81 / 80]
pa2 = Log2[2] * (1 + commaScale)
pa3 = Log2[3] * (1 - commaScale)
pa5 = Log2[5] * (1 + commaScale)
period = pa2
generator = pa2 * 2 - pa3


Solve[{Log2[2] * a + Log2[3] * b + Log2[5] * c == period, Log2[2] * d + Log2[3] * e + Log2[5] * f == generator, {{1, 2, 4}, {0, -1, -4}}.{{a, d}, {b, e}, {c, f}} == IdentityMatrix[2]}, {a, b, c, d, e, f}] // N
