def digits(n, i):
    """Generates the n-numbered digits of i, in reverse order."""
    while True:
        if i == 0:
            return
        else:
            i, q = divmod(i, n)
            yield q
            
def parity(n, i):
    return sum(digits(n, i)) % n

