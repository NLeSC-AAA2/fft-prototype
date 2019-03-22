UPPER_LEFT = u'\u250c'
UPPER_RIGHT = u'\u2510'
LOWER_LEFT = u'\u2514'
LOWER_RIGHT = u'\u2518'
HORIZONTAL = u'\u2500'
VERTICAL = u'\u2502'

def upper_line(width):
    return UPPER_LEFT + HORIZONTAL * width + UPPER_RIGHT

def lower_line(width):
    return LOWER_LEFT + HORIZONTAL * width + LOWER_RIGHT

def left_line(height):
    return "\n".join([UPPER_LEFT] + [VERTICAL] * height + [LOWER_LEFT])

def right_line(height):
    return "\n".join([UPPER_RIGHT] + [VERTICAL] * height + [LOWER_RIGHT])

def ndtotext(A, w=None, h=None):
    """Returns a string to pretty print the numpy.ndarray `A`.

    Currently supports 1 - 3 dimensions only.
    Raises a NotImplementedError if an array with more dimensions is passed.

    Describe `w` and `h`.
    """
    if A.ndim == 1:
        if w is None:
            return str(A)
        s = " ".join([str(value).rjust(width) for value, width in zip(A, w)])
        return '[{}]'.format(s)
    elif A.ndim == 2:
        widths = [max([len(str(s)) for s in A[:, i]]) for i in range(A.shape[1])]
        s = "".join([' ' + ndtotext(AA, w=widths) + ' \n' for AA in A])
        w0 = sum(widths) + len(widths) - 1 + 2 # spaces between elements and corners
        return upper_line(w0) + '\n'  + s + lower_line(w0)
    elif A.ndim == 3:
        h = A.shape[1]
        strings = [left_line(h)]
        strings.extend(ndtotext(a) + '\n' for a in A)
        strings.append(right_line(h))
        return '\n'.join(''.join(pair) for pair in zip(*map(str.splitlines, strings)))
    raise NotImplementedError("Currently only 1 - 3 dimensions are supported")
