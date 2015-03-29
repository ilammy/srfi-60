import math
import random

def make_procedures_for(width_limit):
    integer_limit = 2**width_limit

    exported_functions = {}

    def export(function):
        exported_functions[function.__name__] = function
        return function

    def domain(*definitions):
        def annotate(function):
            function.domain = tuple(definitions)
            return function
        return annotate

    #
    # Domain definitions
    #

    def integer(low=-integer_limit, high=+integer_limit):
        return lambda: random.randint(low, high)

    def index(excess=0):
        return integer(0, width_limit - 1 + excess)

    def index_start():
        return integer(0, width_limit / 2)

    def index_end():
        return integer(width_limit / 2, width_limit - 1)

    def offset(excess=0):
        limit = width_limit + excess
        return integer(-limit, +limit)

    def boolean():
        return lambda: bool(random.randint(0, 1))

    def list_of(count, thing):
        return lambda: [thing() for _ in xrange(count())]

    #
    # Bitwise Operations
    #

    @domain(integer(), integer())
    @export
    def bitwise_and(a, b):
        return (a & b)

    @domain(integer(), integer())
    @export
    def bitwise_ior(a, b):
        return (a | b)

    @domain(integer(), integer())
    @export
    def bitwise_xor(a, b):
        return (a ^ b)

    @domain(integer())
    @export
    def bitwise_not(x):
        return (~x)

    @domain(integer(), integer(), integer())
    @export
    def bitwise_if(c, t, f):
        return ((c & t) | (~c & f))

    @domain(integer(), integer())
    @export
    def any_bits_setp(m, n):
        return bool((m & n) != 0)

    #
    # Integer Properties
    #

    @domain(integer())
    @export
    def bit_count(n):
        if n < 0: n = ~n
        count = 0
        while n:
            n &= n - 1
            count += 1
        return count

    @domain(integer())
    @export
    def integer_length(n):
        if n < 0: n = ~n
        if n == 0: return 0
        return int(math.floor(math.log(n, 2) + 1))

    @domain(integer())
    @export
    def first_set_bit(n):
        return integer_length(n & -n) - 1

    #
    # Bit Within Word
    #

    @domain(index(), integer())
    @export
    def bit_setp(index, num):
        return bool(num & (2**index))

    @domain(index(), integer(), boolean())
    @export
    def copy_bit(index, num, bit):
        mask = (1 << index)
        num &= ~mask
        num |= (int(bit) << index)
        return num

    #
    # Field of Bits
    #

    def mask_of(length):
        return ((1 << length) - 1)

    def mask_at(start, end):
        return mask_of(end - start) << start

    @domain(integer(), index_start(), index_end())
    @export
    def bit_field(num, start, end):
        if end <= start:
            return 0
        return ((num >> start) & mask_of(end - start))

    @domain(integer(), integer(), index_start(), index_end())
    @export
    def copy_bit_field(num_to, num_from, start, end):
        if end <= start:
            return num_to
        to_mask = ~mask_at(start, end)
        from_mask = mask_of(end - start)
        return ((num_to & to_mask) | ((num_from & from_mask) << start))

    @domain(integer(), offset(+2))
    @export
    def arithmetic_shift(num, count):
        if count < 0:
            return (num >> -count)
        else:
            return (num << count)

    @domain(integer(), offset(+2), index_start(), index_end())
    @export
    def rotate_bit_field(num, count, start, end):
        if end <= start:
            return num
        count %= (end -start)
        mask = mask_at(start, end)
        chunk = bit_field(num, start, end)
        left = arithmetic_shift(chunk, count)
        right = arithmetic_shift(chunk, count - (end - start))
        return bitwise_if(mask, (left | right) << start, num)

    def bit_reverse(num, length):
        res = 0
        for _ in xrange(length):
            res <<= 1
            res |= (num & 1)
            num >>= 1
        return res

    @domain(integer(), index_start(), index_end())
    @export
    def reverse_bit_field(num, start, end):
        if end <= start:
            return num
        mask = mask_at(start, end)
        chunk = bit_field(num, start, end)
        chunk = bit_reverse(chunk, (end - start))
        return bitwise_if(mask, chunk << start, num)

    #
    # Bits as Booleans
    #

    @domain(integer(low=0), index(+4))
    @export
    def integer_to_list(num, len):
        result = []
        while (len > 0):
            result.append(bool(num & 1))
            num >>= 1
            len -= 1
        result.reverse()
        return result

    @domain(list_of(index(), boolean()))
    @export
    def list_to_integer(booleans):
        result = 0
        for bit in booleans:
            result <<= 1
            result |= int(bit)
        return result

    #
    # Now it's time to return
    #

    return exported_functions
