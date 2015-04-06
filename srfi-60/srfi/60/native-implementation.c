/*
 * native-implementation.c -- Chibi-native bitwise arithmetic
 * Copyright (c) 2015 ilammy <a.lozovsky@gmail.com>
 * 3-clause BSD license: http://github.com/ilammy/srfi-60/blob/master/LICENSE
 */

#include <assert.h>
#include <stdbool.h>

#include <chibi/sexp.h>
#include <chibi/bignum.h>

/*
 * Two's-complement conversion
 *
 * One option to get a two's-complement would be to allocate new bignums and
 * store converted arguments there. Pro: conversion is straighforward. Con:
 * more stress on the garbage collector (three new bignums instead of one).
 * So instead we go for the on-the-fly streaming conversion.
 */

static inline
sexp_uint_t signum_limb(sexp bignum)
{
	assert(sexp_bignump(bignum) && "Expect a bignum argument");

	return (sexp_uint_t)((sexp_bignum_sign(bignum) < 0) ? -1 : 0);
}

static inline
bool index_overflows(sexp_uint_t index, sexp bignum)
{
	assert(sexp_bignump(bignum) && "Expect a bignum argument");

	return (index >= sexp_bignum_length(bignum));
}

enum
{
	STATE_MET_ONLY_ZEROS,
	STATE_MET_NON_ZERO,
	STATE_OVERFLOW,
};

#define START_ITERATION STATE_MET_ONLY_ZEROS

/**
 * Iterate over a bignum limbs in two's-complement representation.
 *
 * This function implements stateful iteration over (limitless) bignum limbs.
 * The `state` parameter should be initialized with `START_ITERATION` on start
 * and not messed up with after the first call. The `index` is meant to be
 * incremented by one between the calls (until the `state` is reinitialized).
 *
 * Remember that numbers in two's-complement representation have effectively
 * unlimited length due to sign-extension. That is, positive numbers have
 * unlimited stream of zeroes as their most significant bits while negative
 * numbers have a stream of ones at the same place. Therefore it is safe to
 * pass indices larger than the number of limbs in a fixnum, in this case a
 * special sign-extension limb will be returned.
 *
 * @param bignum  a bignum to iterate over
 * @param index   bignum limb index
 * @param state   pointer to iteration state
 *
 * @returns Two's-complement representation of `bignum[index]` limb.
 */
static inline
sexp_uint_t bignum_next_limb(sexp bignum, sexp_uint_t index, int *state)
{
	sexp_uint_t limb;

	assert(state && "State must be non-NULL");
	assert(sexp_bignump(bignum) && "Expect a bignum argument");

	if ((*state == STATE_OVERFLOW) || index_overflows(index, bignum)) {
		*state = STATE_OVERFLOW;
		return signum_limb(bignum);
	}

	limb = sexp_bignum_data(bignum)[index];

	if (sexp_bignum_sign(bignum) < 0) {
		limb = (*state == STATE_MET_ONLY_ZEROS) ? -limb : ~limb;

		if ((*state == STATE_MET_ONLY_ZEROS) && (limb != 0))
			*state = STATE_MET_NON_ZERO;
	}

	return limb;
}

/**
 * Convert two's-complement representation to conventional one.
 *
 * "Two's complement bignums" are a special intermediate kind of bignums that
 * result from bitwise operations. They have non-zero length and contain data,
 * but that data is an array of _unsigned_ limbs. The sign of the bignum is +,
 * however the actual sign is encoded in the most significant bit of the last
 * limb. This function converts such bignums into proper sign-magnitude ones.
 *
 * @param bignum  bignum to be converted
 */
static
void convert_to_sign_magnitude(sexp bignum)
{
	signed char sign;
	sexp_uint_t i, len;
	sexp_uint_t* limbs;
	sexp_uint_t signum;

	assert(sexp_bignump(bignum) && "Expect bignum argument");
	assert((sexp_bignum_length(bignum) > 0) && "Bignum must be not empty");
	assert((sexp_bignum_sign(bignum) == 1) && "Bignum must be positive");

	len = sexp_bignum_length(bignum);
	limbs = sexp_bignum_data(bignum);
	sign = ((sexp_sint_t)(limbs[len - 1]) >= 0) ? +1 : -1;
	signum = (sign > 0) ? 0 : -1;

	/* Shave off sign extension */

	i = len - 1;
	while ((limbs[i] != signum) && (i > 0))
		i--;

	if (i == 0) {
		/* When all limbs are filled with ones then this is a -1 which
		   will be handled properly below. But if they are filled with
		   zeros then it is 0 which has special sign. */
		if ((sign > 0) && (limbs[0] == signum))
			sign = 0;
	}
	len = i + 1;

	/* Do the actual conversion for negative numbers */

	if (sign < 0) {
		for (i = 0; i < len; i++) {
			limbs[i] = -limbs[i];

			if (limbs[i] != 0)
				break;
		}
		for (i = i + 1; i < len; i++)
			limbs[i] = ~limbs[i];
	}

	sexp_bignum_length(bignum) = len;
	sexp_bignum_sign(bignum) = sign;
}

/*
 * (bit-and num1 num2) - bitwise conjunction
 */

static
sexp sexp_bit_and_big_fix(sexp context, sexp self, sexp_sint_t n,
		sexp bignum, sexp fixnum)
{
	return sexp_type_exception(context, self, SEXP_OBJECT, SEXP_VOID);
}

static inline
sexp_uint_t max(sexp_uint_t v1, sexp_uint_t v2)
{
	return (v1 > v2) ? v1 : v2;
}

static
sexp sexp_bit_and_big_big(sexp context, sexp self, sexp_sint_t n,
		sexp num1, sexp num2)
{
	sexp_uint_t i, len;
	int state1, state2;
	sexp_gc_var1(result);
	sexp_gc_preserve1(context, result);

	assert(sexp_bignump(num1) && "Expect a bignum argument");
	assert(sexp_bignump(num2) && "Expect a bignum argument");

	len = max(sexp_bignum_length(num1), sexp_bignum_length(num2));
	result = sexp_make_bignum(context, len);

	state1 = state2 = START_ITERATION;
	for (i = 0; i < len; i++) {
		sexp_bignum_data(result)[i] =
			bignum_next_limb(num1, i, &state1) &
			bignum_next_limb(num2, i, &state2);
	}

	convert_to_sign_magnitude(result);
	result = sexp_bignum_normalize(result);

	sexp_gc_release1(context);
	return result;
}

static
sexp sexp_bit_and(sexp context, sexp self, sexp_sint_t n, sexp num1, sexp num2)
{
	if (sexp_fixnump(num1) && sexp_fixnump(num2))
		return (sexp)((sexp_uint_t)num1 & (sexp_uint_t)num2);

	if (sexp_fixnump(num1) && sexp_bignump(num2))
		return sexp_bit_and_big_fix(context, self, n, num1, num2);

	if (sexp_bignump(num1) && sexp_fixnump(num2))
		return sexp_bit_and_big_fix(context, self, n, num2, num1);

	if (sexp_bignump(num1) && sexp_bignump(num2))
		return sexp_bit_and_big_big(context, self, n, num1, num2);

	if (!(sexp_fixnump(num1) || sexp_bignump(num1)))
		return sexp_type_exception(context, self, SEXP_FIXNUM, num1);
	else
		return sexp_type_exception(context, self, SEXP_FIXNUM, num2);
}

/*
 * (bitwise-not num) - bitwise inversion
 */

static
sexp sexp_bitwise_not_bignum(sexp context, sexp self, sexp_sint_t n, sexp num)
{
	sexp_gc_var1(result);
	sexp_gc_preserve1(context, result);

	assert(sexp_bignump(num) && "Expect a bignum argument");

	result = sexp_copy_bignum(context, NULL, num, 0);

	if (sexp_bignum_sign(result) >= 0)
		result = sexp_bignum_fxadd(context, result, 1);
	else
		result = sexp_bignum_fxsub(context, result, 1);

	sexp_bignum_sign(result) = -sexp_bignum_sign(result);

	sexp_gc_release1(context);
	return result;
}

static
sexp sexp_bitwise_not(sexp context, sexp self, sexp_sint_t n, sexp num)
{
	if (sexp_fixnump(num))
		return sexp_make_fixnum(~(sexp_uint_t)sexp_unbox_fixnum(num));

	if (sexp_bignump(num))
		return sexp_bitwise_not_bignum(context, self, n, num);

	return sexp_type_exception(context, self, SEXP_FIXNUM, num);
}

/*
 * Library initialization
 */

sexp sexp_init_library(sexp context, sexp self, sexp_sint_t n, sexp env,
		const char *version, const sexp_abi_identifier_t abi)
{
	if (!(sexp_version_compatible(context, version, sexp_version)
		&& sexp_abi_compatible(context, abi, SEXP_ABI_IDENTIFIER)))
	{
		return SEXP_ABI_ERROR;
	}

	sexp_define_foreign(context, env, "bit-and", 2, sexp_bit_and);
	sexp_define_foreign(context, env, "bitwise-not", 1, sexp_bitwise_not);

	return SEXP_VOID;
}
