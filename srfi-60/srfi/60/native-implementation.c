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

	sexp_define_foreign(context, env, "bitwise-not", 1, sexp_bitwise_not);

	return SEXP_VOID;
}
