/*
 * native-implementation.c -- Chibi-native bitwise arithmetic
 * Copyright (c) 2015 ilammy <a.lozovsky@gmail.com>
 * 3-clause BSD license: http://github.com/ilammy/srfi-60/blob/master/LICENSE
 */

#include <assert.h>

#include <chibi/sexp.h>
#include <chibi/bignum.h>

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
