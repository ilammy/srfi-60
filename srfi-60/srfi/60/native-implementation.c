/*
 * native-implementation.c -- Chibi-native bitwise arithmetic
 * Copyright (c) 2015 ilammy <a.lozovsky@gmail.com>
 * 3-clause BSD license: http://github.com/ilammy/srfi-60/blob/master/LICENSE
 */

#include <chibi/sexp.h>
#include <chibi/bignum.h>

sexp sexp_init_library(sexp context, sexp self, sexp_sint_t n, sexp env,
		const char *version, const sexp_abi_identifier_t abi)
{
	if (!(sexp_version_compatible(context, version, sexp_version)
		&& sexp_abi_compatible(context, abi, SEXP_ABI_IDENTIFIER)))
	{
		return SEXP_ABI_ERROR;
	}

	return SEXP_VOID;
}
