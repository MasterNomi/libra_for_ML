/******************************************************************************
 * The Libra Toolkit                                                          *
 *                                                                            *
 * Copyright (C) 2015 by Daniel Lowd and Amirmohammad Rooshenas               *
 * All rights reserved.                                                       *
 *                                                                            *
 * Redistribution and use in source and binary forms, with or without         *
 * modification, are permitted provided that the following conditions are     *
 * met:                                                                       *
 *                                                                            *
 * 1. Redistributions of source code must retain the above copyright          *
 * notice, this list of conditions and the following disclaimer.              *
 *                                                                            *
 * 2. Redistributions in binary form must reproduce the above copyright       *
 * notice, this list of conditions and the following disclaimer in the        *
 * documentation and/or other materials provided with the distribution.       *
 *                                                                            *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS        *
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT          *
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR      *
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT       *
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,      *
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT           *
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,      *
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY      *
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT        *
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE      *
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.       *
 ******************************************************************************/

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <math.h>
//#include <stdio.h>
//#include <string.h>

//#include "math_num.h"
double fminbr(				// An estimate to the min location
	const double ax,		// Specify the interval the minimum
	const double bx,		// to be sought in
	double (*f)(double, void*),		// Function under investigation
    void* user_data,
	const double tol);		// Acceptable tolerance


void evalfunc_helper(double* fx, value* f, double x)
{
    CAMLparam0();
    CAMLlocal2(vx, vret);
    vx = caml_copy_double(x);
    vret = callback(*f, vx);
    *fx = Double_val(vret);
    CAMLreturn0;
}

double evalfunc(double x, void* user_data)
{
    double fx;
    value* f = (value *)user_data;
    evalfunc_helper(&fx, f, x);
    return fx;
}


CAMLprim value c_fminbr(value vax, value vbx, value vf, value vtol)
{
    CAMLparam4(vax, vbx, vf, vtol);
    CAMLlocal1(vret);

    double ax = Double_val(vax);
    double bx = Double_val(vbx);
    double tol = Double_val(vtol);
    double ret;

    ret = fminbr(ax, bx, evalfunc, &vf, tol);
    CAMLreturn(caml_copy_double(ret));
}

