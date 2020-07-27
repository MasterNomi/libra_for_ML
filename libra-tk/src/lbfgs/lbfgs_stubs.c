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
#include "lbfgs.h"
#include <stdio.h>
#include <math.h>
#include <string.h>


void val_and_grad(value* f, double* ok_x, const double* x, double* g, 
        int n, double step, double* pfx)
{
    CAMLparam0();
    CAMLlocal3(vx, vg, vret);
    int x_okay = 1;
    int i;
    /* Copy x into a caml array. sizeof is to handle 64/32-bit architectures. */
    vx = alloc(n*sizeof(double)/sizeof(void*), Double_array_tag);
    for (i = 0; i < n; i++) { 
        if (!isfinite(x[i])) { x_okay = 0; }
        Store_double_field(vx, i, x[i]); 
    } 
    /* Save x if it's finite */
    if (x_okay) {
        for (i = 0; i < n; i++) {
            ok_x[i] = x[i];
        }
    }
    /* Execute the callback, storing the gradient and returning the value */
    vg = alloc(n*sizeof(double)/sizeof(void*), Double_array_tag);
    for (i = 0; i < n; i++) { Store_double_field(vg, i, 0.0); } 
    vret = callback2(*f, vx, vg);
    *pfx = Double_val(vret);
    /* Extract the gradient and return the value */
    for (i = 0; i < n; i++) { g[i] = Double_field(vg, i); }
    CAMLreturn0;
}

struct func_and_vec {
    value* f;
    double* x;
};

double val_and_grad2(void* instance, const double* x, double* g,
        int n, double step) {
    double fx;
    struct func_and_vec* fnx = (struct func_and_vec*)instance;
    val_and_grad(fnx->f, fnx->x, x, g, n, step, &fx);
    return fx;
}

CAMLprim value c_lbfgs(value f, value vx, value vc, value vdelta, value vmaxiter)
{
    CAMLparam5 (f, vx, vc, vdelta, vmaxiter);
    CAMLlocal1 (ret);
    int n = Wosize_val(vx);
    double c = Double_val(vc);
    double delta = Double_val(vdelta);
    int maxiter = Int_val(vmaxiter);
    double* x = (double*)malloc(sizeof(double) * n);
    double fx;
    int i;
    int x_okay = 1;
    int errcode;
    struct func_and_vec fng;
    lbfgs_parameter_t param;

    for (i = 0; i < n; i++) { x[i] = Double_field(vx, i); }

    /* Initialize struct to hold data */
    fng.f = &f;
    fng.x = (double*)malloc(sizeof(double) * n);
    memcpy(fng.x, x, sizeof(double) * n);

    lbfgs_parameter_init(&param);
    if (delta > 0.0) {
        param.past = 5;
        param.delta = delta;
    }
    param.max_iterations = maxiter;
    param.orthantwise_c = c;
    if (c > 0.0) {
        param.linesearch = LBFGS_LINESEARCH_BACKTRACKING;
    }
    errcode = lbfgs(n, x, &fx, val_and_grad2, NULL, (void*)&fng, &param); 
    for (i = 0; i < n; i++) {
        if (!isfinite(x[i])) {
          x_okay = 0;
        }
    }
    for (i = 0; i < n; i++) {
        Store_double_field(vx, i, x_okay ? x[i] : fng.x[i]);
    }
    free(x);
    free(fng.x);
    ret = caml_alloc(2, 0);
    Store_field(ret, 0, Val_int(errcode));
    Store_field(ret, 1, caml_copy_double(fx));
    fflush(stdout);
    CAMLreturn(ret);
}

