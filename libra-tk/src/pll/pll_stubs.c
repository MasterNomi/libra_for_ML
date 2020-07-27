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
 
#include <caml/custom.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include "pll.h"
#include <stdio.h>
#include <math.h>
#include <string.h>

/*
 * OCaml support for data_t type, representing a set of weighted examples.
 */
#define Data_val(v) (*((data_t **) Data_custom_val(v)))

static void data_finalize(value v)
{
    data_t* data = Data_val(v);
    free(data->points);
    free(data->weights);
    free(data);
}

static struct custom_operations custom_data_ops = {
    "ocaml_pll_data_t",
    data_finalize,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default    
};

CAMLprim value
ocaml_create_data(value vdata)
{
    CAMLparam1(vdata);
    CAMLlocal3(vwx, vx, vresult);
    data_t* data = (data_t*)malloc(sizeof(data_t));
    int i, j, xlen;
    int* x;

    data->n = Wosize_val(vdata);
    data->nvar = Wosize_val(Field(Field(vdata, 0), 1));
    data->weights = (double*)malloc(sizeof(double) * data->n);
    data->points  = (int*)malloc(sizeof(int) * data->n * data->nvar);

    // Copy all examples and weights
    for (i = 0, x = data->points; i < data->n; i++, x += data->nvar) { 
        vwx = Field(vdata, i);
        data->weights[i] = Double_val(Field(vwx, 0));
        vx = Field(vwx, 1);
        xlen = Wosize_val(vx);
        for (j = 0; j < xlen; j++) {
            x[j] = Int_val(Field(vx, j));
        }
    }

    vresult = alloc_custom(&custom_data_ops, sizeof(data_t**), 0, 1);
    Data_val(vresult) = data;
    CAMLreturn (vresult);
}

/*
 * OCaml support for mn_t type, representing a Markov network.
 */

#define Mn_val(v) (*((mn_t **) Data_custom_val(v)))

static void mn_finalize(value v)
{
    int i;
    mn_t* mn = Mn_val(v);
    free(mn->schema);
    for (i = 0; i < mn->nf; i++) {
        free(mn->features[i]);
    }
    free(mn->features);
    free(mn->weights);
    free(mn);
}

static struct custom_operations custom_mn_ops = {
    "ocaml_pll_mn_t",
    mn_finalize,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default    
};

CAMLprim value
ocaml_create_mn(value vschema, value vfeatures, value vw)
{
    CAMLparam3(vschema, vfeatures, vw);
    CAMLlocal3(f, cond, vmn);
    int nvar = Wosize_val(vschema);
    int nf   = Wosize_val(vfeatures);
    int*     schema   = (int*) malloc(sizeof(int) * nvar);
    varval** features = (varval**) malloc(sizeof(varval*) * nf);
    double*  weights  = (double*) malloc(sizeof(double) * nf);
    int i, j, nfi;
    mn_t* mn = (mn_t*)malloc(sizeof(mn_t));

    // Copy schema and weights
    for (i = 0; i < nvar; i++) { schema[i] = Int_val(Field(vschema, i)); }
    for (i = 0; i < nf; i++)   { weights[i] = Double_field(vw, i); }

    // Copy feature conditions
    for (i = 0; i < nf; i++) { 
        f = Field(vfeatures, i);
        nfi = Wosize_val(f);
        // DEBUG
        //printf("nfi=%d\n", nfi);
        features[i] = (varval*)malloc(sizeof(varval) * (nfi+1));
        for (j = 0; j < nfi; j++) {
            cond = Field(f, j);
            int sense = Bool_val(Field(cond, 0));
            int var = Int_val(Field(cond, 1));
            int varvalue = Int_val(Field(cond, 2));
            // DEBUG
            //printf("(%d, %d, %d)\n", sense, var, varvalue);
            features[i][j].var = var;
            if (sense) {
                features[i][j].value = varvalue;
            } else {
                features[i][j].value = NOTVALUE(varvalue);
            }
        }
        features[i][nfi].var = END_FEATURE;
    }

    mn->nvar = nvar;
    mn->schema = schema;
    mn->nf = nf;
    mn->features = features;
    mn->weights = weights;

    vmn = alloc_custom(&custom_mn_ops, sizeof(mn_t**), 0, 1);
    Mn_val(vmn) = mn;
    CAMLreturn (vmn);
}


/*
 * OCaml support for cache_t type, representing cached feature gradient data.
 */

#define Cache_val(v) (*((cache_t **) Data_custom_val(v)))

void cache_finalize(value v)
{
    cache_t* cache = Cache_val(v);
    // DO NOT FREE SCHEMA!  free(cache->schema);
    free(cache->features);
    free(cache->nf);
    free(cache);
}

static struct custom_operations custom_cache_ops = {
    "ocaml_pll_cache_t",
    cache_finalize,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default    
};

CAMLprim value
ocaml_create_cache(value vmn, value vdata)
{
    CAMLparam2(vmn, vdata);
    CAMLlocal1(vresult);
    cache_t* cache;
    cache = pll_build_cache(Mn_val(vmn), Data_val(vdata));
    vresult = alloc_custom(&custom_cache_ops, sizeof(cache_t**), 0, 1);
    Cache_val(vresult) = cache;
    CAMLreturn (vresult);
}

/*
 * PLL of a single example
 */
CAMLprim value ocaml_c_pll_mn(value vmn, value vx)
{
    CAMLparam2 (vmn, vx);
    mn_t* mn = Mn_val(vmn);
    int i;

    // Copy x
    int* x = (int*) malloc(sizeof(int) * mn->nvar);
    for (i = 0; i < mn->nvar; i++) { x[i] = Int_val(Field(vx, i)); }

    // Perform computation
    double pll = pll_fa(mn->nvar, mn->schema, mn->nf, mn->features, 
            mn->weights, x);
    CAMLreturn(caml_copy_double(pll));
}


/*
 * PLL value and gradient, for optimization.  No caching.
 */
CAMLprim value ocaml_c_pll_val_and_grad(value vmn, value vdata, 
        value vw, value vg)
{
    CAMLparam4 (vmn, vdata, vw, vg);
    CAMLlocal1 (result);
    mn_t* mn = Mn_val(vmn);
    data_t* data = Data_val(vdata);
    int i;
    double* w = (double*) malloc(sizeof(double) * mn->nf);
    double* g = (double*) malloc(sizeof(double) * mn->nf);
    double pll;

    // Copy weights
    for (i = 0; i < mn->nf; i++) { w[i] = Double_field(vw, i); }

    // Perform computation
    pll = pll_val_and_gradient_mn(mn, data, w, g);

    // Save gradient
    for (i = 0; i < mn->nf; i++) { Store_double_field(vg, i, g[i]); }

    // Return
    result = caml_copy_double(pll);
    CAMLreturn(result);
}


CAMLprim value ocaml_c_pll_val_and_grad_cached(value vmn, value vdata, 
        value vcache, value vw, value vg)
{
    CAMLparam5 (vmn, vdata, vcache, vw, vg);
    CAMLlocal1 (result);
    mn_t* mn = Mn_val(vmn);
    data_t* data = Data_val(vdata);
    cache_t* cache = Cache_val(vcache);
    int i;
    double* w = (double*) malloc(sizeof(double) * mn->nf);
    double* g = (double*) malloc(sizeof(double) * mn->nf);
    double pll;

    // Copy weights
    for (i = 0; i < mn->nf; i++) { w[i] = Double_field(vw, i); }

    // Perform computation
    pll = pll_val_and_gradient_mn_cache(mn, data, cache, w, g);

    // Save gradient
    for (i = 0; i < mn->nf; i++) { Store_double_field(vg, i, g[i]); }

    // Return
    result = caml_copy_double(pll);
    CAMLreturn(result);
}

/*
CAMLprim value ocaml_c_check_cache(value vcache)
{
    CAMLparam1 (vcache);
    cache_t* cache = Cache_val(vcache);
    printf("CHECK: cache->nvar = %d\n", cache->nvar);
    printf("CHECK: cache->ndata = %d\n", cache->ndata);
    printf("CHECK: cache->nf = %p\n", cache->nf);
    CAMLreturn(vcache);
}
*/
