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

/* Supplemental code to compute PLL and its gradient, given a set of examples
 * and weighted features
 */
#ifndef PLL_H
#define PLL_H

#include <math.h>
#include <stdlib.h>

/* Structure representing a variable/value condition.
 * If value >= 0, then this condition is satisfied when 
 *   the variable equals the given value.
 * If value < 0, then this condition is satisfied when
 *   the variable does NOT equal (-value-1).
 */
typedef struct {
    int var;
    int value;
} varval;

/* Structure representing a Markov network as a set of
 * weighted features.
 */
typedef struct 
{
    int nvar;           // Number of variables
    int* schema;        // Array of variable ranges
    int nf;             // Number of features
    varval** features;  // Array of features, each an array of conditions
    double* weights;    // Array of feature weights
} mn_t;

// Set of weighted examples
typedef struct
{
    int n;
    int nvar;
    double* weights;

    /* This 1D array is actually a flattened 2D array.  
     * points[nvar*i + j] is the jth attribute of the ith point. */
    int* points;
} data_t;

// Cached information about features that affect the gradient
typedef struct
{
    int nvar;
    int* schema;
    int ndata;

    /* Number of features relevant for each value of each variable in each
     * example.  This is a 3D array (examples * variables * values) flattened
     * into a 1D array. */
    int* nf;        

    /* List of features relevant for each example/var/value combination.
     * This is a 4D jagged array (examples * variables * value * nf) flattened 
     * into a 1D array. */
    int* features;

    // HACK -- add list of features to subtract.
    int* neg_nf;
    int* neg_features;
} cache_t;

#define SATISFIED -1
#define UNSATISFIABLE -2
#define NOTVALUE(v) (-(v)-1)
#define NEGATED(v) ((v) < 0)
#define END_FEATURE -1

double pll_fa(int nvar, int* schema, int nf, varval** features, 
        double* w, int* x);

double pll_val_and_gradient_mn(mn_t* mn, data_t* data, double* w, double* g);

// Faster, cached version
cache_t* pll_build_cache(mn_t* mn, data_t* data);
double pll_val_and_gradient_mn_cache(mn_t* mn, data_t* data, cache_t* cache,
        double* w, double* g);

#endif // PLL_H
