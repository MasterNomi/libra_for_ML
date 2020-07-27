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
#include "pll.h"
#include <stdio.h>

varval check_feature(int* x, varval* f)
{
    varval ret = {SATISFIED, 0};
    int fj;

    // Check to see if var is satisfied on this instance
    for (fj = 0; f[fj].var != END_FEATURE; fj++) {
        int var = f[fj].var;
        int value = f[fj].value;
        // DEBUG
        //printf("fj=%d; f[fj]=(%d, %d)\n", fj, var, value);
        //printf("x[%d] = %d\n", var, x[var]);
        if (x[var] == value
            || (NEGATED(value) && x[var] != NOTVALUE(value))) {
            // DEBUG
            //printf("x satisfies condition.\n");
            // Condition is satisfied.  Nothing to do.
        } else {
            // DEBUG
            //printf("x does not satisfy condition.\n");
            // Condition is not satisfied 
            if (ret.var == SATISFIED || ret.var == var) {
                // First unsatisfied condition
                ret.var = var;
                ret.value = value;
            } else {
                // If there's more than one unsatisfied condition, stop.
                ret.var = UNSATISFIABLE;
                break;
            }
        }
    }

    return ret;
}

// OPT: Should this be inlined via a macro?
// Add weight to a distribution based on the value.
void add_weight_to_dist(int nvals, double* dist, int value, double weight)
{
    if (NEGATED(value)) {
        dist[NOTVALUE(value)] -= weight;
    } else {
        dist[value] += weight;
    }
}

typedef struct _inode {
    int i;
    struct _inode* next;
} inode;

void add_to_list(inode** head, int i)
{
    inode* n = (inode*)malloc(sizeof(inode));
    n->i = i;
    n->next = *head;
    *head = n; 
}

int list_len(inode* head)
{
    int i = 0;
    while (head) {
        i++;
        head = head->next;
    }
    return i;
}

// OPT: Should this be inlined?
double logsumexp(double a, double b)
{
    if (a > b) {
        return a + log(1.0 + exp(b - a));
    } else if (a < b) {
        return b + log(1.0 + exp(a - b));
    } else {
        return a + log(2.0);
    }
}

void normalize_dist(int dim, double* logdist)
{
    double maxval = logdist[0];
    double total = 0.0;
    double logz;
    int i;

    // Get max value, to avoid numerical overflow 
    for (i = 0; i < dim; i++) {
        if (maxval < logdist[i]) {
            maxval = logdist[i];
        }
    }

    // Compute normalization constant
    for (i = 0; i < dim; i++) {
        total += exp(logdist[i] - maxval);
    }
    logz = log(total) + maxval;

    // Normalize
    for (i = 0; i < dim; i++) {
        logdist[i] -= logz;
    }
}

double pll_fa(int nvar, int* schema, 
              int nf, varval** features, double* w, int* x)
{
    double score = 0.0;
    int v, fi;
    double** logdist;

    // Allocate distribution
    logdist = (double**)malloc(sizeof(double*) * nvar);
    for (v = 0; v < nvar; v++) {
        int i;
        logdist[v] = (double*)malloc(sizeof(double) * schema[v]);
        for (i = 0; i < schema[v]; i++) {
            logdist[v][i] = 0.0;
        }
    }

    // Compute all P(X_i|X_-i)
    for (fi = 0; fi < nf; fi++) {
        varval* f = features[fi];
        varval sat = check_feature(x, f);
        if (sat.var == UNSATISFIABLE) {
            // Do nothing -- no single variable change makes a difference.
        } else if (sat.var == SATISFIED) {
            // Add weight or negative weight for each var/value 
            // condition in the feature.
            int fj;
            for (fj = 0; f[fj].var != END_FEATURE; fj++) {
                int var = f[fj].var;
                add_weight_to_dist(schema[var], logdist[var], 
                        f[fj].value, w[fi]);
            }
        } else {
            // Add weight for just the unsatisfied variable.
            int fj;
            for (fj = 0; f[fj].var != END_FEATURE; fj++) {
                if (f[fj].var == sat.var) {
                    add_weight_to_dist(schema[sat.var], logdist[sat.var], 
                            f[fj].value, w[fi]);
                }
            }
        }
    }

    // Add log P(X_i=x_i|X_-i=x_-i) to PLL total
    for (v = 0; v < nvar; v++) {
        normalize_dist(schema[v], logdist[v]);
        score += logdist[v][x[v]];
    }

    // Free distribution
    for (v = 0; v < nvar; v++) {
        free(logdist[v]);
    }
    free(logdist);

    return score;
}

double pll_mn(mn_t* mn, int* x)
{
    return pll_fa(mn->nvar, mn->schema, mn->nf, mn->features, mn->weights, x);
}

double** alloc_dist(int nvar, int* schema)
{
    double** dist;
    int i, j;

    dist = (double**)malloc(sizeof(double*) * nvar);
    for (i = 0; i < nvar; i++) {
        dist[i] = (double*)malloc(sizeof(double) * schema[i]);
        for (j = 0; j < schema[i]; j++) {
            dist[i][j] = 0.0;
        }
    }
    return dist;
}

void free_dist(int nvar, double** dist)
{
    int v;
    for (v = 0; v < nvar; v++) {
        free(dist[v]);
    }
    free(dist);
}

/* countlists contains a list of relevant features for every single
 * variable/value combination twice: once for adding the weight,
 * and once for subtracting the weight. */
inode*** alloc_counts(int nvar, int* schema)
{
    int i, j;
    inode*** countlists = (inode***)malloc(sizeof(inode**) * nvar * 2);;

    for (i = 0; i < nvar; i++) {
        countlists[i] = (inode**)malloc(sizeof(inode*) * schema[i]);
        countlists[i+nvar] = (inode**)malloc(sizeof(inode*) * schema[i]);
        for (j = 0; j < schema[i]; j++) {
            countlists[i][j] = NULL; 
            countlists[i+nvar][j] = NULL; 
        }
    }
    return countlists;
}

void reset_counts(int nvar, int* schema, inode*** countlists)
{
    int i, j;
    for (i = 0; i < 2*nvar; i++) {
        for (j = 0; j < schema[i%nvar]; j++) {
            while (countlists[i][j] != NULL) {
                inode* tmp = countlists[i][j]->next;
                free(countlists[i][j]);
                countlists[i][j] = tmp;
            }
        }
    }
}

void free_counts(int nvar, int* schema, inode*** countlists)
{
    int i;
    reset_counts(nvar, schema, countlists);
    for (i = 0; i < 2*nvar; i++) {
        free(countlists[i]);
    }
    free(countlists);
}


void add_datapoint_to_countlists(mn_t* mn, int* x, inode*** countlists)
{
    int nvar = mn->nvar;
    int fi;
    inode*** negcountlists = countlists + nvar;

    for (fi = 0; fi < mn->nf; fi++) {
        varval* f = mn->features[fi];
        varval sat = check_feature(x, f);
        if (sat.var == UNSATISFIABLE) {
            // Do nothing -- no single variable change makes a difference.
        } else if (sat.var == SATISFIED) {
            // Add weight for every listed variable/value combo
            int fj;
            for (fj = 0; f[fj].var != END_FEATURE; fj++) {
                int var = f[fj].var;
                int value = f[fj].value;
                if (NEGATED(value)) {
                    add_to_list(&negcountlists[var][NOTVALUE(value)], fi);
                } else {
                    add_to_list(&countlists[var][value], fi);
                }
            }
        } else {
            // Add weight for single unsatisfied variable
            int fj;
            for (fj = 0; f[fj].var != END_FEATURE; fj++) {
                int var = f[fj].var;
                int value = f[fj].value;
                if (var != sat.var) {
                    continue;
                }
                if (NEGATED(value)) {
                    add_to_list(&negcountlists[var][NOTVALUE(value)], fi);
                } else {
                    add_to_list(&countlists[var][value], fi);
                }
            }
        }
    }
}


double pll_val_and_gradient_mn(mn_t* mn, data_t* data, 
        double* w, double* g)
{
    int nvar = mn->nvar;
    int* schema = mn->schema;
    double** logdists = alloc_dist(nvar, schema);
    double pll = 0.0;
    int i, j, k, fi;
    inode*** countlists = alloc_counts(nvar, schema);
    inode*** negcountlists = countlists + nvar;
    int* x;

    // Clear gradient array
    for (fi = 0; fi < mn->nf; fi++) {
        g[fi] = 0.0;
    }

    for (i = 0, x = data->points; i < data->n; i++, x += nvar) {
        //int* x = data->points[i];

        // Clear counts lists
        reset_counts(nvar, schema, countlists);

        add_datapoint_to_countlists(mn, x, countlists);

        for (j = 0; j < nvar; j++) {
            inode** counts = countlists[j];
            inode** negcounts = negcountlists[j];
            // Compute distribution over P(X_j) 
            double* logdist = logdists[j];
            double* dist = logdist; // HACK: Overwrite logdist with dist
            for (k = 0; k < schema[j]; k++) {
                logdist[k] = 0.0;
            }
            // Add in positive and negative counts
            for (k = 0; k < schema[j]; k++) {
                inode* pnode;
                for (pnode = counts[k]; pnode; pnode = pnode->next) {
                    logdist[k] += w[pnode->i];
                }
                for (pnode = negcounts[k]; pnode; pnode = pnode->next) {
                    logdist[k] -= w[pnode->i];
                }
            }
            normalize_dist(schema[j], logdist);

            // Add to value of PLL 
            pll += data->weights[i] * logdist[x[j]];
            /* For each relevant feature's gradient, we want to add the 
             * difference between the true counts and the expectation.
             * We do this by subtracting one from the true state probability
             * (and zero from all others). */
            for (k = 0; k < schema[j]; k++) {
                dist[k] = exp(logdist[k]);
            } 
            dist[x[j]] -= 1.0;

            // Add to gradient of PLL 
            for (k = 0; k < schema[j]; k++) {
                double weight = data->weights[i];
                inode* pnode;
                for (pnode = counts[k]; pnode; pnode = pnode->next) {
                    g[pnode->i] -= weight * dist[k];
                }
                for (pnode = negcounts[k]; pnode; pnode = pnode->next) {
                    g[pnode->i] += weight * dist[k];
                }
            }
        }
    }
    free_dist(nvar, logdists);
    free_counts(nvar, schema, countlists);
    return pll;
}

int countlists_to_arrays(int nvar, int* schema, inode*** countlists, 
        int** xfeatures, int* nf_array)
{
    int index = 0;
    int totalfeatures = 0;
    int j, k, l;
    for (j = 0; j < nvar; j++) {
        for (k = 0; k < schema[j]; k++) {
            int nf = list_len(countlists[j][k]);
            int* jkfeatures = (int*)malloc(sizeof(int) * nf);
            inode* pnode = countlists[j][k];
            for (l = 0; l < nf; l++) {
                jkfeatures[l] = pnode->i;
                pnode = pnode->next;
            }
            nf_array[index] = nf;
            xfeatures[index] = jkfeatures;
            index++;
            totalfeatures += nf;
        }
    }
    return totalfeatures;
}

cache_t* pll_build_cache(mn_t* mn, data_t* data)
{
    int nvar = mn->nvar;
    int* schema = mn->schema;
    int i, j, k, l;
    cache_t* cache = (cache_t*)malloc(sizeof(cache_t));
    inode*** countlists = alloc_counts(nvar, schema);
    inode*** negcountlists = countlists + nvar;
    int* x;
    int dim, index, findex, neg_findex; 
    int totalfeatures = 0;
    int neg_totalfeatures = 0;
    int** xfeatures;
    int** neg_xfeatures;

    dim = 0;
    for (j = 0; j < nvar; j++) {
        dim += schema[j];
    }

    // Initialize cache
    cache->nvar = nvar;
    cache->schema = schema;
    cache->ndata = data->n;
    cache->nf = (int*)malloc(sizeof(int) * data->n * dim);
    cache->features = NULL;
    cache->neg_nf = (int*)malloc(sizeof(int) * data->n * dim);
    cache->neg_features = NULL;
    // Store features in a 2D array, which we will flatten at the end.
    xfeatures = (int**)malloc(sizeof(int*) * data->n * dim);
    neg_xfeatures = (int**)malloc(sizeof(int*) * data->n * dim);

    index = 0;
    for (i = 0, x = data->points; i < data->n; i++, x += nvar) {
        //int* x = data->points[i];

        add_datapoint_to_countlists(mn, x, countlists);

        // Convert lists to arrays and save them
        totalfeatures += countlists_to_arrays(nvar, schema, countlists, 
                                     &xfeatures[index], &cache->nf[index]);
        neg_totalfeatures += countlists_to_arrays(nvar, schema, negcountlists, 
                              &neg_xfeatures[index], &cache->neg_nf[index]);
        index += dim;
        reset_counts(nvar, schema, countlists);
    }

    cache->features = (int*)malloc(sizeof(int) * totalfeatures);
    cache->neg_features = (int*)malloc(sizeof(int) * neg_totalfeatures);
    index = 0;
    findex = 0;
    neg_findex = 0;
    for (i = 0; i < data->n; i++) {
        for (j = 0; j < nvar; j++) {
            for (k = 0; k < schema[j%nvar]; k++) {
                for (l = 0; l < cache->nf[index]; l++) {
                    cache->features[findex] = xfeatures[index][l];
                    findex++;
                }
                free(xfeatures[index]);
                // Repeat for subtracted features
                for (l = 0; l < cache->neg_nf[index]; l++) {
                    cache->neg_features[neg_findex] = neg_xfeatures[index][l];
                    neg_findex++;
                }
                free(neg_xfeatures[index]);

                index++;
            }
        }
    }
    free(xfeatures);
    free(neg_xfeatures);

    free_counts(nvar, schema, countlists);
    return cache;
}

double pll_val_and_gradient_mn_cache(mn_t* mn, data_t* data, cache_t* cache,
        double* w, double* g)
{
    int nvar = mn->nvar;
    int* schema = mn->schema;
    double** logdists = alloc_dist(nvar, schema);
    double pll = 0.0;
    int i, j, k, l, fi;
    int *last_pfeature, *pfeature;
    int *last_pnf, *pnf;
    int *last_neg_pfeature, *neg_pfeature;
    int *last_neg_pnf, *neg_pnf;
    int *x;

    // Clear gradient array
    for (fi = 0; fi < mn->nf; fi++) {
        g[fi] = 0.0;
    }

    pfeature = cache->features;
    neg_pfeature = cache->neg_features;
    pnf = &(cache->nf[0]);
    neg_pnf = &(cache->neg_nf[0]);
    for (i = 0, x = data->points; i < data->n; i++, x += nvar) {
        double weight = data->weights[i];
        for (j = 0; j < nvar; j++) {
            // Compute distribution over P(X_j) 
            double* logdist = logdists[j];
            double* dist = logdist; // HACK: Overwrite logdist with dist
            last_pfeature = pfeature;
            last_pnf = pnf;
            for (k = 0; k < schema[j]; k++) {
                logdist[k] = 0.0;
                for (l = 0; l < *pnf; l++) {
                    logdist[k] += w[*pfeature];
                    pfeature++;
                }
                pnf++;
            }

            // Repeat for subtracted features
            last_neg_pfeature = neg_pfeature;
            last_neg_pnf = neg_pnf;
            for (k = 0; k < schema[j]; k++) {
                for (l = 0; l < *neg_pnf; l++) {
                    logdist[k] -= w[*neg_pfeature];
                    neg_pfeature++;
                }
                neg_pnf++;
            }

            normalize_dist(schema[j], logdist);

            // Add to value of PLL 
            pll += data->weights[i] * logdist[x[j]];
            /* For each relevant feature's gradient, we want to add the 
             * difference between the true counts and the expectation.
             * We do this by subtracting one from the true state probability
             * (and zero from all others). */
            for (k = 0; k < schema[j]; k++) {
                dist[k] = exp(logdist[k]);
            } 
            dist[x[j]] -= 1.0;

            // Reset pointers so we can iterate through again
            pfeature = last_pfeature;
            pnf = last_pnf;

            // Add to gradient of PLL 
            for (k = 0; k < schema[j]; k++) {
                for (l = 0; l < *pnf; l++) {
                    g[*pfeature] -= weight * dist[k];
                    pfeature++;
                }
                pnf++;
            }

            // Repeat for subtracted features
            neg_pfeature = last_neg_pfeature;
            neg_pnf = last_neg_pnf;
            for (k = 0; k < schema[j]; k++) {
                for (l = 0; l < *neg_pnf; l++) {
                    g[*neg_pfeature] += weight * dist[k];
                    neg_pfeature++;
                }
                neg_pnf++;
            }
        }
    }
    free_dist(nvar, logdists);
    return pll;
}

