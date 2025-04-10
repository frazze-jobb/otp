/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 2010-2023. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * %CopyrightEnd%
 */

#include "ec.h"
#include "bn.h"

#ifdef HAVE_EC
# if defined(HAS_3_0_API)

# include <openssl/core_names.h>

int get_curve_definition(ErlNifEnv* env, ERL_NIF_TERM *ret, ERL_NIF_TERM def,
                         OSSL_PARAM params[], int *i,
                         size_t *order_size,
                         struct get_curve_def_ctx* gcd)
{
    const ERL_NIF_TERM* curve;
    int c_arity = -1;
    const ERL_NIF_TERM *prime;
    int p_arity = -1;
    const ERL_NIF_TERM *field;
    int f_arity = -1;
    BIGNUM *p = NULL;
    int arity = -1;
    const ERL_NIF_TERM* curve_tuple;

    /* Here are two random curve definition examples, one prime_field and
       one characteristic_two_field. Both are from the crypto/src/crypto_ec_curves.erl.

        curve(secp192r1) ->
           {
            {prime_field, <<16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFF:192>>}, %% Prime
            {<<16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFC:192>>,               %% A
             <<16#64210519E59C80E70FA7E9AB72243049FEB8DEECC146B9B1:192>>,               %% B
             <<16#3045AE6FC8422F64ED579528D38120EAE12196D5:160>>},                      %% Seed
             <<16#04:8,
               16#188DA80EB03090F67CBF20EB43A18800F4FF0AFD82FF1012:192,                 %% X(p0)
               16#07192B95FFC8DA78631011ED6B24CDD573F977A11E794811:192>>,               %% Y(p0)
             <<16#FFFFFFFFFFFFFFFFFFFFFFFF99DEF836146BC9B1B4D22831:192>>,               %% Order
             <<16#01:8>>                                                                %% CoFactor
           };

        curve(c2pnb176v1) ->
           {
            {characteristic_two_field, 176, {ppbasis,1,2,43}},
            {<<16#E4E6DB2995065C407D9D39B8D0967B96704BA8E9C90B:176>>,                   %% A
             <<16#5DDA470ABE6414DE8EC133AE28E9BBD7FCEC0AE0FFF2:176>>,                   %% B
             none},                                                                     %% Seed
             <<16#04:8,
               16#8D16C2866798B600F9F08BB4A8E860F3298CE04A5798:176,                     %% X(p0)
               16#6FA4539C2DADDDD6BAB5167D61B436E1D92BB16A562C:176>>,                   %% Y(p0)
             <<16#010092537397ECA4F6145799D62B0A19CE06FE26AD:168>>,                     %% Order
             <<16#FF6E:16>>                                                             %% CoFactor
           };
    */

    /* Separate the curve definition from the curve name */
    if (!enif_get_tuple(env, def, &arity, &curve_tuple) || (arity != 2))
        assign_goto(*ret, err, EXCP_ERROR(env, "Tuple arity 2 expected."));

    /* {Field, Prime, Point, Order, CoFactor} = CurveDef */
    if (!enif_get_tuple(env, curve_tuple[0], &c_arity, &curve) ||
        c_arity != 5)
        assign_goto(*ret, err, EXCP_ERROR_N(env, 1, "Bad curve def. Expect 5-tuple."));

    if (gcd->use_curve_name
        && curve_tuple[1] != atom_undefined
        && enif_get_atom(env, curve_tuple[1], gcd->curve_name,
                         sizeof(gcd->curve_name), ERL_NIF_LATIN1)) {
        ErlNifBinary order_bin;
        params[(*i)++] = OSSL_PARAM_construct_utf8_string(OSSL_PKEY_PARAM_GROUP_NAME, gcd->curve_name, 0);

        if (order_size) {
            if (!enif_inspect_binary(env, curve[3], &order_bin))
                assign_goto(*ret, err, EXCP_ERROR_N(env, 1, "Bad order"));
            *order_size = order_bin.size;
        }
        gcd->use_curve_name = 1;
        return 1;
    }
    gcd->use_curve_name = 0;

    if (!get_ossl_octet_string_param_from_bin(env, OSSL_PKEY_PARAM_EC_GENERATOR, curve[2], &params[(*i)++]))
        assign_goto(*ret, err, EXCP_ERROR_N(env, 1, "Bad Generator (Point)"));

    if (!get_ossl_BN_param_from_bin_sz(env, OSSL_PKEY_PARAM_EC_ORDER, curve[3], &params[(*i)++], order_size))
        assign_goto(*ret, err, EXCP_ERROR_N(env, 1, "Bad order"));

    if (curve[4] == atom_none)
        assign_goto(*ret, err, EXCP_ERROR_N(env, 1, "Cofactor must be != none"));

    if (!get_ossl_BN_param_from_bin(env, OSSL_PKEY_PARAM_EC_COFACTOR, curve[4], &params[(*i)++]))
        assign_goto(*ret, err, EXCP_ERROR_N(env, 1, "Bad cofactor"));

    /* {A, B, Seed} = Prime = curve[1] */
    if (!enif_get_tuple(env, curve[1], &p_arity, &prime))
        assign_goto(*ret, err, EXCP_ERROR_N(env, 1, "Bad Prime"));

    if (!get_ossl_BN_param_from_bin(env, OSSL_PKEY_PARAM_EC_A, prime[0], &params[(*i)++]))
        assign_goto(*ret, err, EXCP_ERROR_N(env, 1, "Bad a"));

    if (!get_ossl_BN_param_from_bin(env, OSSL_PKEY_PARAM_EC_B, prime[1], &params[(*i)++]))
        assign_goto(*ret, err, EXCP_ERROR_N(env, 1, "Bad b"));

    if (enif_is_binary(env, prime[2]))
        if (!get_ossl_octet_string_param_from_bin(env, OSSL_PKEY_PARAM_EC_SEED, prime[2], &params[(*i)++]))
            assign_goto(*ret, err, EXCP_ERROR_N(env, 1, "Bad seed"));

    /* Field = curve[0] */
    if (!enif_get_tuple(env, curve[0], &f_arity, &field)) {
        assign_goto(*ret, err, EXCP_ERROR_N(env, 1, "Bad Field"));
    }
    else if (f_arity == 2 && field[0] == atom_prime_field) {
        /* {prime_field, Prime} */
        params[(*i)++] = OSSL_PARAM_construct_utf8_string(OSSL_PKEY_PARAM_EC_FIELD_TYPE,  "prime-field", 0);

        if (!get_ossl_BN_param_from_bin(env, OSSL_PKEY_PARAM_EC_P, field[1], &params[(*i)++]))
            assign_goto(*ret, err, EXCP_ERROR_N(env, 1, "Bad p (Prime)"));
    }

    else if (f_arity == 3 && field[0] == atom_characteristic_two_field) {
        /* {characteristic_two_field, M, Basis} */
#  if defined(OPENSSL_NO_EC2M)
        assign_goto(*ret, err, EXCP_NOTSUP_N(env, 1, "Unsupported field-type (characteristic_two_field)"));
#  else
        int b_arity = -1;
        const ERL_NIF_TERM* basis;
        long field_bits;

        params[(*i)++] = OSSL_PARAM_construct_utf8_string(OSSL_PKEY_PARAM_EC_FIELD_TYPE, "characteristic-two-field", 0);

        if ((p = BN_new()) == NULL)
            assign_goto(*ret, err, EXCP_ERROR(env, "Creating bignum failed"));

        if (!enif_get_long(env, field[1], &field_bits) ||
            (field_bits > OPENSSL_ECC_MAX_FIELD_BITS || field_bits > INT_MAX)
            )
            assign_goto(*ret, err, EXCP_ERROR_N(env, 1, "Bad field-bits (M)"));

        if (enif_get_tuple(env, field[2], &b_arity, &basis)) {
            if (b_arity == 2) {
                unsigned int k1;

                if (basis[0] != atom_tpbasis)
                    assign_goto(*ret, err, EXCP_ERROR_N(env, 1, "Bad atom"));
                if (!enif_get_uint(env, basis[1], &k1))
                    assign_goto(*ret, err, EXCP_ERROR_N(env, 1, "uint expected (k1)"));

                /* {tpbasis, k} = Basis */
                if (field_bits <= k1 || k1 == 0 || k1 > INT_MAX)
                    assign_goto(*ret, err, EXCP_ERROR_N(env, 1, "bad values (field_bits or k1)"));

                /* create the polynomial */
                if (!BN_set_bit(p, (int)field_bits) ||
                    !BN_set_bit(p, (int)k1) ||
                    !BN_set_bit(p, 0))
                    assign_goto(*ret, err, EXCP_ERROR(env, "Polynom bit setting failed"));

            } else if (b_arity == 4) {
                /* {ppbasis, k1, k2, k3} = Basis */
                unsigned int k1, k2, k3;

                if (basis[0] != atom_ppbasis)
                    assign_goto(*ret, err, EXCP_ERROR_N(env, 1, "Bad atom"));

                if (!enif_get_uint(env, basis[1], &k1) ||
                    !enif_get_uint(env, basis[2], &k2) ||
                    !enif_get_uint(env, basis[3], &k3))
                    assign_goto(*ret, err, EXCP_ERROR_N(env, 1, "Expecting uint (k1,k2,k3)"));

                if (field_bits <= k3 || k3 <= k2 || k2 <= k1 || k1 == 0 ||
                    k3 > INT_MAX || k2 > INT_MAX || k1 > INT_MAX)
                    assign_goto(*ret, err, EXCP_ERROR_N(env, 1, "bad values (field_bits, k1, k2 or k3)"));

                /* create the polynomial */
                if (!BN_set_bit(p, (int)field_bits) ||
                    !BN_set_bit(p, (int)k1) ||
                    !BN_set_bit(p, (int)k2) ||
                    !BN_set_bit(p, (int)k3) ||
                    !BN_set_bit(p, 0) )
                    assign_goto(*ret, err, EXCP_ERROR(env, "Polynom bit setting failed"));

            } else
                assign_goto(*ret, err, EXCP_ERROR_N(env, 1, "Bad tuple"));

        } else if (field[2] == atom_onbasis) {
            /* onbasis = Basis */
            /* no parameters */
            assign_goto(*ret, err, EXCP_NOTSUP_N(env, 1, "'onbasis' not supported"));
        } else
            assign_goto(*ret, err, EXCP_ERROR_N(env, 1, "Bad last field"));

        if (!get_ossl_BN_param_from_bn(env, OSSL_PKEY_PARAM_EC_P, p, &params[(*i)++]))
            assign_goto(*ret, err, EXCP_ERROR_N(env, 1, "BN padding failed"));
#  endif
    }
    else
        assign_goto(*ret, err, EXCP_ERROR_N(env, 1, "Bad field-type"));

    if (p) BN_free(p);
    return 1;

 err:
    if (p) BN_free(p);
    return 0;
}

int get_ec_public_key(ErlNifEnv* env, ERL_NIF_TERM key, EVP_PKEY **pkey)
{ /* key :: {CurveDef::{_,_,_,_,_}, PubKey::binary()} */
    ERL_NIF_TERM ret = atom_undefined;
    const ERL_NIF_TERM *tpl_terms;
    int tpl_arity;
    int i = 0;
    OSSL_PARAM params[15];
    struct get_curve_def_ctx gcd;
    EVP_PKEY_CTX *pctx = NULL;

    if (!enif_get_tuple(env, key, &tpl_arity, &tpl_terms) ||
        (tpl_arity != 2) ||
        !enif_is_tuple(env, tpl_terms[0]) ||
        !enif_is_binary(env, tpl_terms[1]) )
        assign_goto(ret, err, EXCP_BADARG_N(env, 0, "Bad public key format"));

    if (!get_ossl_octet_string_param_from_bin(env, OSSL_PKEY_PARAM_PUB_KEY,  tpl_terms[1], &params[i++]))
        assign_goto(ret, err, EXCP_BADARG_N(env, 0, "Bad public key"));

    if (!(pctx = EVP_PKEY_CTX_new_from_name(NULL, "EC", NULL)))
        assign_goto(ret, err, EXCP_ERROR(env, "Can't make EVP_PKEY_CTX"));

    gcd.use_curve_name = 1;
retry_without_name:
    if (!get_curve_definition(env, &ret, tpl_terms[0], params, &i, NULL, &gcd))
        goto err;

    params[i++] = OSSL_PARAM_construct_end();

    if (EVP_PKEY_fromdata_init(pctx) <= 0)
        assign_goto(ret, err, EXCP_ERROR(env, "Can't init fromdata"));

    if (EVP_PKEY_fromdata(pctx, pkey, EVP_PKEY_PUBLIC_KEY, params) <= 0) {
        if (gcd.use_curve_name) {
            gcd.use_curve_name = 0;
            i = 1;
            goto retry_without_name;
        }
        assign_goto(ret, err, EXCP_ERROR(env, "Can't do fromdata"));
    }

    if (!*pkey)
        assign_goto(ret, err, EXCP_ERROR(env, "Couldn't get a public key"));

    if (pctx) EVP_PKEY_CTX_free(pctx);
    return 1;

 err:
    if (pctx) EVP_PKEY_CTX_free(pctx);
    return 0;
}


static int get_ec_private_key_2(ErlNifEnv* env,
                         ERL_NIF_TERM curve, ERL_NIF_TERM key,
                         EVP_PKEY **pkey,
                         ERL_NIF_TERM *ret,
                         size_t *order_size)
{
    int i = 0;
    OSSL_PARAM params[15];
    struct get_curve_def_ctx gcd;
    EVP_PKEY_CTX *pctx = NULL;

    if (!get_ossl_BN_param_from_bin(env, OSSL_PKEY_PARAM_PRIV_KEY,  key, &params[i++]))
        assign_goto(*ret, err, EXCP_BADARG_N(env, 0, "Bad private key"));

    if (!(pctx = EVP_PKEY_CTX_new_from_name(NULL, "EC", NULL)))
        assign_goto(*ret, err, EXCP_ERROR(env, "Can't make EVP_PKEY_CTX"));

    gcd.use_curve_name = 1;
retry_without_name:
    if (!get_curve_definition(env, ret, curve, params, &i, order_size, &gcd))
        goto err;

    params[i++] = OSSL_PARAM_construct_end();

    if (EVP_PKEY_fromdata_init(pctx) <= 0)
        assign_goto(*ret, err, EXCP_ERROR(env, "Can't init fromdata"));

    if (EVP_PKEY_fromdata(pctx, pkey, EVP_PKEY_KEYPAIR, params) <= 0) {
        if (gcd.use_curve_name) {
            gcd.use_curve_name = 0;
            i = 1;
            goto retry_without_name;
        }
        assign_goto(*ret, err, EXCP_ERROR(env, "Can't do fromdata"));
    }

    if (!*pkey)
        assign_goto(*ret, err, EXCP_ERROR(env, "Couldn't get a private key"));

    if (pctx) EVP_PKEY_CTX_free(pctx);
    return 1;

 err:
    if (pctx) EVP_PKEY_CTX_free(pctx);
    return 0;
}


int get_ec_private_key(ErlNifEnv* env, ERL_NIF_TERM key, EVP_PKEY **pkey)
{  /* key ::  {CurveDef::{_,_,_,_,_}, PrivKey::binary()}  */
    ERL_NIF_TERM ret = atom_undefined;
    const ERL_NIF_TERM *tpl_terms;
    int tpl_arity;

    if (!enif_get_tuple(env, key, &tpl_arity, &tpl_terms) ||
        (tpl_arity != 2) ||
        !enif_is_tuple(env, tpl_terms[0]) ||
        !enif_is_binary(env, tpl_terms[1]) )
        assign_goto(ret, err, EXCP_BADARG_N(env, 0, "Bad private key format"));

    if (!get_ec_private_key_2(env, tpl_terms[0], tpl_terms[1], pkey, &ret, NULL))
        goto err;

    return 1;

 err:
    return 0;
}

static int mk_pub_key_binary(ErlNifEnv* env, EVP_PKEY *pkey,
                             point_conversion_form_t point_form,
                             ErlNifBinary *pubkey_bin, ERL_NIF_TERM *ret);

ERL_NIF_TERM ec_generate_key_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{ /* (Curve :: {CurveDef, CurveName}, PrivKey :: binary() | undefined |
                                                 {binary() | undefined,
                                                  Format :: compressed | uncompressed}) */
    ERL_NIF_TERM ret = atom_undefined;
    int i = 0;
    OSSL_PARAM params[15];
    struct get_curve_def_ctx gcd;
    EVP_PKEY_CTX *pctx = NULL;
    EVP_PKEY *pkey = NULL;
    size_t order_size = 0;
    BIGNUM *priv_bn = NULL;
    ErlNifBinary pubkey_bin = {0};
    point_conversion_form_t point_form = POINT_CONVERSION_UNCOMPRESSED; // Default to uncompressed
    ERL_NIF_TERM priv_key = atom_undefined;
    const ERL_NIF_TERM *priv_tuple;
    int priv_tuple_arity = 0;

    // Expect 2 arguments: Curve, PrivKey|undefined
    if (argc != 2) {
        return EXCP_BADARG_N(env, 0, "Expected 2 arguments: Curve, PrivKey");
    }

    // Check if second argument is a tuple containing private key and format
    if (enif_is_tuple(env, argv[1]) &&
        enif_get_tuple(env, argv[1], &priv_tuple_arity, &priv_tuple) &&
        priv_tuple_arity == 2) {

        // Get private key (first element of tuple)
        priv_key = priv_tuple[0];

        // Get point format (second element of tuple)
        if (enif_compare(priv_tuple[1], atom_compressed) == 0) {
            point_form = POINT_CONVERSION_COMPRESSED;
        } else if (enif_compare(priv_tuple[1], atom_uncompressed) == 0) {
            point_form = POINT_CONVERSION_UNCOMPRESSED;
        } else {
            assign_goto(ret, err, EXCP_BADARG_N(env, 1, "Format must be 'compressed' or 'uncompressed'"));
        }
    } else {
        // If not a tuple, assume it's just a private key with default uncompressed format
        priv_key = argv[1];
    }

    if (priv_key != atom_undefined)
        {
            /* Key pair is derived from the provided private key */
            if (!get_ec_private_key_2(env, argv[0], argv[1], &pkey, &ret, &order_size))
                goto err; // Error term already set in ret by get_ec_private_key_2
        }
    else
        {
            /* Neither the private nor the public key is known, so we generate the pair: */
            if (!(pctx = EVP_PKEY_CTX_new_from_name(NULL, "EC", NULL)))
                assign_goto(ret, err, EXCP_ERROR(env, "Can't EVP_PKEY_CTX_new_from_name"));

            gcd.use_curve_name = 1;
    retry_without_name:
            /* PrivKey (that is, argv[1]) == atom_undefined */
            i = 0;
            if (!get_curve_definition(env, &ret, argv[0], params, &i,
                                      &order_size, &gcd))
                // INSERT "ret" parameter in get_curve_definition !!
                assign_goto(ret, err, EXCP_BADARG_N(env, 0, "Couldn't get Curve definition"));

            params[i++] = OSSL_PARAM_construct_end();

            if (EVP_PKEY_keygen_init(pctx) <= 0)
                assign_goto(ret, err, EXCP_ERROR(env, "Can't EVP_PKEY_keygen_init"));

            if (!EVP_PKEY_CTX_set_params(pctx, params))
                assign_goto(ret, err, EXCP_ERROR(env, "Can't EVP_PKEY_CTX_set_params"));

            if (!EVP_PKEY_generate(pctx, &pkey)) {
                if (gcd.use_curve_name) {
                    gcd.use_curve_name = 0;
                    i = 0;
                    // Need to free pctx and recreate it to reset internal state before retrying
                    if (pctx) EVP_PKEY_CTX_free(pctx); pctx = NULL; // Clean up old ctx
                    if (pkey) EVP_PKEY_free(pkey); pkey = NULL; // Clean up potentially partial key
                    if (!(pctx = EVP_PKEY_CTX_new_from_name(NULL, "EC", NULL)))
                        assign_goto(ret, err, EXCP_ERROR(env, "Can't EVP_PKEY_CTX_new_from_name on retry"));
                    goto retry_without_name; // Retry getting curve def and generating
                }
                assign_goto(ret, err, EXCP_ERROR(env, "Couldn't generate EC key"));
            }
            // pkey now holds the generated key pair
        }
    /* Now, pkey holds the valid key pair (either loaded or generated) */
    /* Get the public key binary using the determined point_form */
    if (!mk_pub_key_binary(env, pkey, point_form, &pubkey_bin, &ret)) {
        // ret is already set by mk_pub_key_binary on error
        goto err;
    }

    /* Get the private key as BN */
    if (!EVP_PKEY_get_bn_param(pkey, OSSL_PKEY_PARAM_PRIV_KEY, &priv_bn)) {
        // Need to release the allocated pubkey_bin if we fail here
        if (pubkey_bin.data) enif_release_binary(&pubkey_bin);
        assign_goto(ret, err, EXCP_BADARG_N(env, 1, "Couldn't get priv key BN"));
    }
    /* If order_size wasn't determined via named curve or explicit params, estimate from BN */
    /* Note: get_ec_private_key_2 and get_curve_definition attempt to set order_size */
    if (order_size == 0 && priv_bn != NULL) {
        // This is a fallback, usually order_size should be known from the curve def
        order_size = BN_num_bytes(priv_bn);
         // A better fallback might be EVP_PKEY_get_int_param(pkey, OSSL_PKEY_PARAM_EC_ORDER_BITS, &bits) / 8
    }

    /* Construct the return tuple: {PublicKeyBinary, PrivateKeyBinary} */
    ret = enif_make_tuple2(env,
                           enif_make_binary(env, &pubkey_bin),
                           bn2term(env, order_size, priv_bn));
    /* Need to prevent double free/release of pubkey_bin. */
    /* enif_make_binary takes ownership if it succeeds. We shouldn't release it manually after this. */
    pubkey_bin.data = NULL; // Mark as consumed by enif_make_binary

 err:
    if (pkey) EVP_PKEY_free(pkey);
    if (pctx) EVP_PKEY_CTX_free(pctx);
    if (priv_bn) BN_free(priv_bn);
    // Release binary only if it wasn't successfully consumed by enif_make_binary
    if (pubkey_bin.data != NULL) enif_release_binary(&pubkey_bin);
    return ret;
}

static int mk_pub_key_binary(ErlNifEnv* env, EVP_PKEY *pkey,
                             point_conversion_form_t point_form,
                             ErlNifBinary *pubkey_bin, ERL_NIF_TERM *ret)
{
    size_t pub_key_size = 0;
    size_t group_name_size = 0;
    char group_name_buf[20];
    char* group_name = group_name_buf;
    int group_nid;
    EC_GROUP* ec_group = NULL;
    EC_POINT* pub_key = NULL;
    BIGNUM* priv_bn = NULL;
    int ok = 0;
    EVP_PKEY_CTX *ctx = NULL; // Needed for parameter extraction
    int allocated_group_name = 0;

    /* This code was inspired by
     * https://github.com/openssl/openssl/issues/18437
     * which first tried to get public key directly with
     * EVP_PKEY_get_octet_string_param(peer_pkey, OSSL_PKEY_PARAM_PUB_KEY,..)
     *
     * I removed that since I don't know what key format that will produce
     * if it succeeds. That is, we go directly to the "fallback" and calculate
     * the public key from the private key and group parameters.
     * This approach also gives us control over the compression format.
     */

    // Create a context for the key to extract parameters
    ctx = EVP_PKEY_CTX_new_from_pkey(NULL, pkey, NULL);
    if (ctx == NULL)
        assign_goto(*ret, err, EXCP_ERROR(env, "Couldn't create PKEY_CTX"));

    // Get group name size
    if (!EVP_PKEY_get_utf8_string_param(pkey, OSSL_PKEY_PARAM_GROUP_NAME,
                                        NULL, 0, &group_name_size)) {
        assign_goto(*ret, err, EXCP_ERROR(env, "Couldn't get EC group name size"));
    }

    if (group_name_size == 0) // Should not happen if the previous call succeeded, but check anyway
         assign_goto(*ret, err, EXCP_ERROR(env, "EC group name size is zero"));

    if (group_name_size >= sizeof(group_name_buf)) {

        group_name = enif_alloc(group_name_size + 1);
        if (!group_name)
             assign_goto(*ret, err, EXCP_ERROR(env, "Couldn't allocate memory for group name"));
        allocated_group_name = 1;
    }

     // Get group name string
    if (!EVP_PKEY_get_utf8_string_param(pkey, OSSL_PKEY_PARAM_GROUP_NAME,
                                        group_name, group_name_size + 1, // Use actual size + null terminator space
                                        NULL))
        assign_goto(*ret, err, EXCP_ERROR(env, "Couldn't get EC group name"));

    group_nid = OBJ_txt2nid(group_name);
    if (group_nid == NID_undef)
        assign_goto(*ret, err, EXCP_ERROR(env, "Couldn't get EC group nid from name"));

    ec_group = EC_GROUP_new_by_curve_name(group_nid);
    if (ec_group == NULL)
        assign_goto(*ret, err, EXCP_ERROR(env, "Couldn't get EC_GROUP by curve NID"));

    pub_key = EC_POINT_new(ec_group);
    if (pub_key == NULL)
        assign_goto(*ret, err, EXCP_ERROR(env, "Couldn't create POINT"));

    // Get the private key BIGNUM from the EVP_PKEY
    if (!EVP_PKEY_get_bn_param(pkey, OSSL_PKEY_PARAM_PRIV_KEY, &priv_bn))
        assign_goto(*ret, err, EXCP_BADARG_N(env, 1, "Couldn't get private key BN from EVP_PKEY"));

    if (!EC_POINT_mul(ec_group, pub_key, priv_bn, NULL, NULL, NULL))
        assign_goto(*ret, err, EXCP_ERROR(env, "Couldn't multiply POINT"));

    pub_key_size = EC_POINT_point2oct(ec_group, pub_key,
                                      point_form, // Use the passed format
                                      NULL, 0, NULL);
    if (pub_key_size == 0)
        assign_goto(*ret, err, EXCP_ERROR(env, "Couldn't get pub_key_size"));

    // Allocate the Erlang binary
    if (!enif_alloc_binary(pub_key_size, pubkey_bin))
         assign_goto(*ret, err, EXCP_ERROR(env, "Couldn't allocate pub key binary"));

    // Convert the public key point to an octet string in the requested format
    if (EC_POINT_point2oct(ec_group, pub_key, point_form, // Use the passed format
                            pubkey_bin->data,
                            pubkey_bin->size, NULL) != pub_key_size) { // Check return value matches expected size
        enif_release_binary(pubkey_bin); // Release the binary if conversion failed
        assign_goto(*ret, err, EXCP_ERROR(env, "Couldn't convert EC_POINT to octet string"));
    }

    *ret = enif_make_binary(env, pubkey_bin);
    ok = 1;

err:
    if (allocated_group_name && group_name) enif_free(group_name);
    if (pub_key) EC_POINT_free(pub_key);
    if (ec_group) EC_GROUP_free(ec_group);
    if (priv_bn) BN_free(priv_bn);
    if (ctx) EVP_PKEY_CTX_free(ctx);

    return ok;
}
    
# endif /* HAS_3_0_API */




/*----------------------------------------------------------------
  Non 3.0-specific functions
*/

# if ! defined(HAS_3_0_API)

static EC_KEY* ec_key_new(ErlNifEnv* env, ERL_NIF_TERM curve_arg, size_t *size)
{
    EC_KEY *key = NULL;
    int c_arity = -1;
    const ERL_NIF_TERM* curve;
    ErlNifBinary seed;
    BIGNUM *p = NULL;
    BIGNUM *a = NULL;
    BIGNUM *b = NULL;
    BIGNUM *bn_order = NULL;
    BIGNUM *cofactor = NULL;
    EC_GROUP *group = NULL;
    EC_POINT *point = NULL;
    int f_arity = -1;
    const ERL_NIF_TERM *field;
    int p_arity = -1;
    const ERL_NIF_TERM *prime;
    long field_bits;

    /* {Field, Prime, Point, Order, CoFactor} = Curve */
    if (!enif_get_tuple(env, curve_arg, &c_arity, &curve))
        goto err;
    if (c_arity != 5)
        goto err;
    if (!get_bn_from_bin_sz(env, curve[3], &bn_order, size))
        goto err;
    if (curve[4] != atom_none) {
        if (!get_bn_from_bin(env, curve[4], &cofactor))
            goto err;
    }

    /* {A, B, Seed} = Prime */
    if (!enif_get_tuple(env, curve[1], &p_arity, &prime))
        goto err;
    if (!get_bn_from_bin(env, prime[0], &a))
        goto err;
    if (!get_bn_from_bin(env, prime[1], &b))
        goto err;

    if (!enif_get_tuple(env, curve[0], &f_arity, &field))
        goto err;

    if (f_arity == 2 && field[0] == atom_prime_field) {
        /* {prime_field, Prime} */
        if (!get_bn_from_bin(env, field[1], &p))
            goto err;
        if (BN_is_negative(p))
            goto err;
        if (BN_is_zero(p))
            goto err;

        field_bits = BN_num_bits(p);
        if (field_bits > OPENSSL_ECC_MAX_FIELD_BITS)
            goto err;

        /* create the EC_GROUP structure */
        if ((group = EC_GROUP_new_curve_GFp(p, a, b, NULL)) == NULL)
            goto err;

    } else if (f_arity == 3 && field[0] == atom_characteristic_two_field) {
#if defined(OPENSSL_NO_EC2M)
        enif_raise_exception(env, atom_notsup);
        goto err;
#else
        /* {characteristic_two_field, M, Basis} */
        int b_arity = -1;
        const ERL_NIF_TERM* basis;

        if ((p = BN_new()) == NULL)
            goto err;
        if (!enif_get_long(env, field[1], &field_bits))
            goto err;
        if (field_bits > OPENSSL_ECC_MAX_FIELD_BITS || field_bits > INT_MAX)
            goto err;

        if (enif_get_tuple(env, field[2], &b_arity, &basis)) {
            if (b_arity == 2) {
                unsigned int k1;

                if (basis[0] != atom_tpbasis)
                    goto err;
                if (!enif_get_uint(env, basis[1], &k1))
                    goto err;

                /* {tpbasis, k} = Basis */
                if (field_bits <= k1 || k1 == 0 || k1 > INT_MAX)
                    goto err;

                /* create the polynomial */
                if (!BN_set_bit(p, (int)field_bits))
                    goto err;
                if (!BN_set_bit(p, (int)k1))
                    goto err;
                if (!BN_set_bit(p, 0))
                    goto err;

            } else if (b_arity == 4) {
                unsigned int k1, k2, k3;

                if (basis[0] != atom_ppbasis)
                    goto err;
                if (!enif_get_uint(env, basis[1], &k1))
                    goto err;
                if (!enif_get_uint(env, basis[2], &k2))
                    goto err;
                if (!enif_get_uint(env, basis[3], &k3))
                    goto err;

                /* {ppbasis, k1, k2, k3} = Basis */
                if (field_bits <= k3 || k3 <= k2 || k2 <= k1 || k1 == 0 || k3 > INT_MAX || k2 > INT_MAX || k1 > INT_MAX)
                    goto err;

                /* create the polynomial */
                if (!BN_set_bit(p, (int)field_bits))
                    goto err;
                if (!BN_set_bit(p, (int)k1))
                    goto err;
                if (!BN_set_bit(p, (int)k2))
                    goto err;
                if (!BN_set_bit(p, (int)k3))
                    goto err;
                if (!BN_set_bit(p, 0))
                    goto err;

            } else
                goto err;
        } else if (field[2] == atom_onbasis) {
            /* onbasis = Basis */
            /* no parameters */
            goto err;

        } else
            goto err;

        if ((group = EC_GROUP_new_curve_GF2m(p, a, b, NULL)) == NULL)
            goto err;
#endif
    } else
        goto err;

    if (enif_inspect_binary(env, prime[2], &seed)) {
        if (!EC_GROUP_set_seed(group, seed.data, seed.size))
            goto err;
    }

    if (!term2point(env, curve[2], group, &point))
        goto err;

    if (BN_is_negative(bn_order))
        goto err;
    if (BN_is_zero(bn_order))
        goto err;
    if (BN_num_bits(bn_order) > (int)field_bits + 1)
        goto err;

    if (!EC_GROUP_set_generator(group, point, bn_order, cofactor))
        goto err;

    EC_GROUP_set_asn1_flag(group, 0x0);

    if ((key = EC_KEY_new()) == NULL)
        goto err;

    if (!EC_KEY_set_group(key, group))
        goto err;

    goto done;

 err:
    if (key)
        EC_KEY_free(key);
    key = NULL;

 done:
    /* some OpenSSL structures are mem-dup'ed into the key,
       so we have to free our copies here */
    if (bn_order)
        BN_free(bn_order);
    if (cofactor)
        BN_free(cofactor);
    if (a)
        BN_free(a);
    if (b)
        BN_free(b);
    if (p)
        BN_free(p);
    if (group)
        EC_GROUP_free(group);
    if (point)
        EC_POINT_free(point);

    return key;
}

int term2point(ErlNifEnv* env, ERL_NIF_TERM term, EC_GROUP *group, EC_POINT **pptr)
{
    ErlNifBinary bin;
    EC_POINT *point = NULL;

    if (!enif_inspect_binary(env, term, &bin))
        goto err;

    if ((point = EC_POINT_new(group)) == NULL)
        goto err;

    /* set the point conversion form */
    EC_GROUP_set_point_conversion_form(group, (point_conversion_form_t)(bin.data[0] & ~0x01));

    /* extract the ec point */
    if (!EC_POINT_oct2point(group, point, bin.data, bin.size, NULL))
        goto err;

    *pptr = point;
    return 1;

 err:
    if (point)
        EC_POINT_free(point);
    return 0;
}

static ERL_NIF_TERM point2term(ErlNifEnv* env,
			       const EC_GROUP *group,
			       const EC_POINT *point,
			       point_conversion_form_t form)
{
    ERL_NIF_TERM ret;
    size_t dlen;
    ErlNifBinary bin;
    int bin_alloc = 0;

    if ((dlen = EC_POINT_point2oct(group, point, form, NULL, 0, NULL)) == 0)
	return atom_undefined;

    if (!enif_alloc_binary(dlen, &bin))
        goto err;
    bin_alloc = 1;

    if (!EC_POINT_point2oct(group, point, form, bin.data, bin.size, NULL))
        goto err;

    ERL_VALGRIND_MAKE_MEM_DEFINED(bin.data, bin.size);

    ret = enif_make_binary(env, &bin);
    bin_alloc = 0;
    goto done;

 err:
    if (bin_alloc)
        enif_release_binary(&bin);
    ret = enif_make_badarg(env);

 done:
    return ret;
}

int get_ec_private_key(ErlNifEnv* env, ERL_NIF_TERM key, EVP_PKEY **pkey)
{
    const ERL_NIF_TERM *tpl_terms;
    int tpl_arity;
    EC_KEY *ec = NULL;

    if (!enif_get_tuple(env, key, &tpl_arity, &tpl_terms))
        goto err;
    if (tpl_arity != 2)
        goto err;
    if (!enif_is_tuple(env, tpl_terms[0]))
        goto err;
    if (!enif_is_binary(env, tpl_terms[1]))
        goto err;
    if (!get_ec_key_sz(env, tpl_terms[0], tpl_terms[1], atom_undefined, &ec, NULL))
        goto err;

    *pkey = EVP_PKEY_new();
    if (EVP_PKEY_assign_EC_KEY(*pkey, ec) != 1)
        goto err;
            /* On success, result owns ec */
    ec = NULL;
    return 1;

 err:
    if (ec)
        EC_KEY_free(ec);
    return 0;
}

int get_ec_public_key(ErlNifEnv* env, ERL_NIF_TERM key, EVP_PKEY **pkey)
{
    const ERL_NIF_TERM *tpl_terms;
    int tpl_arity;
    EC_KEY *ec = NULL;

    if (!enif_get_tuple(env, key, &tpl_arity, &tpl_terms))
        goto err;
    if (tpl_arity != 2)
        goto err;
    if (!enif_is_tuple(env, tpl_terms[0]))
        goto err;
    if (!enif_is_binary(env, tpl_terms[1]))
        goto err;
    if (!get_ec_key_sz(env, tpl_terms[0], atom_undefined, tpl_terms[1], &ec, NULL))
        goto err;

    *pkey = EVP_PKEY_new();
    if (EVP_PKEY_assign_EC_KEY(*pkey, ec) != 1)
        goto err;
            /* On success, result owns ec */
    ec = NULL;
    return 1;

 err:
    if (ec)
        EC_KEY_free(ec);
    return 0;
}


int get_ec_key_sz(ErlNifEnv* env,
                  ERL_NIF_TERM curve, ERL_NIF_TERM priv, ERL_NIF_TERM pub,
                  EC_KEY** res,
                  size_t* size)
{
    EC_KEY *key = NULL;
    BIGNUM *priv_key = NULL;
    EC_POINT *pub_key = NULL;
    EC_GROUP *group = NULL;
    int arity = -1;
    const ERL_NIF_TERM* curve_tuple;

    if (priv != atom_undefined) {
        if (!get_bn_from_bin(env, priv, &priv_key))
            goto err;
    }
    if (pub != atom_undefined) {
        if (!enif_is_binary(env, pub))
            goto err;
    }

    if (!enif_get_tuple(env, curve, &arity, &curve_tuple) || (arity != 2))
        goto err;

    if ((key = ec_key_new(env, curve_tuple[0], size)) == NULL)
        goto err;

    if ((group = EC_GROUP_dup(EC_KEY_get0_group(key))) == NULL)
        goto err;

    if (term2point(env, pub, group, &pub_key)) {
        if (!EC_KEY_set_public_key(key, pub_key))
            goto err;
    }

    if (priv != atom_undefined && !BN_is_zero(priv_key)) {
        if (!EC_KEY_set_private_key(key, priv_key))
            goto err;

        /* calculate public key (if necessary) */
        if (EC_KEY_get0_public_key(key) == NULL) {
            /* the public key was not included in the SEC1 private
             * key => calculate the public key */
            if ((pub_key = EC_POINT_new(group)) == NULL)
                goto err;
            if (!EC_POINT_copy(pub_key, EC_GROUP_get0_generator(group)))
                goto err;
            if (!EC_POINT_mul(group, pub_key, priv_key, NULL, NULL, NULL))
                goto err;
            if (!EC_KEY_set_public_key(key, pub_key))
                goto err;
        }
    }
    goto done;

 err:
    if (key)
        EC_KEY_free(key);
    key = NULL;

 done:
    /* some OpenSSL structures are mem-dup'ed into the key,
       so we have to free our copies here */
    if (priv_key)
        BN_clear_free(priv_key);
    if (group)
        EC_GROUP_free(group);
    if (pub_key)
        EC_POINT_free(pub_key);

    if (key == NULL)
        return 0;

    *res = key;
    return 1;
}

ERL_NIF_TERM ec_generate_key_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{ /* (Curve, PrivKey)  */
    EC_KEY *key = NULL;
    const EC_GROUP *group;
    const EC_POINT *public_key;
    ERL_NIF_TERM priv_key;
    ERL_NIF_TERM pub_key;
    ERL_NIF_TERM ret;
    size_t size;

    if (!get_ec_key_sz(env, argv[0], argv[1], atom_undefined, &key, &size))
        assign_goto(ret, err, EXCP_BADARG_N(env, 1, "Couldn't get EC key"));

    if (argv[1] == atom_undefined) {
	if (!EC_KEY_generate_key(key))
            assign_goto(ret, err, EXCP_ERROR(env, "Couldn't generate EC key"));
    }

    group = EC_KEY_get0_group(key);
    public_key = EC_KEY_get0_public_key(key);

    if (group == NULL || public_key == NULL) {
        pub_key = atom_undefined;

    } else {
        pub_key = point2term(env, group, public_key,
                             EC_KEY_get_conv_form(key));
    }

    priv_key = bn2term(env, size, EC_KEY_get0_private_key(key));
    ret = enif_make_tuple2(env, pub_key, priv_key);
    goto done;

 err:
 done:
    if (key)
        EC_KEY_free(key);
    return ret;
}

# endif /* ! HAS_3_0_API */

#else /* ifndef HAVE_EC */

ERL_NIF_TERM ec_generate_key_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{ /* (Curve, PrivKey)  */
    return EXCP_NOTSUP_N(env, 0, "EC not supported");
}
#endif
