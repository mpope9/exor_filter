/*
#ifndef XOR8_FILTER_RESOURCE_H
#define XOR8_FILTER_RESOURCE_H

#include "erl_nif.h"
#include <stdint.h>
#include "xorfilter.h"

typedef struct {
   int         is_buffer_allocated;
   uint64_t*   buffer;
   int         is_filter_allocated;
   xor8_t*     filter;
} xor8_filter_resource;

void destory_xor8_filter_resource(ErlNifEnv*, void*);

ErlNifResourceType* xor8_filter_resource_type(ErlNifEnv*);

#endif
*/
