#include <stdint.h>
#include <string.h>
#include "erl_nif.h"

#define malloc(size) enif_alloc(size)
#define free(size) enif_free(size)
#include "xorfilter.h"

static ErlNifResourceType* xor8_resource_type;
static ErlNifResourceType* xor16_resource_type;

// portable encoding/decoding helpers
void
unpack_le_u64(uint64_t * dst, uint8_t const * src) {
    *dst = ((uint64_t)src[7] << 56) | ((uint64_t)src[6] << 48)
           | ((uint64_t)src[5] << 40) | ((uint64_t)src[4] << 32)
           | ((uint64_t)src[3] << 24) | ((uint64_t)src[2] << 16)
           | ((uint64_t)src[1] << 8) | (uint64_t)src[0];
}

void
pack_le_u64(uint8_t * dst, uint64_t val) {
    dst[0] = val & 0xff;
    dst[1] = (val >> 8) & 0xff;
    dst[2] = (val >> 16) & 0xff;
    dst[3] = (val >> 24) & 0xff;
    dst[4] = (val >> 32) & 0xff;
    dst[5] = (val >> 40) & 0xff;
    dst[6] = (val >> 48) & 0xff;
    dst[7] = (val >> 56) & 0xff;
}

void 
destroy_xor8_filter_resource(ErlNifEnv* env, void* obj) 
{
   xor8_t* filter = (xor8_t*) obj;

   xor8_free(filter);
}

void 
destroy_xor16_filter_resource(ErlNifEnv* env, void* obj) 
{
   xor16_t* filter = (xor16_t*) obj;

   xor16_free(filter);
}

ErlNifResourceType* 
xor8_filter_resource_type(ErlNifEnv* env) 
{
   return enif_open_resource_type(
      env,
      NULL,
      "xor8_filter_resource",
      destroy_xor8_filter_resource,
      ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER,
      NULL
   );
}

ErlNifResourceType* 
xor16_filter_resource_type(ErlNifEnv* env) 
{
   return enif_open_resource_type(
      env,
      NULL,
      "xor16_filter_resource",
      destroy_xor16_filter_resource,
      ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER,
      NULL
   );
}

ERL_NIF_TERM
mk_atom(ErlNifEnv* env, const char* atom)
{
   ERL_NIF_TERM ret;

   if(!enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1))
   {
      return enif_make_atom(env, atom);
   }

   return ret;
}

ERL_NIF_TERM
mk_error(ErlNifEnv* env, const char* mesg)
{
   return enif_make_tuple2(env, mk_atom(env, "error"), mk_atom(env, mesg));
}

static int
fill_buffer(uint64_t* buffer, ErlNifEnv* env, ERL_NIF_TERM list)
{
   ERL_NIF_TERM head;
   uint64_t current = 0;
   for(int i = 0; enif_get_list_cell(env, list, &head, (ERL_NIF_TERM*) &list); i++) 
   {
      if(!enif_get_uint64(env, head, &current)) 
      {
         return false;
      }
      buffer[i] = current;
   }
   return true;
}

/* Begin xor8 nif code */
static ERL_NIF_TERM
xor8_initialize(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[], int buffered)
{
   ERL_NIF_TERM is_list = argv[0];
   unsigned list_length;
   uint64_t* value_list;

   if(argc != 1)
   {
      return enif_make_badarg(env);
   }

   if(!enif_is_list(env, is_list)) 
   {
      return enif_make_badarg(env);
   }

   if(!enif_get_list_length(env, argv[0], &list_length)) 
   {
      return mk_error(env, "get_list_length_error");
   }

   value_list = enif_alloc(sizeof(uint64_t) * list_length);

   if(value_list == NULL) 
   {
      return mk_error(env, "could_not_allocate_memory_error");
   }

   if(!(fill_buffer(value_list, env, argv[0]))) 
   {
      enif_free(value_list);
      return mk_error(env, "convert_to_uint64_t_error");
   }

    xor8_t* filter = 
      enif_alloc_resource(xor8_resource_type, sizeof(xor8_t));

   if(!xor8_allocate(list_length, filter)) 
   {
      enif_free(value_list);
      enif_release_resource(filter);
      return mk_error(env, "xor8_allocate_error");
   }

   // Determine to use a buffered populate for speed or not
   if(buffered) 
   {
      if(!xor8_buffered_populate(value_list, list_length, filter)) 
      {
         enif_free(value_list);
         xor8_free(filter);
         enif_release_resource(filter);
         return mk_error(env, "duplicates_in_hash_error");
      }
   }
   else
   {
      if(!xor8_populate(value_list, list_length, filter)) 
      {
         enif_free(value_list);
         xor8_free(filter);
         enif_release_resource(filter);
         return mk_error(env, "duplicates_in_hash_error");
      }
   }

   enif_free(value_list);

   ERL_NIF_TERM res = enif_make_resource(env, filter);
   // release this resource now its owned by Erlang
   enif_release_resource(filter);
   return res;

}

static ERL_NIF_TERM
xor8_initialize_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
   return xor8_initialize(env, argc, argv, false);
}

static ERL_NIF_TERM
xor8_buffered_initialize_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
   return xor8_initialize(env, argc, argv, true);
}

static ERL_NIF_TERM
xor8_contain_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{

   if(argc != 2)
   {
      return enif_make_badarg(env);
   }

   xor8_t* filter;
   if(!enif_get_resource(env, argv[0], xor8_resource_type, (void**) &filter)) 
   {
      return mk_error(env, "get_filter_for_contains_error");
   }

   ErlNifUInt64 key;
   if(!enif_get_uint64(env, argv[1], &key)) 
   {
      return mk_error(env, "get_key_for_contains_error");
   }

   if(xor8_contain(key, filter)) 
   {
      return mk_atom(env, "true");
   }
   else 
   {
      return mk_atom(env, "false");
   }

   return mk_atom(env, "false");
}

static ERL_NIF_TERM
xor8_to_bin_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{

   if(argc != 1)
   {
      return enif_make_badarg(env);
   }

   xor8_t* filter;
   if(!enif_get_resource(env, argv[0], xor8_resource_type, (void**) &filter)) 
   {
      return mk_error(env, "get_filter_for_to_bin");
   }

   size_t bin_size = (sizeof(uint64_t)*2) + (sizeof(uint8_t) * filter->blockLength * 3);

   ErlNifBinary bin;

   if(!enif_alloc_binary(bin_size, &bin)) {
      return mk_error(env, "allocate_binary_for_to_bin");
   }

   pack_le_u64(bin.data, filter->seed);
   pack_le_u64(bin.data + sizeof(uint64_t), filter->blockLength);
   memcpy(bin.data + (sizeof(uint64_t) * 2), filter->fingerprints, filter->blockLength*3);

   return enif_make_binary(env, &bin);
}

static ERL_NIF_TERM
xor8_from_bin_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{

   if(argc != 1)
   {
      return enif_make_badarg(env);
   }

   ErlNifBinary bin;

   if (!enif_inspect_binary(env, argv[0], &bin)) {
      return enif_make_badarg(env);
   }

   if (bin.size < sizeof(uint64_t) * 2) {
      return enif_make_badarg(env);
   }

   xor8_t* filter = 
       enif_alloc_resource(xor8_resource_type, sizeof(xor8_t));

   unpack_le_u64(&filter->seed, bin.data);
   unpack_le_u64(&filter->blockLength, bin.data+sizeof(uint64_t));

   if (bin.size != (sizeof(uint64_t)*2) + (filter->blockLength * 3)) {
       enif_release_resource(filter);
      return enif_make_badarg(env);
   }

   filter->fingerprints = enif_alloc(filter->blockLength * 3);
   memcpy(filter->fingerprints, bin.data+(sizeof(uint64_t) * 2), filter->blockLength * 3);

   ERL_NIF_TERM res = enif_make_resource(env, filter);
   // release this resource now its owned by Erlang
   enif_release_resource(filter);
   return res;
}


/* Begin xor16 nif code */
static ERL_NIF_TERM
xor16_initialize(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[], int buffered)
{
   ERL_NIF_TERM is_list = argv[0];
   unsigned list_length;
   uint64_t* value_list;

   if(argc != 1)
   {
      return enif_make_badarg(env);
   }

   if(!enif_is_list(env, is_list)) 
   {
      return enif_make_badarg(env);
   }

   if(!enif_get_list_length(env, argv[0], &list_length)) 
   {
      return mk_error(env, "get_list_length_error");
   }

   value_list = enif_alloc(sizeof(uint64_t) * list_length);

   if(value_list == NULL) 
   {
      return mk_error(env, "could_not_allocate_memory_error");
   }

   if(!(fill_buffer(value_list, env, argv[0]))) {
      enif_free(value_list);
      return mk_error(env, "convert_to_uint64_t_error");
   }

    xor16_t* filter = 
      enif_alloc_resource(xor16_resource_type, sizeof(xor16_t));

   if(!xor16_allocate(list_length, filter)) 
   {
      enif_free(value_list);
      enif_release_resource(filter);
      return mk_error(env, "xor16_allocate_error");
   }

   // Determine to use a buffered populate for speed or not
   if(buffered) 
   {
      if(!xor16_buffered_populate(value_list, list_length, filter)) 
      {
         enif_free(value_list);
         xor16_free(filter);
         enif_release_resource(filter);
         return mk_error(env, "duplicates_in_hash_error");
      }
   }
   else
   {
      if(!xor16_populate(value_list, list_length, filter)) 
      {
         enif_free(value_list);
         xor16_free(filter);
         enif_release_resource(filter);
         return mk_error(env, "duplicates_in_hash_error");
      }
   }

   enif_free(value_list);

   ERL_NIF_TERM res = enif_make_resource(env, filter);
   // release this resource now its owned by Erlang
   enif_release_resource(filter);
   return res;
}

static ERL_NIF_TERM
xor16_initialize_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
   return xor16_initialize(env, argc, argv, false);
}

static ERL_NIF_TERM
xor16_buffered_initialize_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
   return xor16_initialize(env, argc, argv, true);
}

static ERL_NIF_TERM
xor16_contain_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{

   if(argc != 2)
   {
      return enif_make_badarg(env);
   }

   xor16_t* filter;
   if(!enif_get_resource(env, argv[0], xor16_resource_type, (void**) &filter)) 
   {
      return mk_error(env, "get_filter_for_contains_error");
   }

   ErlNifUInt64 key;
   // Hash the values or not.
   if(!enif_get_uint64(env, argv[1], &key)) 
   {
      return mk_error(env, "get_key_for_contains_error");
   }

   if(xor16_contain(key, filter)) 
   {
      return mk_atom(env, "true");
   }
   else 
   {
      return mk_atom(env, "false");
   }

   return mk_atom(env, "false");
}

static ERL_NIF_TERM
xor16_to_bin_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{

   if(argc != 1)
   {
      return enif_make_badarg(env);
   }

   xor16_t* filter;
   if(!enif_get_resource(env, argv[0], xor16_resource_type, (void**) &filter)) 
   {
      return mk_error(env, "get_filter_for_to_bin");
   }

   size_t bin_size = (sizeof(uint64_t)*2) + (sizeof(uint16_t) * filter->blockLength * 3);

   ErlNifBinary bin;

   if(!enif_alloc_binary(bin_size, &bin)) {
      return mk_error(env, "allocate_binary_for_to_bin");
   }

   pack_le_u64(bin.data, filter->seed);
   pack_le_u64(bin.data + sizeof(uint64_t), filter->blockLength);
   // TODO endianness
   memcpy(bin.data + (sizeof(uint64_t) * 2), filter->fingerprints, filter->blockLength*sizeof(uint16_t)*3);

   return enif_make_binary(env, &bin);
}

static ERL_NIF_TERM
xor16_from_bin_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{

   if(argc != 1)
   {
      return enif_make_badarg(env);
   }

   ErlNifBinary bin;

   if (!enif_inspect_binary(env, argv[0], &bin)) {
      return enif_make_badarg(env);
   }

   if (bin.size < sizeof(uint64_t) * 2) {
      return enif_make_badarg(env);
   }

   xor16_t* filter = 
      enif_alloc_resource(xor16_resource_type, sizeof(xor16_t));

   unpack_le_u64(&filter->seed, bin.data);
   unpack_le_u64(&filter->blockLength, bin.data+sizeof(uint64_t));

   if (bin.size != (sizeof(uint64_t)*2) + (filter->blockLength * sizeof(uint16_t) * 3)) {
       enif_release_resource(filter);
      return enif_make_badarg(env);
   }

   // TODO endianness
   filter->fingerprints = enif_alloc(filter->blockLength * sizeof(uint16_t) * 3);
   memcpy(filter->fingerprints, bin.data+(sizeof(uint64_t) * 2), filter->blockLength * sizeof(uint16_t) * 3);

   ERL_NIF_TERM res = enif_make_resource(env, filter);
   // release this resource now its owned by Erlang
   enif_release_resource(filter);
   return res;
}

static int
nif_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
   xor8_resource_type = xor8_filter_resource_type(env);
   xor16_resource_type = xor16_filter_resource_type(env);
   return 0;
}

static ErlNifFunc nif_funcs[] = {
   {"xor8_initialize_nif", 1, xor8_initialize_nif},
   {"xor8_initialize_nif_dirty", 1, xor8_initialize_nif, 
      ERL_NIF_DIRTY_JOB_CPU_BOUND},
   {"xor8_buffered_initialize_nif", 1, xor8_buffered_initialize_nif},
   {"xor8_buffered_initialize_nif_dirty", 1, 
      xor8_buffered_initialize_nif, ERL_NIF_DIRTY_JOB_CPU_BOUND},
   {"xor8_contain_nif", 2, xor8_contain_nif},
   {"xor8_to_bin_nif", 1, xor8_to_bin_nif},
   {"xor8_from_bin_nif", 1, xor8_from_bin_nif},
   
   {"xor16_initialize_nif", 1, xor16_initialize_nif},
   {"xor16_initialize_nif_dirty", 1, xor16_initialize_nif, 
      ERL_NIF_DIRTY_JOB_CPU_BOUND},
   {"xor16_buffered_initialize_nif", 1, 
      xor16_buffered_initialize_nif},
   {"xor16_buffered_initialize_nif_dirty", 1, 
      xor16_buffered_initialize_nif, ERL_NIF_DIRTY_JOB_CPU_BOUND},
   {"xor16_contain_nif", 2, xor16_contain_nif},
   {"xor16_to_bin_nif", 1, xor16_to_bin_nif},
   {"xor16_from_bin_nif", 1, xor16_from_bin_nif},

};

ERL_NIF_INIT(exor_filter, nif_funcs, nif_load, NULL, NULL, NULL);
