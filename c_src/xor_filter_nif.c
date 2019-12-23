#include <stdint.h>
#include "erl_nif.h"
#include "xorfilter.h"

static ErlNifResourceType* xor8_resource_type;
static ErlNifResourceType* xor16_resource_type;

typedef struct {
   int         is_buffer_allocated;
   uint64_t*   buffer;
   int         is_filter_allocated;
   xor8_t*     filter;
} xor8_filter_resource;

typedef struct {
   int         is_buffer_allocated;
   uint64_t*   buffer;
   int         is_filter_allocated;
   xor16_t*    filter;
} xor16_filter_resource;

void destroy_xor8_filter_resource(ErlNifEnv* env, void* obj) 
{
   xor8_filter_resource* resource = (xor8_filter_resource*) obj;

   if(obj != NULL && resource->is_buffer_allocated) {
      enif_free(resource->buffer);
   }
   if(obj != NULL && resource->is_filter_allocated) {
      xor8_free(resource->filter);
      enif_free(resource->filter);
   }
}

void destroy_xor16_filter_resource(ErlNifEnv* env, void* obj) 
{
   xor16_filter_resource* resource = (xor16_filter_resource*) obj;

   if(obj != NULL && resource->is_buffer_allocated) {
      enif_free(resource->buffer);
   }
   if(obj != NULL && resource->is_filter_allocated) {
      xor16_free(resource->filter);
      enif_free(resource->filter);
   }
}

ErlNifResourceType* xor8_filter_resource_type(ErlNifEnv* env) 
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

ErlNifResourceType* xor16_filter_resource_type(ErlNifEnv* env) 
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


/* Begin xor8 nif code */
static ERL_NIF_TERM
xor8_initialize(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[], int buffered)
{
   ERL_NIF_TERM head;
   ERL_NIF_TERM is_list = argv[0];
   unsigned list_length;
   uint64_t* value_list;

   if(argc != 1)
   {
      return enif_make_badarg(env);
   }

   if(!enif_is_list(env, is_list)) {
      return enif_make_badarg(env);
   }

   if(!enif_get_list_length(env, argv[0], &list_length)) {
      return mk_error(env, "get_list_length_error");
   }

   xor8_filter_resource* filter_resource = 
      enif_alloc_resource(xor8_resource_type, sizeof(xor8_filter_resource));
   filter_resource->is_buffer_allocated = false;
   filter_resource->is_filter_allocated = false;

   value_list = enif_alloc(sizeof(uint64_t) * list_length);

   if(value_list == NULL) {
      return mk_error(env, "could_not_allocate_memory_error");
   }

   uint64_t current = 0;
   for(int i = 0; enif_get_list_cell(env, argv[0], &head, (ERL_NIF_TERM*) &(argv[0])); i++) {
      if(!enif_get_uint64(env, head, &current)) {
         return mk_error(env, "convert_to_uint64_t_error");
      }
      value_list[i] = current;
   }

   filter_resource->buffer = value_list;
   filter_resource->is_buffer_allocated = true;
   
   xor8_t* filter = enif_alloc(sizeof(xor8_t));

   if(!xor8_allocate(list_length, filter)) {
      enif_release_resource(filter);
      return mk_error(env, "xor8_allocate_error");
   }

   /* Determine to use a buffered populate for speed or not */
   if(buffered) {
      if(!xor8_populate(value_list, list_length, filter)) {
         enif_release_resource(filter);
         return mk_error(env, "xor8_populate_error");
      }
   }
   else
   {
      if(!xor8_buffered_populate(value_list, list_length, filter)) {
         enif_release_resource(filter);
         return mk_error(env, "xor8_populate_error");
      }
   }

   filter_resource->filter = filter;
   filter_resource->is_filter_allocated = true;
   ERL_NIF_TERM term = enif_make_resource(env, filter_resource);

   return term;
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

   xor8_filter_resource* filter_resource;
   if(!enif_get_resource(env, argv[0], xor8_resource_type, (void**) &filter_resource)) 
   {
      return mk_error(env, "get_filter_for_contains_error");
   }
   xor8_t* filter = filter_resource->filter;

   ErlNifUInt64 key;
   if(!enif_get_uint64(env, argv[1], &key)) 
   {
      return mk_error(env, "get_key_for_contains_error");
   }

   if(xor8_contain(key, filter)) {
      return mk_atom(env, "true");
   }
   else {
      return mk_atom(env, "false");
   }

   return mk_atom(env, "false");
}

static ERL_NIF_TERM
xor8_free_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{

   if(argc != 1)
   {
      return enif_make_badarg(env);
   }

   xor8_filter_resource* filter_resource;
   if(!enif_get_resource(env, argv[0], xor8_resource_type, (void**) &filter_resource)) {
      return mk_error(env, "get_filter_for_deallocation_error");
   }

   enif_release_resource(filter_resource);

   return mk_atom(env, "ok");
}

/* Begin xor16 nif code */
static ERL_NIF_TERM
xor16_initialize(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[], int buffered)
{
   ERL_NIF_TERM head;
   ERL_NIF_TERM is_list = argv[0];
   unsigned list_length;
   uint64_t* value_list;

   if(argc != 1)
   {
      return enif_make_badarg(env);
   }

   if(!enif_is_list(env, is_list)) {
      return enif_make_badarg(env);
   }

   if(!enif_get_list_length(env, argv[0], &list_length)) {
      return mk_error(env, "get_list_length_error");
   }

   xor16_filter_resource* filter_resource = 
      enif_alloc_resource(xor16_resource_type, sizeof(xor8_filter_resource));
   filter_resource->is_buffer_allocated = false;
   filter_resource->is_filter_allocated = false;

   value_list = enif_alloc(sizeof(uint64_t) * list_length);

   if(value_list == NULL) {
      return mk_error(env, "could_not_allocate_memory_error");
   }

   uint64_t current = 0;
   for(int i = 0; enif_get_list_cell(env, argv[0], &head, (ERL_NIF_TERM*) &(argv[0])); i++) {
      if(!enif_get_uint64(env, head, &current)) {
         return mk_error(env, "convert_to_uint64_t_error");
      }
      value_list[i] = current;
   }

   filter_resource->buffer = value_list;
   filter_resource->is_buffer_allocated = true;
   
   xor16_t* filter = enif_alloc(sizeof(xor8_t));

   if(!xor16_allocate(list_length, filter)) {
      enif_release_resource(filter);
      return mk_error(env, "xor16_allocate_error");
   }

   /* Determine to use a buffered populate for speed or not */
   if(buffered) {
      if(!xor16_populate(value_list, list_length, filter)) {
         enif_release_resource(filter);
         return mk_error(env, "xor16_populate_error");
      }
   }
   else
   {
      if(!xor16_buffered_populate(value_list, list_length, filter)) {
         enif_release_resource(filter);
         return mk_error(env, "xor16_populate_error");
      }
   }

   filter_resource->filter = filter;
   filter_resource->is_filter_allocated = true;
   ERL_NIF_TERM term = enif_make_resource(env, filter_resource);

   return term;
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

   xor16_filter_resource* filter_resource;
   if(!enif_get_resource(env, argv[0], xor16_resource_type, (void**) &filter_resource)) {
      return mk_error(env, "get_filter_for_contains_error");
   }
   xor16_t* filter = filter_resource->filter;

   ErlNifUInt64 key;
   if(!enif_get_uint64(env, argv[1], &key)) {
      return mk_error(env, "get_key_for_contains_error");
   }

   if(xor16_contain(key, filter)) {
      return mk_atom(env, "true");
   }
   else {
      return mk_atom(env, "false");
   }

   return mk_atom(env, "false");
}

static ERL_NIF_TERM
xor16_free_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{

   if(argc != 1)
   {
      return enif_make_badarg(env);
   }

   xor16_filter_resource* filter_resource;
   if(!enif_get_resource(env, argv[0], xor16_resource_type, (void**) &filter_resource)) {
      return mk_error(env, "get_filter_for_deallocation_error");
   }

   enif_release_resource(filter_resource);

   return mk_atom(env, "ok");
}

static int
nif_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
   xor8_resource_type = xor8_filter_resource_type(env);
   xor16_resource_type = xor16_filter_resource_type(env);
   return 0;
}

static ErlNifFunc nif_funcs[] = {
   {"xor8_initialize", 1, xor8_initialize_nif},
   {"xor8_buffered_initialize", 1, xor8_buffered_initialize_nif},
   {"xor8_contain", 2, xor8_contain_nif},
   {"xor8_free", 1, xor8_free_nif},
   {"xor16_initialize", 1, xor16_initialize_nif},
   {"xor16_buffered_initialize", 1, xor16_buffered_initialize_nif},
   {"xor16_contain", 2, xor16_contain_nif},
   {"xor16_free", 1, xor16_free_nif}
};

ERL_NIF_INIT(exor_filter, nif_funcs, nif_load, NULL, NULL, NULL);
