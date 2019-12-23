/*
#include "xor8_filter_resource.h"

void destroy_xor8_filter_resource(ErlNifEnv* env, void* obj) 
{
   xor8_filter_resource* resource = (xor8_filter_resource*) obj;

   if(resource->is_buffer_allocated) {
      enif_free(resource->buffer);
   }
   if(resource->is_filter_allocated) {
      xor8_free(resource->filter);
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
*/
