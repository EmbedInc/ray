{   Subroutine RAY_INIT (PARENT_MEM)
*
*   Initialize use of the ray tracer library.  This must be the first call into
*   the RAY library, and is the only valid call immediately after RAY_CLOSE.
*   All memory dynamically allocated by the RAY library will be subordinate to
*   PARENT_MEM, and will be deallocated by RAY_CLOSE.
}
module ray_init;
define ray_init;
%include 'ray2.ins.pas';

const
  pool_size = 1000000;                 {bytes to allocate at one time}
  max_chunk = 25000;                   {max chunk size to get from pool}

procedure ray_init (
  in out  parent_mem: util_mem_context_t); {all new ray mem will be below this}
  val_param;

begin
  util_mem_context_get (parent_mem, ray_mem_p); {create mem context for ray tracer}
  ray_mem_p^.pool_size := pool_size;   {set amount of mem to allocate at one time}
  ray_mem_p^.max_pool_chunk := max_chunk; {max allowed to get from pool at one time}
  end;
