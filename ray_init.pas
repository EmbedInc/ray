{   Subroutine RAY_INIT (PARENT_MEM)
*
*   Init use of the ray tracer library.  This must always be the first RAY call
*   and the first call after RAY_CLOSE.  All memory dynamically allocated by the
*   ray tracer will be subordinate to PARENT_MEM.
}
module ray_init;
define ray_init;
%include 'ray2.ins.pas';

procedure ray_init (
  in out  parent_mem: util_mem_context_t); {all new ray mem will be below this}
  val_param;

begin
  util_mem_context_get (parent_mem, ray_mem_p); {create mem context for ray tracer}
  ray_mem_p^.pool_size := 250000;      {allocate 1/4 Mb at a time}
  ray_mem_p^.max_pool_chunk := 25000;  {max allowed to get from pool at one time}
  end;
