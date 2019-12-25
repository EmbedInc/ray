{   Memory allocation and deallocation routines.
}
module ray_mem;
define ray_mem_alloc_perm;

%include 'ray2.ins.pas';
{
********************************************************************************
*
*   Function RAY_MEM_ALLOC_PERM (SIZE)
*
*   Allocate memory under the ray tracer context.  This memory can not later be
*   deallocated.  It persists until the ray tracer memory context is deleted.
*
*   The function returns a pointer to the start of the new memory.  The program
*   is bombed with error if the new memory is not available.
}
function ray_mem_alloc_perm (          {alloc un-releasable mem under ray tracer context}
  in      size: sys_int_adr_t)         {amount of memory to allocate}
  :univ_ptr;                           {pnt to new mem, bombs prog on no mem}
  val_param;

var
  p: univ_ptr;                         {pointer to the new memory}

begin
  util_mem_grab (                      {allocate dynamic memory}
    size,                              {amount of memory to allocate}
    ray_mem_p^,                        {memory context to allocate under}
    false,                             {will not individually dealloc this memory}
    p);                                {returned pointer to the new memory}
  ray_mem_alloc_perm := p;             {return pointer to the new memory}
  end;
