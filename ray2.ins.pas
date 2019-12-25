{   Private include file for modules implementing the RAY library.
}
%include 'sys.ins.pas';
%include 'util.ins.pas';
%include 'string.ins.pas';
%include 'file.ins.pas';
%include 'vect.ins.pas';
%include 'ray.ins.pas';

var (ray)
  ray_mem_p: util_mem_context_p_t;     {points to master memory context for ray lib}
