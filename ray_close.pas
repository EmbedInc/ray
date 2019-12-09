{   Subroutine RAY_CLOSE
*
*   Close this use of the ray tracer library.  All resources will be deallocated.
*   RAY_INIT must be called before the ray tracer library can be used again.
}
module ray_close;
define ray_close;
%include 'ray2.ins.pas';

procedure ray_close;                   {close use of ray tracer, release resources}
  val_param;

begin
  util_mem_context_del (ray_mem_p);    {deallocate all ray library dynamic memory}
  end;
