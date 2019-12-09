{   Subroutine RAY_INIT_DEFAULT
*
*   Init ray tracer library, using defaults.  This call may be made instead of
*   any RAY_INIT call.
}
module ray_init_default;
define ray_init_default;
%include 'ray2.ins.pas';

procedure ray_init_default;            {init ray tracer, use defaults}
  val_param;

begin
  ray_init (util_top_mem_context);
  end;
