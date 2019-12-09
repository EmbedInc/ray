{   Subroutine RAY_TRACE (RAY, COLOR)
*
*   This is the only pre-written "standard" routine of the ray tracer
*   kernel.  RAY is the input ray descriptor in RAY_DESC_T.  It
*   contains or points to all the necessary information to eventually
*   resolve the ray's color.  COLOR is the returned ray's color.  It
*   is internally declared as a 32 bit integer, but RAY_TRACE imposes
*   no format on COLOR.  The caller's format for COLOR must agree with
*   the assumptions made in the shaders.
}
module ray_trace;
define ray_trace;
%include 'ray2.ins.pas';

procedure ray_trace (
  in out  ray: univ ray_desc_t;        {everything you need to know about one ray}
  out     color: univ ray_color_t);    {returned color}
  val_param;

var
  hit_info: ray_hit_info_t;            {handle to all results from INTERSECT_CHECK}
  shader: ray_shader_t;                {pointer to shader that resolves hit color}

begin
  if ray.context_p^.top_level_obj_p^.routines_p^.intersect_check^ ( {hit something ?}
      ray,                             {all the information about this ray}
      ray.context_p^.top_level_obj_p^, {object to intersect ray with}
      ray.context_p^.object_parms_p^,  {parameters for top level object}
      hit_info,                        {specific data returned about this hit}
      shader)                          {routine to call to get hit color}
{
*   The ray hit something.  Call the shader returned by the INTERSECT_CHECK routine
*   to resolve the object's color at the intersect point.
}
    then begin                         {yes, the ray hit something}
      shader^ (                        {call object's shader to resolve color}
        ray,                           {everything you need to know about the ray}
        hit_info,                      {specific info about this intersection}
        color);                        {retured color at intersect point}
      end                              {done with ray hit object case}
{
*   The ray didn't hit any objects.  Call the background shader pointed to
*   by the static ray descriptor to get the background color.
}
    else begin                         {the ray hit nothing at all}
      ray.context_p^.backg_shader^ (   {call background shader}
        ray,                           {same call args as normal shader}
        ray.context_p^.backg_hit_info, {hit info block when nothing was hit}
        color);                        {retured background color for this ray}
      end
    ;
  end;
