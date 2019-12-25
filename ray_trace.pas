{   Subroutine RAY_TRACE (RAY, COLOR)
*
*   Trace a ray and resolve its "color".
*
*   The data type for the ray used here, RAY_DESC_T, is not intended to be used
*   directly.  It is a template for holding only the minimum necessary
*   information required at this level.  The TYPEn client routines define the
*   details of the ray, as assumed by their objects and shaders.
*
*   COLOR is returned the final resolved color of the ray.  The format of color
*   is set by the TYPEn client routines, and is unknown here.
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
{
*   Find whether the ray hits something.  If so, then HIT_INFO and SHADER are
*   also returned.
}
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
      shader^ (                        {call the supplied shader to resolve the color}
        ray,                           {everything you need to know about the ray}
        hit_info,                      {specific info about this intersection}
        color);                        {retured color at intersect point}
      end                              {done with ray hit object case}
{
*   The ray didn't hit any objects.  Call the background shader pointed to
*   by the static ray descriptor to get the background color.
}
    else begin                         {the ray hit nothing at all}
      ray.context_p^.backg_shader^ (   {call the default "background" shader}
        ray,                           {the ray information}
        ray.context_p^.backg_hit_info, {default hit info when ray hit nothing}
        color);                        {retured background color for this ray}
      end
    ;
  end;
