{   Subroutine TYPE1_SHADER_FIXED (RAY,HIT_INFO,COLOR)
*
*   Shader that always returns a fixed color.  This can be used for the background.
}
module type1_shader_fixed;
define type1_shader_fixed;
%include 'ray_type1_2.ins.pas';

procedure type1_shader_fixed (         {always returns fixed color, used for backg}
  in var  ray: type1_ray_t;            {handle to the ray}
  in var  hit_info: ray_hit_info_t;    {info about specific intersection}
  out     color: type1_color_t);       {returned ray color}

var
  parms_p: type1_shader_fixed_data_p_t; {data pointed to from HIT_INFO}

begin
  parms_p :=                           {make pointer to our shader data block}
    type1_shader_fixed_data_p_t(hit_info.shader_parms_p);
  color := parms_p^.col;               {return the color from parms block}
  end;
