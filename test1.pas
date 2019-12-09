{   Quick test program for ray tracer.  Does not load executable code dynamically.
}
program test1;
%ifdef base_$inserted %then %else
  %include '/cognivision_links/sys_ins/base.ins.pas';
  %var base_$inserted %endif
%ifdef gpr_$inserted %then %else
  %include '/cognivision_links/sys_ins/gpr.ins.pas';
  %var gpr_$inserted %endif
%include 'string.ins.pas';
%include 'file.ins.pas';
%include 'img.ins.pas';
%include 'vect.ins.pas';
%include 'ray_kernel.ins.pas';
%include 'ray.ins.pas';
%include 'ray_t1.ins.pas';

const
  max_objects = 1;
  max_liparms = 1;
  max_visprops = 1;

var
  sphere_routines: ray_object_routines_t; {list of entry points to SPHERE object}
  sphere_data:                         {data to set up a sphere object}
    type1_sphere_user_data_t;
  object:                              {all the various object handles}
    array[1..max_objects] of ray_object_t;
  liparm:                              {all the lightsource parameter blocks}
    array[1..max_liparms] of type1_liparm_t;
  visprop:                             {all the object visual property blocks}
    array[1..max_visprops] of type1_visprop_t;
  ray_context: ray_context_t;          {world context information}
  backg_color:                         {color returned by background shader}
    type1_shader_fixed_data_t;
  world_parms: type1_object_parms_t;   {run time parameters for world object}
  view: ray_view1_t;                   {veiw geometry definition}
  img_handle: img_conn_t;              {handle to image output file}
  fnam:                                {file name of image output file}
    %include '(cog)lib/string_treename.ins.pas';
  status: status_$t;                   {system error code}
  i: integer32;                        {loop counter}

begin
  type1_sphere_routines_make (         {get SPHERE object entry points}
    sphere_routines,                   {where to put entry points}
    sizeof(sphere_routines));          {how much storage to fill in}

  sphere_data.shader :=
    ray_shader_t(addr(type1_shader_phong));
  sphere_data.liparm_p := addr(liparm[1]);
  sphere_data.visprop_p := addr(visprop[1]);
  sphere_data.center.x := 0.0;
  sphere_data.center.y := 0.0;
  sphere_data.center.z := 0.0;
  sphere_data.radius := 0.9;

  liparm[1].n_lights := 2;
  liparm[1].light[1].flag := type1_litype_ambient;
  liparm[1].light[1].amb_col.red := 0.15;
  liparm[1].light[1].amb_col.grn := 0.15;
  liparm[1].light[1].amb_col.blu := 0.15;
  liparm[1].light[1].amb_col.wat := 0.15;
  liparm[1].light[2].flag := type1_litype_directional;
  liparm[1].light[2].dir_col.red := 0.85;
  liparm[1].light[2].dir_col.grn := 0.85;
  liparm[1].light[2].dir_col.blu := 0.85;
  liparm[1].light[2].dir_col.wat := 0.85;
  liparm[1].light[2].dir_uvect.x := 2.0;
  liparm[1].light[2].dir_uvect.y := -4.0;
  liparm[1].light[2].dir_uvect.z := 5.0;
  vect_adjm (liparm[1].light[2].dir_uvect, 1.0); {make unit vector}

  visprop[1].diff_on := true;
  visprop[1].diff_col.red := 0.8;
  visprop[1].diff_col.grn := 0.8;
  visprop[1].diff_col.blu := 0.0;
  visprop[1].diff_col.wat := 1.0;
  visprop[1].spec_on := true;
  visprop[1].spec_col.red := 0.2;
  visprop[1].spec_col.grn := 0.2;
  visprop[1].spec_col.blu := 0.2;
  visprop[1].spec_col.wat := 1.0;
  visprop[1].spec_exp := 15.0;
  visprop[1].refl_on := false;
  visprop[1].trans_on := false;

  object[1].routines_p := addr(sphere_routines);
  sphere_routines.create^ (            {create a new sphere object}
    object[1],                         {object to fill in data pointer to}
    sphere_data,                       {specific data for this object}
    status);                           {error return code}
  file_error_abort (status, 'On create a SPHERE object.');

  ray_context.top_level_obj_p := addr(object[1]);
  ray_context.object_parms_p :=
    ray_object_parms_p_t(addr(world_parms));
  ray_context.backg_shader :=
    ray_shader_t(addr(type1_shader_fixed));
  ray_context.backg_hit_info.shader_parms_p :=
    ray_shader_parms_p_t(addr(backg_color));

  backg_color.col.red := 0.15;
  backg_color.col.grn := 0.15;
  backg_color.col.blu := 0.6;
  backg_color.col.wat := 1.0;
  backg_color.liparm_p := addr(liparm);

  world_parms.shader :=
    ray_shader_t(addr(type1_shader_phong));
  world_parms.liparm_p := addr(liparm[1]);
  world_parms.visprop_p := addr(visprop[1]);

  view.eye.x := 0.0;
  view.eye.y := -3.333;
  view.eye.z := 0.0;
  view.img_cent.x := 0.0;
  view.img_cent.y := 0.0;
  view.img_cent.z := 0.0;
  view.up.x := 0.0;
  view.up.y := 0.0;
  view.up.z := 1.0;
  view.focal_len := 50.0;
  view.x_size := 1.25;
  view.y_size := 1.0;
  view.x_pix := 100;
  view.y_pix := 80;
  ray_view1 (view);                    {set view and image geometry}

  string_appends (fnam, 'test1');      {set generic name of output file}
  img_handle.x_size := view.x_pix;
  img_handle.y_size := view.y_pix;
  img_handle.aspect := 1.25;
  img_handle.explain.max := img_string_length;
  img_handle.explain.len := 0;
  for i := 1 to img_comment_lines do begin
    img_handle.comment[i].max := img_string_length;
    img_handle.comment[i].len := 0;
    end;
  img_handle.file_t := 2;              {indicate IMG file type}
  img_handle.img_dformat := 1;         {use runlen format}
  img_open_w (fnam, img_handle, status); {open image file for write}
  file_error_abort (status, 'On open image output file for write.');
  type1_image_aliased (img_handle, ray_context); {go do the image}
  end.
