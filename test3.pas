{   Quick test program for ray tracer.  Does not load executable code dynamically.
*   Intended to test transparency.

}
program test3;
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
  max_objects = 10;
  max_liparms = 1;
  max_visprops = 10;

var
  sphere_routines: ray_object_routines_t; {list of entry points to SPHERE object}
  sphere_data:                         {data to set up a sphere object}
    type1_sphere_user_data_t;
  list_routines: ray_object_routines_t; {list of entry points to LIST object}
  list_data:                           {data to set up a list object}
    type1_list_user_data_t;
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
  n_obj: integer32;                    {current number of objects}
  n_vsp: integer32;                    {current number of visprop blocks}
  r1, r2: real;                        {scratch floating point numbers}

begin
  n_obj := 0;                          {init number of current objects}
  n_vsp := 0;                          {init number of current visprop blocks}
  type1_sphere_routines_make (         {get SPHERE object entry points}
    sphere_routines,                   {where to put entry points}
    sizeof(sphere_routines));          {how much storage to fill in}
  type1_list_routines_make (           {get LIST object entry points}
    list_routines,                     {where to put entry points}
    sizeof(list_routines));            {how much storage to fill in}

  view.eye.x := 0.0;
  view.eye.y := 0.0;
  view.eye.z := 2.5;
  view.img_cent.x := 0.0;
  view.img_cent.y := -0.2;
  view.img_cent.z := 0.0;
  view.up.x := 0.0;
  view.up.y := 1.0;
  view.up.z := 0.0;
  view.focal_len := 50.0;
  view.x_size := 1.25;
  view.y_size := 1.0;
  view.x_pix := 100;
  view.y_pix := 80;
  ray_view1 (view);                    {set view and image geometry}

  backg_color.col.red := 0.18;
  backg_color.col.grn := 0.18;
  backg_color.col.blu := 0.18;
  backg_color.col.wat := 1.0;
  backg_color.liparm_p := addr(liparm);

  liparm[1].n_lights := 3;
  liparm[1].light[1].flag := type1_litype_ambient;
  liparm[1].light[1].amb_col.red := 0.15;
  liparm[1].light[1].amb_col.grn := 0.15;
  liparm[1].light[1].amb_col.blu := 0.15;
  liparm[1].light[1].amb_col.wat := 0.15;
  liparm[1].light[2].flag := type1_litype_directional;
  liparm[1].light[2].dir_col.red := 0.55;
  liparm[1].light[2].dir_col.grn := 0.55;
  liparm[1].light[2].dir_col.blu := 0.55;
  liparm[1].light[2].dir_col.wat := 0.55;
  liparm[1].light[2].dir_uvect.x := 2.0;
  liparm[1].light[2].dir_uvect.y := 4.0;
  liparm[1].light[2].dir_uvect.z := 5.0;
  vect_adjm (liparm[1].light[2].dir_uvect, 1.0); {make unit vector}
  liparm[1].light[3].flag := type1_litype_directional;
  liparm[1].light[3].dir_col.red := 0.30;
  liparm[1].light[3].dir_col.grn := 0.30;
  liparm[1].light[3].dir_col.blu := 0.30;
  liparm[1].light[3].dir_col.wat := 0.30;
  liparm[1].light[3].dir_uvect.x := 6.0;
  liparm[1].light[3].dir_uvect.y := 4.0;
  liparm[1].light[3].dir_uvect.z := 4.0;
  vect_adjm (liparm[1].light[3].dir_uvect, 1.0); {make unit vector}

  list_data.shader := nil;
  list_data.liparm_p := nil;
  list_data.visprop_p := nil;

  n_obj := 1;                          {first object is the aggregate}
  object[n_obj].routines_p := addr(list_routines);
  list_routines.create^ (              {create a new list object}
    object[n_obj],                     {object to fill in data pointer to}
    list_data,                         {specific data for this object}
    status);                           {error return code}
  file_error_abort (status, 'On create a LIST object.');

  n_vsp := n_vsp + 1;                  {allocate a visprop block}
  visprop[n_vsp].diff_on := true;
  visprop[n_vsp].diff_col.red := 0.7;
  visprop[n_vsp].diff_col.grn := 0.14;
  visprop[n_vsp].diff_col.blu := 0.14;
  visprop[n_vsp].diff_col.wat := 0.7;
  visprop[n_vsp].spec_on := false;
  visprop[n_vsp].refl_on := true;
  visprop[n_vsp].refl_min.red := 0.3;
  visprop[n_vsp].refl_min.grn := 0.3;
  visprop[n_vsp].refl_min.blu := 0.3;
  visprop[n_vsp].refl_min.wat := 0.3;
  visprop[n_vsp].refl_max.red := 1.0;
  visprop[n_vsp].refl_max.grn := 1.0;
  visprop[n_vsp].refl_max.blu := 1.0;
  visprop[n_vsp].refl_max.wat := 1.0;
  visprop[n_vsp].trans_on := false;
  sphere_data.shader := nil;
  sphere_data.liparm_p := nil;
  sphere_data.visprop_p := addr(visprop[n_vsp]);
  sphere_data.center.x := 0.0;
  sphere_data.center.y := 0.2;
  sphere_data.center.z := 0.0;
  sphere_data.radius := 0.2;
  n_obj := n_obj + 1;                  {allocate a new object}
  object[n_obj].routines_p := addr(sphere_routines);
  sphere_routines.create^ (            {create a new sphere object}
    object[n_obj],                     {object to fill in data pointer to}
    sphere_data,                       {specific data for this object}
    status);                           {error return code}
  file_error_abort (status, 'On create a SPHERE object.');
  list_routines.add_child^ (           {add this object to master aggregate}
    object[1],                         {the aggregate object}
    object[n_obj],                     {the object to add}
    0);                                {optional parameters not used by LIST}

  n_vsp := n_vsp + 1;                  {allocate a visprop block}
  visprop[n_vsp].diff_on := true;
  visprop[n_vsp].diff_col.red := 0.225;
  visprop[n_vsp].diff_col.grn := 0.225;
  visprop[n_vsp].diff_col.blu := 0.45;
  visprop[n_vsp].diff_col.wat := 0.45;
  visprop[n_vsp].spec_on := false;
  visprop[n_vsp].refl_on := true;
  visprop[n_vsp].refl_min.red := 0.55;
  visprop[n_vsp].refl_min.grn := 0.55;
  visprop[n_vsp].refl_min.blu := 0.55;
  visprop[n_vsp].refl_min.wat := 0.55;
  visprop[n_vsp].refl_max.red := 1.0;
  visprop[n_vsp].refl_max.grn := 1.0;
  visprop[n_vsp].refl_max.blu := 1.0;
  visprop[n_vsp].refl_max.wat := 1.0;
  visprop[n_vsp].trans_on := false;
  sphere_data.shader := nil;
  sphere_data.liparm_p := nil;
  sphere_data.visprop_p := addr(visprop[n_vsp]);
  sphere_data.center.x := -0.4;
  sphere_data.center.y := -0.5;
  sphere_data.center.z := -0.85;
  sphere_data.radius := 0.6;
  n_obj := n_obj + 1;                  {allocate a new object}
  object[n_obj].routines_p := addr(sphere_routines);
  sphere_routines.create^ (            {create a new sphere object}
    object[n_obj],                     {object to fill in data pointer to}
    sphere_data,                       {specific data for this object}
    status);                           {error return code}
  file_error_abort (status, 'On create a SPHERE object.');
  list_routines.add_child^ (           {add this object to master aggregate}
    object[1],                         {the aggregate object}
    object[n_obj],                     {the object to add}
    0);                                {optional parameters not used by LIST}

  n_vsp := n_vsp + 1;                  {allocate a visprop block}
  visprop[n_vsp].diff_on := false;
  visprop[n_vsp].diff_col.red := 0.05;
  visprop[n_vsp].diff_col.grn := 0.05;
  visprop[n_vsp].diff_col.blu := 0.0;
  visprop[n_vsp].diff_col.wat := 0.05;
  visprop[n_vsp].spec_on := false;
  visprop[n_vsp].refl_on := true;
  visprop[n_vsp].refl_min.red := 0.1;
  visprop[n_vsp].refl_min.grn := 0.1;
  visprop[n_vsp].refl_min.blu := 0.1;
  visprop[n_vsp].refl_min.wat := 0.1;
  visprop[n_vsp].refl_max.red := 1.0;
  visprop[n_vsp].refl_max.grn := 1.0;
  visprop[n_vsp].refl_max.blu := 1.0;
  visprop[n_vsp].refl_max.wat := 1.0;
  visprop[n_vsp].trans_on := true;
  visprop[n_vsp].trans_col.red := 1.0;
  visprop[n_vsp].trans_col.grn := 1.0;
  visprop[n_vsp].trans_col.blu := 1.0;
  visprop[n_vsp].trans_col.wat := 1.0;
  visprop[n_vsp].trans_ind := 1.5;
  visprop[n_vsp].trans_wat := 0.95;
  sphere_data.shader := nil;
  sphere_data.liparm_p := nil;
  sphere_data.visprop_p := addr(visprop[n_vsp]);
  sphere_data.center.x := 0.35;
  sphere_data.center.y := -0.35;
  sphere_data.center.z := 0.0;
  sphere_data.radius := 0.33;
  n_obj := n_obj + 1;                  {allocate a new object}
  object[n_obj].routines_p := addr(sphere_routines);
  sphere_routines.create^ (            {create a new sphere object}
    object[n_obj],                     {object to fill in data pointer to}
    sphere_data,                       {specific data for this object}
    status);                           {error return code}
  file_error_abort (status, 'On create a SPHERE object.');
  list_routines.add_child^ (           {add this object to master aggregate}
    object[1],                         {the aggregate object}
    object[n_obj],                     {the object to add}
    0);                                {optional parameters not used by LIST}

  world_parms.shader :=
    ray_shader_t(addr(type1_shader_phong));
  world_parms.liparm_p := addr(liparm[1]);
  world_parms.visprop_p := addr(visprop[1]);

  ray_context.top_level_obj_p := addr(object[1]);
  ray_context.object_parms_p :=
    ray_object_parms_p_t(addr(world_parms));
  ray_context.backg_shader :=
    ray_shader_t(addr(type1_shader_fixed));
  ray_context.backg_hit_info.shader_parms_p :=
    ray_shader_parms_p_t(addr(backg_color));

  string_appends (fnam, 'test3');      {set generic name of output file}
  img_handle.x_size := view.x_pix;
  img_handle.y_size := view.y_pix;
  r1 := view.x_pix;
  r2 := view.y_pix;
  img_handle.aspect := r1/r2;          {assume square pixels}
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
