{   Public include file for the type 1 implementation of the ray tracer.
*
*   The base ray tracer does not define any tracable objects, shaders, or the
*   format of color values.  This is done by specific implementations, intended
*   to be named TYPEn.  See the RAY.INS.PAS file for details.
*
*   This type 1 implementation is intended to be reasonably "normal".  It is
*   included with the overall ray tracer package.
}
const
  type1_max_light_sources_k = 8;       {max number of simultaneous light sources}
  type1_max_generation_k = 8;          {max recursive ray generations allowed}
  type1_min_energy_k = 0.001;          {min energy for ray to be valid}
  type1_3dfield_max_iso_vals_k = 8;    {max number of allowed iso-values in 3DFIELD}

type
  type1_color_t = record               {one color}
    red: real;                         {0.0 to 1.0 color components}
    grn: real;
    blu: real;
    alpha: real;                       {0.0 to 1.0 opacity fraction}
    end;

  type1_ray_p_t = ^type1_ray_t;
  type1_ray_t = record                 {data for one ray}
    base: ray_base_t;                  {mandatory ray data}
    point: vect_3d_t;                  {ray origin}
    vect: vect_3d_t;                   {DX, DY, DZ unit ray vector}
    generation: sys_int_machine_t;     {generation counter (eye ray = 1)}
    energy: real;                      {0.0 to 1.0 contribution into first gen ray}
    min_dist: real;                    {minimum distance to valid intersection}
    max_dist: real;                    {maximum distance to valid intersection}
    end;

  type1_ltype_k_t = (                  {ID for each type of light source}
    type1_ltype_off_k,                 {this light source turned off}
    type1_ltype_ambient_k,             {ambient light, same from all directions}
    type1_ltype_directional_k,         {directional (point at infinity)}
    type1_ltype_point_constant_k,      {point light source, no fall off}
    type1_ltype_point_r2_k);           {point light source, 1/r**2 falloff}

  type1_light_t = record               {template for one light source}
    ltype: type1_ltype_k_t;            {light source type, use TYPE1_LTYPE_xxx_K}
    case type1_ltype_k_t of            {different data for each kind of light source}
      type1_ltype_off_k: (             {this light source is turned off}
        );
      type1_ltype_ambient_k: (         {ambient light, same from all directions}
        amb_red: real;                 {light source color intensity values}
        amb_grn: real;
        amb_blu: real;
        );
      type1_ltype_directional_k: (     {point light at infinity, no fall off}
        dir_red: real;                 {light source color intensity values}
        dir_grn: real;
        dir_blu: real;
        dir_uvect: vect_3d_t;          {unit vector towards light source}
        );
      type1_ltype_point_constant_k: (  {point light with no fall off}
        pcon_red: real;                {light source color intensity values}
        pcon_grn: real;
        pcon_blu: real;
        pcon_coor: vect_3d_t;          {world coordinates of light source}
        );
      type1_ltype_point_r2_k: (        {point light with 1/R**2 falloff}
        pr2_red: real;                 {light source color intensity values at R = 1}
        pr2_grn: real;
        pr2_blu: real;
        pr2_coor: vect_3d_t;           {world coordinates of light source}
        );
    end;

  type1_liparm_p_t = ^type1_liparm_t;
  type1_liparm_t = record              {light source parameters descriptor block}
    n_lights: sys_int_machine_t;       {number of light sources actually defined}
    light:                             {array of all the light sources}
      array[1..type1_max_light_sources_k] of type1_light_t;
    end;

  type1_visprop_p_t = ^type1_visprop_t;
  type1_visprop_t = record             {visual properties description block}
    back_p: type1_visprop_p_t;         {visual properties of back side, may be NIL}
    emis_red: real;                    {emissive color}
    emis_grn: real;
    emis_blu: real;
    diff_red: real;                    {diffuse color}
    diff_grn: real;
    diff_blu: real;
    spec_red: real;                    {specular color}
    spec_grn: real;
    spec_blu: real;
    spec_exp: sys_int_machine_t;       {specular exponent}
    opac_front: real;                  {opacity when facing head on}
    opac_side: real;                   {opacity when at limb curve}
    diff_on: boolean;                  {diffuse reflections on/off flag}
    spec_on: boolean;                  {specular reflections on/off flag}
    opac_on: boolean;                  {transparency on/off flag}
    end;

  type1_hit_geom_p_t = ^type1_hit_geom_t;
  type1_hit_geom_t = record            {TYPE1 minimum hit geometry save block}
    liparm_p: type1_liparm_p_t;        {pointer to light source parameters block}
    visprop_p: type1_visprop_p_t;      {pointer to visual properties block}
    end;

  type1_object_parms_p_t = ^type1_object_parms_t;
  type1_object_parms_t = record        {runtime parameters for INTERSECT_CHECK}
    shader: ray_shader_t;              {pointer to shader entry point, may = NIL}
    liparm_p: type1_liparm_p_t;        {pointer to lightsource block, may = NIL}
    visprop_p: type1_visprop_p_t;      {pointer to VISPROP block, may = NIL}
    end;
{
********************************************************************************
*
*   Shaders.  These routines resolve the color apparent along a ray, given the
*   specifics of a ray/object intersection.
}
type
  type1_shader_fixed_data_p_t = ^type1_shader_fixed_data_t;
  type1_shader_fixed_data_t = record   {SHADER PARMS for fixed backg shader}
    col: type1_color_t;                {the fixed color to return}
    liparm_p: type1_liparm_p_t;        {pointer to light source descriptors}
    end;

procedure type1_shader_fixed (         {always returns fixed color, used for backg}
  in var  ray: type1_ray_t;            {handle to the ray}
  in var  hit_info: ray_hit_info_t;    {info about specific intersection}
  out     color: type1_color_t);       {returned ray color}
  extern;

procedure type1_shader_phong (         {shader using Phong lighting model}
  in var  ray: type1_ray_t;            {handle to the ray}
  in var  hit_info: ray_hit_info_t;    {info about specific intersection}
  out     color: type1_color_t);       {returned ray color}
  extern;
{
********************************************************************************
*
*   Objects.  There are two basic types of objects, aggregate and direct.
*
*   Aggregate objects hold other objects.  Intersecting a ray with a aggregate
*   object causes the ray to be intersected against the contained objects
*   instead.
*
*   Direct objects are the actual geometric entities that are directly visible.
}
{
********************
*
*   Triangle.
}
type
  type1_tri_flags_k_t = (              {per-triangle separate flags}
    type1_tri_flag_rgb_k,              {explicit diffuse RGB for each vertex}
    type1_tri_flag_alpha_k,            {explicit alpha for each vertex}
    type1_tri_flag_shnorm_k,           {shading normal is present for each vertex}
    type1_tri_flag_rgba_k);            {RGBA stored, used internally}

  type1_tri_flags_t =                  {all the triangle flags in one set}
    set of type1_tri_flags_k_t;

  type1_tri_opt_vert_t = record        {all the optional data about a triangle vert}
    red, grn, blu: real;               {explicit diffuse 0.0 to 1.0 color}
    alpha: real;                       {explicit 0.0 to 1.0 opacity fraction}
    shnorm: vect_3d_t;                 {shading normal vector}
    end;

  type1_tri_crea_data_p_t = ^type1_tri_crea_data_t;
  type1_tri_crea_data_t = record       {creation data for TRI (triangle) object}
    visprop_p: type1_visprop_p_t;      {pointer to visprop block, may be NIL}
    p1, p2, p3: vect_3d_t;             {verticies, counterclockwise from front face}
    flags: type1_tri_flags_t;          {indicates what optional per-vertex data used}
    case integer of                    {optional data about each vertex}
      1: (                             {explicit names for each vertex}
        v1, v2, v3: type1_tri_opt_vert_t);
      2: (                             {array for indexing in a loop}
        v: array[1..3] of type1_tri_opt_vert_t);
    end;

procedure type1_tri_routines_make (    {fill in object routines block}
  out     class: ray_object_class_t);  {block to fill in}
  val_param; extern;
{
********************
*
*   Sphere.
}
type
  type1_sphere_crea_data_p_t = ^type1_sphere_crea_data_t;
  type1_sphere_crea_data_t = record    {creation data for SPHERE object}
    visprop_p: type1_visprop_p_t;      {pointer to visprop block, may be NIL}
    center: vect_3d_t;                 {X,Y,Z center of sphere}
    radius: real;                      {radius of sphere}
    end;

procedure type1_sphere_routines_make ( {fill in object routines block}
  out     class: ray_object_class_t);  {block to fill in}
  val_param; extern;
{
********************
*
*   3D scaler field.
*
*   The visible objects are iso-surfaces implied by the scaler field.
}
type
  type1_3dfield_iso_t = record         {description for one iso-surface}
    val: real;                         {data iso-value}
    visprop_p: type1_visprop_p_t;      {may be NIL to indicate inherited}
    end;

  type1_3dfield_crea_data_p_t = ^type1_3dfield_crea_data_t;
  type1_3dfield_crea_data_t = record   {creation data for 3DFIELD object}
    shader: ray_shader_t;              {shader entry pointer, may be NIL}
    liparm_p: type1_liparm_p_t;        {pointer to light source block, may be NIL}
    visprop_p: type1_visprop_p_t;      {pointer to visprop block, may be NIL}
    i, j, k: sys_int_machine_t;        {size of 3D array for each dimension}
    array_adr: univ_ptr;               {address of first array entry}
    val_offset: sys_int_adr_t;         {byte offset for value into array entry}
    min_gen: sys_int_machine_t;        {minimum subdivision level for non-empty voxels}
    max_gen: sys_int_machine_t;        {max allowed voxel subdivision level}
    n_iso: sys_int_machine_t;          {number of iso-surfaces to draw}
    iso:                               {descriptor for each possible iso value}
      array[1..type1_3dfield_max_iso_vals_k] of type1_3dfield_iso_t;
    end;

procedure type1_3dfield_routines_make ( {fill in object routines block}
  out     class: ray_object_class_t);  {block to fill in}
  val_param; extern;
{
********************
*
*   Octree aggregate object.
*
*   This object adaptively subdivides the outer bounding box recursively into 8
*   equal sub-boxes to minimize the objects in each final box.  Ray are quickly
*   traced thru empty voxels.  Non-empty voxels should have been divided enough
*   so that each contain few objects to check for intersection with the ray.
}
type
  type1_octree_crea_data_p_t = ^type1_octree_crea_data_t;
  type1_octree_crea_data_t = record    {creation data for OCTREE object}
    shader: ray_shader_t;              {shader entry pointer, may be NIL}
    liparm_p: type1_liparm_p_t;        {pointer to light source block, may be NIL}
    visprop_p: type1_visprop_p_t;      {pointer to visprop block, may be NIL}
    min_gen: sys_int_machine_t;        {minimum subdivision level for non-empty voxels}
    max_gen: sys_int_machine_t;        {max allowed voxel subdivision level}
    min_miss: sys_int_machine_t;       {min misses before subdividing node with 1 obj}
    origin: vect_3d_t;                 {most negative corner point for all 3 axis}
    size: vect_3d_t;                   {outer octree dimension for each axis}
    end;

procedure type1_octree_routines_make ( {fill in object routines block}
  out     class: ray_object_class_t);  {block to fill in}
  val_param; extern;

procedure type1_octree_geom (          {back door to stomp on octree geometry}
  in      origin: vect_3d_t;           {most negative corner for all 3 axies}
  in      size: vect_3d_t;             {outer octree dimension for each axis}
  in      object: ray_object_t);       {handle to specific octree object}
  val_param; extern;
{
********************
*
*   Octree data object.
*
*   This object shows the voxel structure of a octree object (above), as apposed
*   to the objects within the octree.  This can be useful to show or check how
*   the octree recursive subdivision worked.
}
type
  type1_octree_data_crea_data_p_t = ^type1_octree_data_crea_data_t;
  type1_octree_data_crea_data_t = record {creation data for OCTREE DATA object}
    shader: ray_shader_t;              {shader entry pointer, may be NIL}
    liparm_p: type1_liparm_p_t;        {pointer to light source block, may be NIL}
    visprop_p: type1_visprop_p_t;      {pointer to visprop block, may be NIL}
    oct_obj_p: ray_object_p_t;         {pointer to OCTREE object to display}
    box_size: real;                    {0.0 to 1.0 relative size of displayed voxel}
    show_objects: boolean;             {TRUE if also to display objects in octree}
    unused1: boolean;
    unused2: boolean;
    unused3: boolean;
    end;

procedure type1_octree_data_routines_make ( {fill in object routines block}
  out     class: ray_object_class_t);  {block to fill in}
  val_param; extern;
{
********************
*
*   List aggregate object.
*
*   All the sub-objects are simply stored in a list, and rays are checked for
*   intersection by scanning thru the whole list.  This object is not intended
*   for making final ray-traced images.  It can be useful for research purposes,
*   as a comparison against various speedup techniques.  The code also serves as
*   a template for a minimal aggregate object.
}
type
  type1_list_crea_data_p_t = ^type1_list_crea_data_t;
  type1_list_crea_data_t = record      {creation data for LIST object}
    shader: ray_shader_t;              {shader entry pointer, may be NIL}
    liparm_p: type1_liparm_p_t;        {pointer to light source block, may be NIL}
    visprop_p: type1_visprop_p_t;      {pointer to visprop block, may be NIL}
    end;

procedure type1_list_routines_make (   {fill in object routines block}
  out     class: ray_object_class_t);  {block to fill in}
  val_param; extern;
