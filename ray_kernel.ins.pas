{   Ray tracer kernel.
*
*   This include file defines all the data structures that are dictated by the
*   ray tracer kernel.  All other data structures are defined by convention
*   between the object routines, the shaders, and the calling program to
*   RAY_TRACE.
}
type
{
******************************
*
*   Dynamic ray descriptor.
*
*   The information about any one ray is broken into two data structures, the
*   dynamic part (RAY_DESC_T) and the static part (RAY_CONTEXT_T).  The
*   information that remains constant for all rays with a common ancestor goes
*   into the static descriptor, and only that information that changes between
*   rays of common ancestry goes into the dynamic ray descriptor.  A ray should
*   always be referred to by its dynamic ray descriptor, since this points to
*   the context information necessary to give meaning to the ray info.
*
*   The templates shown here only contain the mandatory part of the ray
*   descriptors dictated by the kernel.  The real ray descriptors MUST start
*   with this data, but the remainder is defined by convention between the
*   object routines, the shader routines, and the calling routine to RAY_TRACE.
}
  ray_context_p_t =                    {pointer to ray context block}
    ^ray_context_t;

  ray_desc_t = record                  {mandatory part of dynamic ray descriptor}
    context_p: ray_context_p_t;        {pointer to static context info}
    end;
{
******************************
*
*   SHADER related data structures.
}
  ray_object_p_t =                     {pointer to an object}
    ^ray_object_t;

  ray_shader_parms_p_t =               {pointer to specific data for a shader}
    ^ray_shader_parms_t;

  ray_hit_info_t = record              {data from object INTERSECT_CHECK routines}
    object_p: ray_object_p_t;          {points to object that the ray hit}
    distance: real;                    {ray distance to hit point}
    shader_parms_p: ray_shader_parms_p_t; {pointer to parameters for the shader}
    enter: boolean;                    {TRUE if ray is entering object, not leaving}
    end;

  ray_color_t =                        {template for application-specific color}
    integer32;                         {kernel just deals with pointers to this}

ray_shader_t = ^procedure (            {resolve color given hit info}
  in var  ray: univ ray_desc_t;        {unique info about this ray}
  in var  hit_info: univ ray_hit_info_t; {unique info about this ray/object hit}
  out     color: univ ray_color_t);    {each shader can have its own color format}

  ray_shader_parms_t =                 {template for run time data for shader}
    integer32;                         {kernel just deals with pointers to this}
{
******************************
*
*   Ray context information.  This data remains constant for any family of rays
*   with a common ancestry.  It would remain constant for an entire image in a
*   "normal" ray tracer.
}
  ray_object_parms_p_t =               {pointer to run-time object specific data}
    ^ray_object_parms_t;

  ray_context_t = record               {mandatory part of ray context information}
    top_level_obj_p: ray_object_p_t;   {pointer to top level "world" object}
    object_parms_p: ray_object_parms_p_t; {world obj specific data parms pointer}
    backg_shader: ray_shader_t;        {pointer to shader routine for background color}
    backg_hit_info: ray_hit_info_t;    {hit block for when ray hit nothing}
    end;
{
******************************
*
*   OBJECT related data structures.
}
  ray_crea_data_p_t = ^ray_crea_data_t;
  ray_crea_data_t = sys_int_machine_t; {generic structure for create-time object data,
                                        use specific xxx_CREA_DATA_T structures}

  ray_object_parms_t =                 {run-time data for object INTERSECT_CHECK proc}
    integer32;                         {kernel only deals with pointers to this}

  ray_geom_flag_values = (             {all separately requestable isect geom parms}
    ray_geom_point,                    {XYZ coordinates of intersection point}
    ray_geom_unorm                     {unit normal vector to surface}
    );
  ray_geom_flags_t =                   {bit field of all RAY_GEOM_FLAG_VALUES}
    set of ray_geom_flag_values;

  ray_geom_info_t = record             {returned geometry information}
    point: vect_3d_t;                  {X,Y,Z coordinate of intersection point}
    unorm: vect_3d_t;                  {DX,DY,DZ unit normal vector of surface}
    flags: ray_geom_flags_t;           {flags for what really got returned}
    unused1: boolean;
    unused2: boolean;
    end;

  ray_object_t = record                {all the object-specific information}
    routines_p: ^ray_object_routines_t; {pointer to generic class of routines}
    data_p: univ_ptr;                  {pointer to object-specfic data}
    end;

  ray_object_version_t = record        {version and ID information about an object}
    year: integer32;                   {full 4-digit year of binary build}
    month: integer32;                  {1-12 month}
    day: integer32;                    {day within month}
    hour: integer32;                   {whole hours since midnight (24 hour clock)}
    minute: integer32;                 {minute within hour}
    second: integer32;                 {second within minute}
    version: integer32;                {arbitrary version ID}
    name: string_var80_t;              {up to 80 character object name}
    aggregate: boolean;                {true if object can hold other objects}
    unused1: boolean;
    unused2: boolean;
    unused3: boolean;
    end;
{
*   Box descriptor and associated data structures.
*
*   RAY_BOX_T defines a paralellpiped volume that is used in doing object/box
*   intersection checks.  It contains redundant information to allow for faster
*   object/box intersection checks.  The basic box is defined by a corner point
*   and the vectors along the three edges intersecting at the corner point.  The
*   UNORM and WIDTH fields supply redundant information, but make it easy to
*   determine whether a point is in the slice of space between the planes of two
*   opposite faces.  For a rectangular box, UNORM and EDGE point in the same
*   direction, and WIDTH is the magnitued of EDGE.  Intersection routines,
*   however, should not assume that the box is rectangular.  Even a axis aligned
*   cube can turn into an arbitrary paralelpiped after tranformation.
}
  ray_box_edge_t = record              {used for describing one edge of a box}
    edge: vect_3d_t;                   {vector from POINT (below) along box edge}
    unorm: vect_3d_t;                  {unit normal to box face not along this edge}
    width: real;                       {dist along UNORM between the opposite faces}
    end;

  ray_box_t = record                   {describes a paralellpiped volume}
    point: vect_3d_t;                  {one of the corner points of the box}
    edge:                              {describes the 3 independent sets of edges}
      array[1..3] of ray_box_edge_t;
    end;
{
*   All the entry points that are pointed to from the OBJECT_ROUTINES block.
}
ray_object_create_proc_t = ^procedure ( {create a new object}
  in out  object: ray_object_t;        {object to be filled in}
  in var  crea: univ ray_crea_data_t;  {specific build-time data for this object}
  out     stat: sys_err_t);            {completion status code}
  val_param;

ray_object_version_proc_t = ^procedure ( {return version info of this object}
  out     version: ray_object_version_t);
  val_param;

ray_object_isect_check_proc_t = ^function ( {check for ray hit this object}
  in out  gray: univ ray_desc_t;       {input ray descriptor}
  in var  object: ray_object_t;        {input object to intersect ray with}
  in var  gparms: univ ray_object_parms_t; {run time obj-specific parameters}
  out     hit_info: ray_hit_info_t;    {handle to routines and data to get hit color}
  out     shader: ray_shader_t)        {pointer to shader to resolve color here}
  :boolean;                            {TRUE if ray hit this object}
  val_param;

ray_object_isect_geom_proc_t = ^procedure ( {get detailed intersect geometry}
  in      hit_info: ray_hit_info_t;    {info about the intersection}
  in      flags: ray_geom_flags_t;     {indicate what parameters are sought}
  out     geom_info: ray_geom_info_t); {returned geometry info}
  val_param;

ray_object_isect_box_proc_t = ^procedure ( {return object/box intersect status}
  in      box: ray_box_t;              {descriptor for a paralellpiped volume}
  in      object: ray_object_t;        {object to intersect with the box}
  out     here: boolean;               {TRUE if ISECT_CHECK could be true in box}
  out     enclosed: boolean);          {TRUE if solid obj and completely encloses box}
  val_param;

ray_object_add_child_proc_t = ^procedure ( {add child to an aggregate object}
  in      aggr_obj: ray_object_t;      {aggregate object to add child to}
  var     object: ray_object_t);       {child object to add to aggregate object}
  val_param;
{
*   Object routines block.
*
*   This contains an exhaustive list of all the routines that perform any action
*   requiring special knowlege about the object.
}
  ray_object_routines_t = record
    create: ray_object_create_proc_t;  {create a new object}
    version: ray_object_version_proc_t; {return version ID of this object}
    intersect_check: ray_object_isect_check_proc_t; {ray/object hit ?}
    intersect_geom: ray_object_isect_geom_proc_t; {get detailed hit geometry}
    intersect_box: ray_object_isect_box_proc_t; {get box/object intersect status}
    add_child: ray_object_add_child_proc_t; {add child to aggregate object}
    end;
{
*   Define template for routine to create object routines block.  The address of
*   these routines are probably found by looking into the symbol table at run
*   time.  This routine(s) will fill in all the entry points existing for the
*   particular object into the appropriate places in the POINTERS block.  Any
*   extra entries in the POINTERS block will be set to the nil pointer to
*   signify that the particular object does not perform that operation.
}
  ray_object_routines_make_t = ^procedure ( {fill in pointers to obj entry points}
    out     pointers: ray_object_routines_t); {block to fill in}
    val_param;
{
******************************
*
*   RAY_TRACE (RAY, COLOR)
*
*   This is the main recursive entry point to the ray tracing kernel.  RAY_TRACE
*   is given a ray, and resolves its color.  This subroutine may be called
*   recursively by shaders that launch more rays as part of their job to
*   determine ray color given an intersection point.  Note that the format of
*   the returned color value is not dictated by the kernel.  RAY_TRACE really
*   just gets it from the shaders and passes it along.  The format of COLOR is
*   set by convention between the caller of RAY_TRACE and the shaders.
}
procedure ray_trace (
  in out  ray: univ ray_desc_t;        {everything you need to know about one ray}
  out     color: univ ray_color_t);    {returned color}
  val_param; extern;
