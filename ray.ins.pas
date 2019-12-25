{   Public include file for the base part of the ray tracer.
*
*   The high level routines and the basic interfaces between the components are
*   defined here.  This does not include details of ray geometry, what a color
*   is, and the like.  No actual ray-tracable objects or shaders are included.
*
*   Any actual ray tracer implementation must provide objects, shaders, and
*   define color.  Public symbols for these are intended to be named TYPEn,
*   where each set of mutually exclusive routines uses a different N.
*
*   This ray tracer library comes with TYPE1 routines.  These implement "normal"
*   geometry and colors.  See the RAY_TYPE1 include file for details.
}
type
  ray_context_p_t = ^ray_context_t;    {pointer to static context for rays}

  ray_object_p_t = ^ray_object_t;
  ray_object_t = record                {one instance of an object}
    routines_p: ^ray_object_routines_t; {pointer to static object information}
    data_p: univ_ptr;                  {pointer to object-specific data for this instance}
    end;

  ray_desc_t = record                  {minimum required data for a ray}
    context_p: ray_context_p_t;        {pointer to static context info}
    end;

  ray_hit_info_t = record              {data from object INTERSECT_CHECK routines}
    object_p: ray_object_p_t;          {points to object that the ray hit}
    distance: real;                    {ray distance to hit point}
    shader_parms_p: univ_ptr;          {pointer to parameters for the shader}
    enter: boolean;                    {TRUE if ray is entering object, not leaving}
    end;

  ray_shader_t = ^procedure (          {resolve color given hit info}
    in var  ray: univ ray_desc_t;      {the ray that hit the object}
    in var  hit_info: ray_hit_info_t;  {saved info about the ray/object intersection}
    out     color: univ sys_int_machine_t); {returned color, defined by TYPEn convention}

  ray_context_t = record               {static context for each ray}
    top_level_obj_p: ray_object_p_t;   {pointer to top level "world" object}
    object_parms_p: univ_ptr;          {points to parameters for top level object}
    backg_shader: ray_shader_t;        {pointer to shader routine for background color}
    backg_hit_info: ray_hit_info_t;    {hit block for when ray hit nothing}
    end;

  ray_geom_flag_values = (             {all separately requestable isect geom parms}
    ray_geom_point,                    {XYZ coordinates of intersection point}
    ray_geom_unorm                     {unit normal vector to surface}
    );
  ray_geom_flags_t =                   {set of all requestable intersect geometry values}
    set of ray_geom_flag_values;

  ray_geom_info_t = record             {returned intersection geometry information}
    point: vect_3d_t;                  {coordinate of intersection point}
    unorm: vect_3d_t;                  {unit normal vector of surface}
    flags: ray_geom_flags_t;           {flags for what really got returned}
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
    in out  object: ray_object_t;      {object instance to be filled in}
    in      gcrea_p: univ_ptr;         {data for creating this object instance}
    out     stat: sys_err_t);          {completion status code}
    val_param;

  ray_object_version_proc_t = ^procedure ( {return version info of this object}
    out     version: ray_object_version_t);
    val_param;

  ray_object_isect_check_proc_t = ^function ( {check for ray hit this object}
    in out  gray: univ ray_desc_t;     {ray to intersect with object}
    in var  object: ray_object_t;      {object to intersect ray with}
    in      gparms_p: univ_ptr;        {pointer to run time TYPEn-specific params}
    out     hit_info: ray_hit_info_t;  {returned intersection info}
    out     shader: ray_shader_t)      {pointer to shader to resolve color here}
    :boolean;                          {TRUE if ray hit this object}
    val_param;

  ray_object_isect_geom_proc_t = ^procedure ( {get detailed intersect geometry}
    in      hit_info: ray_hit_info_t;  {info about the intersection}
    in      flags: ray_geom_flags_t;   {flags for the requested parameters}
    out     geom_info: ray_geom_info_t); {returned geometry info}
    val_param;

  ray_object_isect_box_proc_t = ^procedure ( {return object/box intersect status}
    in      box: ray_box_t;            {descriptor for a paralellpiped volume}
    in      object: ray_object_t;      {object to intersect with the box}
    out     here: boolean;             {TRUE if ISECT_CHECK could be true in box}
    out     enclosed: boolean);        {TRUE if solid obj and completely encloses box}
    val_param;

  ray_object_add_child_proc_t = ^procedure ( {add child to an aggregate object}
    in      aggr_obj: ray_object_t;    {aggregate object to add child to}
    var     object: ray_object_t);     {child object to add to aggregate object}
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

  ray_object_routines_make_t = ^procedure ( {fill in pointers to obj entry points}
    out     pointers: ray_object_routines_t); {block to fill in}
    val_param;
{
*   Subroutines.
}
procedure ray_close;                   {close use of ray tracer, release resources}
  val_param; extern;

procedure ray_init (                   {init ray tracer, must be first call}
  in out  parent_mem: util_mem_context_t); {all new ray mem will be below this}
  val_param; extern;

function ray_mem_alloc_perm (          {alloc un-releasable mem under ray tracer context}
  in      size: sys_int_adr_t)         {amount of memory to allocate}
  :univ_ptr;                           {pnt to new mem, bombs prog on no mem}
  val_param; extern;

procedure ray_trace (
  in out  ray: univ ray_desc_t;        {the ray to trace}
  out     color: univ sys_int_machine_t); {returned color, format defined by TYPEn}
  val_param; extern;
