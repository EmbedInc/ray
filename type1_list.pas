{   WARNING:  This code is old, and some data structures have changed out from
*   under it.  The source code is here to preserve it, but it is currently not
*   being built.
}

{   All the routines to implement a dumb linear list aggregate object.
}
module type1_list;
define type1_list_class_make;
%include 'ray_type1_2.ins.pas';

type
  obj_block_p_t =                      {pointer to a child object list entry block}
    ^obj_block_t;

  obj_block_t = record                 {template for child object list entry block}
    list_p: obj_block_p_t;             {pointer to list of remaining child blocks}
    obj: ray_object_t;                 {handle to child object}
    end;

  object_data_t = record               {data record pointed to by object block}
    shader: ray_shader_t;              {pointer to shader entry point}
    liparm_p: type1_liparm_p_t;        {pointer to light source parameters block}
    visprop_p: type1_visprop_p_t;      {pointer to visual properties block}
    n_obj: sys_int_machine_t;          {current number of child objects}
    list_p: obj_block_p_t;             {pointer to first child object block in list}
    end;

  object_data_p_t =                    {pointer to object data block}
    ^object_data_t;

procedure type1_list_create (          {create new primitive with custom data}
  out     object: ray_object_t;        {newly filled in object block}
  in      data: type1_list_user_data_t; {parameters from user}
  out     stat: sys_err_t);            {completion status code}
  val_param; forward;

function type1_list_intersect_check (  {check for ray/object intersection}
  in out  ray: type1_ray_t;            {ray descriptor}
  in      object: ray_object_t;        {object to intersect ray with}
  in      uparms: type1_object_parms_t; {run-time parameters passed from above}
  out     hit_info: ray_hit_info_t;    {all returned information}
  out     shader: ray_shader_t)        {pointer to shader to resolve color}
  :boolean;                            {TRUE if ray does hit object}
  val_param; forward;

procedure type1_list_intersect_geom (  {return detailed geometry of intersection}
  in      hit_info: ray_hit_info_t;    {data saved by INTERSECT_CHECK}
  in      flags: ray_geom_flags_t;     {bit mask list of what is being requested}
  out     geom_info: ray_geom_info_t); {filled in geometric info}
  val_param; forward;

procedure type1_list_intersect_box (   {find object/box intersection status}
  in      box:    ray_box_t;           {descriptor for a paralellpiped volume}
  in      object: ray_object_t;        {object to intersect with the box}
  out     here:   boolean;             {TRUE if ISECT_CHECK could be true in box}
  out     enclosed: boolean);          {TRUE if solid obj, and completely encloses box}
  val_param; forward;

procedure type1_list_add_child (       {Add child to this object}
  in      aggr_obj: ray_object_t;      {the object to add child to}
  var     object: ray_object_t);       {the object to add}
  val_param; forward;

procedure type1_list_version (         {return version information about object}
  out     version: ray_object_version_t); {returned version information}
  val_param; forward;
{
****************************************************************************
*
*   Local subroutine TYPE1_LIST_CREATE (OBJECT, DATA, STAT)
*
*   Fill in the new object in OBJECT.  DATA is the user data parameters for
*   this object.  STATUS is the standard system error return code.
}
procedure type1_list_create (          {create new primitive with custom data}
  out     object: ray_object_t;        {newly filled in object block}
  in      data: type1_list_user_data_t; {parameters from user}
  out     stat: sys_err_t);            {completion status code}
  val_param;

var
  data_p: object_data_p_t;             {pointer to internal object data}

begin
  sys_error_none (stat);               {init to no error}

  util_mem_grab (                      {allocate data block for new object}
    sizeof(data_p^), ray_mem_p^, false, data_p);
  object.data_p := ray_obj_data_p_t(data_p); {set pointer to object data block}
  data_p^.shader := data.shader;       {copy pointer to shader to use}
  data_p^.liparm_p := data.liparm_p;   {copy pointer to light source block}
  data_p^.visprop_p := data.visprop_p; {copy pointer to visual properties block}
  data_p^.n_obj := 0;                  {init number of child objects}
  data_p^.list_p := nil;               {init pointer to linked list of children}
  end;
{
****************************************************************************
*
*   Local subroutine TYPE1_LIST_VERSION (VERSION)
*
*   Return version information obout this class of objects.
}
procedure type1_list_version (         {return version information about object}
  out     version: ray_object_version_t); {returned version information}
  val_param;

begin
  version.year := 1987;
  version.month := 10;
  version.day := 25;
  version.hour := 19;
  version.minute := 32;
  version.second := 0;
  version.version := 0;
  version.name := string_v('LIST');
  version.aggregate := true;
  end;
{
****************************************************************************
*
*   Local function TYPE1_LIST_INTERSECT_CHECK (
*     RAY, OBJECT, UPARMS, HIT_INFO, SHADER)
*
*   Check ray and object for an intersection.  If so, return TRUE, and save
*   any partial results in HIT_INFO.  These partial results may be used later
*   to get detailed information about the intersection geometry.
}
function type1_list_intersect_check (  {check for ray/object intersection}
  in out  ray: type1_ray_t;            {ray descriptor}
  in      object: ray_object_t;        {object to intersect ray with}
  in      uparms: type1_object_parms_t; {run-time parameters passed from above}
  out     hit_info: ray_hit_info_t;    {all returned information}
  out     shader: ray_shader_t)        {pointer to shader to resolve color}
  :boolean;                            {TRUE if ray does hit object}
  val_param;

var
  dp: object_data_p_t;                 {pointer to object unique data block}
  hit: boolean;                        {TRUE if found hit so far}
  old_mem: sys_int_adr_t;              {MEM index before any hits}
  new_mem: sys_int_adr_t;              {MEM index after best hit so far}
  child_p: obj_block_p_t;              {pointer to children list entry}
  parms: type1_object_parms_t;         {parms for child intersect checks}

begin
  dp := object_data_p_t(object.data_p); {make local pointer to object data}
  if dp^.shader = nil                  {resolve shader pointer inheritance}
    then parms.shader := uparms.shader
    else parms.shader := dp^.shader;
  if dp^.liparm_p = nil                {resolve LIPARM pointer inheritance}
    then parms.liparm_p := uparms.liparm_p
    else parms.liparm_p := dp^.liparm_p;
  if dp^.visprop_p = nil               {resolve VISPROP pointer inheritance}
    then parms.visprop_p := uparms.visprop_p
    else parms.visprop_p := dp^.visprop_p;
  hit := false;                        {init to no object hit so far}
  child_p := dp^.list_p;               {init pointer to first child block}
  old_mem := next_mem;                 {save MEM index before any hits}

  while child_p <> nil do begin        {once for each child object in our list}
    if child_p^.obj.routines_p^.intersect_check^ ( {run child intersect check routine}
        ray,                           {ray descriptor}
        child_p^.obj,                  {object to insertect ray with}
        parms,                         {run time parameters}
        hit_info,                      {data about the hit}
        shader)                        {shader entry point to use for this hit}
        then begin                     {do this block if ray hit child object}
      hit := true;                     {remember that the ray hit something}
      new_mem := next_mem;             {save MEM index after this hit data}
      next_mem := old_mem;             {restore mem index to before hit data}
      end;                             {done handling if ray hit child}
    child_p := child_p^.list_p;        {point to next child block in our list}
    end;                               {back and try next child}

  type1_list_intersect_check := hit;   {indicate whether there was a hit at all}
  if hit then begin                    {there was a hit}
    next_mem := new_mem;               {set next mem after hit data for this hit}
    end;
  end;
{
****************************************************************************
*
*   Local subroutine TYPE1_LIST_INTERSECT_GEOM (HIT_INFO, FLAGS, GEOM_INFO)
*
*   Return specific geometric information about a ray/object intersection
*   in GEOM_INFO.  In HIT_INFO are any useful results left by the ray/object
*   intersection check calculation.  FLAGS identifies what specific geometric
*   information is being requested.
}
procedure type1_list_intersect_geom (  {return detailed geometry of intersection}
  in      hit_info: ray_hit_info_t;    {data saved by INTERSECT_CHECK}
  in      flags: ray_geom_flags_t;     {bit mask list of what is being requested}
  out     geom_info: ray_geom_info_t); {filled in geometric info}
  val_param;

begin
  writeln ('Intersect geom entry point to LIST object called.');
  sys_bomb;                            {save traceback info and bomb out}
  end;
{
***************************************************************************************
*
*   Local subroutine TYPE1_LIST_INTERSECT_BOX (BOX, OBJECT, HERE, ENCLOSED)
*
*   Find the intersection status between this object and a paralellpiped.
*   HERE is returned as TRUE if the intersect check routine for this object could
*   ever return TRUE for ray within the box volume.  ENCLOSED is returned as true
*   if the object completely encloses the box.
}
procedure type1_list_intersect_box (   {find object/box intersection status}
  in      box:    ray_box_t;           {descriptor for a paralellpiped volume}
  in      object: ray_object_t;        {object to intersect with the box}
  out     here:   boolean;             {TRUE if ISECT_CHECK could be true in box}
  out     enclosed: boolean);          {TRUE if solid obj, and completely encloses box}
  val_param;

var
  obj_here: boolean;                   {HERE flag for subordinate object}
  obj_enclosed: boolean;               {ENCLOSED flag for subordinate object}
  obj_block_p: obj_block_p_t;          {pointer to linked object descriptor}
  data_p: object_data_p_t;             {pointer to specific data for this object}

label
  obj_loop;

begin
  here := false;                       {init returned intersect status flags}
  enclosed := false;
  data_p := object_data_p_t(object.data_p); {get pointer to our data block}
  obj_block_p := data_p^.list_p;       {get pointer to first child object block}

obj_loop:                              {back here each new child pointer}
  if obj_block_p = nil then return;    {no more child objects left to check ?}
  obj_block_p^.obj.routines_p^.intersect_box^ ( {call child's box intersector}
    box,                               {the box to intersect object with}
    obj_block_p^.obj,                  {object to intersect box with}
    obj_here,                          {HERE flag for child object}
    obj_enclosed);                     {ENCLOSED flag for child object}
  here := here or obj_here;            {merge in child's here status}
  enclosed := enclosed or obj_enclosed; {merge in child's enclosed status}
  if here and enclosed then return;    {can't change from this state anyway}
  obj_block_p := obj_block_p^.list_p;  {advance pointer to next child descriptor}
  goto obj_loop;                       {back and process new child}
  end;
{
****************************************************************************
*
*   Local subroutine TYPE1_LIST_ADD_CHILD (AGGR_OBJ, OBJECT)
}
procedure type1_list_add_child (       {Add child to this object}
  in      aggr_obj: ray_object_t;      {the object to add child to}
  var     object: ray_object_t);       {the object to add}
  val_param;

var
  obj_p: obj_block_p_t;                {pointer to new child block}
  data_p: object_data_p_t;             {pointer to internal object data}

begin
  data_p := object_data_p_t(aggr_obj.data_p); {make local pointer to our data}
  util_mem_grab (                      {allocate storage for new list entry}
    sizeof(obj_p^), ray_mem_p^, false, obj_p);
  obj_p^.obj := object;                {copy object handle into list entry}
  obj_p^.list_p := data_p^.list_p;     {make new entry point to rest of list}
  data_p^.list_p := obj_p;             {set start of list pointer to new entry}
  data_p^.n_obj := data_p^.n_obj;      {count one more object in list}
  end;
{
********************************************************************************
*
*   Subroutine TYPE1_LIST_CLASS_MAKE (CLASS)
*
*   Fill in the routines block for this class of objects.
}
procedure type1_list_class_make (      {fill in object class descriptor}
  out     class: ray_object_class_t);  {block to fill in}
  val_param;

begin
  class.create := addr(type1_list_create);
  class.intersect_check := addr(type1_list_intersect_check);
  class.hit_geom := addr(type1_list_intersect_geom);
  class.intersect_box := addr(type1_list_intersect_box);
  class.add_child := addr(type1_list_add_child);
  end;
