{   All the routines to implement a dumb sphere object.
}
module type1_sphere;
define type1_sphere_routines_make;
%include 'ray_type1_2.ins.pas';

type
  object_data_t = record               {data record pointed to by object block}
    visprop_p: type1_visprop_p_t;      {pointer to visual properties block}
    center: vect_3d_fp1_t;             {XYZ coordinates of center point}
    radius: single;                    {sphere radius}
    radius2: single;                   {sphere radius squared}
    end;

  object_data_p_t =                    {pointer to object data block}
    ^object_data_t;

  object_hit_info_t = record           {saved data from intersection check}
    base: type1_hit_info_t;            {mandatory part of hit geometry save area}
    ray_point: vect_3d_t;              {ray origin point in object's space}
    ray_vect: vect_3d_t;               {ray unit vector in object's space}
    end;

  object_hit_geom_p_t =                {pointer to hit geometry block}
    ^object_hit_info_t;

procedure type1_sphere_create (        {create new primitive with custom data}
  out     object: ray_object_t;        {newly filled in object block}
  in      data: type1_sphere_user_data_t; {parameters from user}
  out     stat: sys_err_t);            {completion status code}
  val_param; forward;

procedure type1_sphere_version (       {return version information about object}
  out     version: ray_object_version_t); {returned version information}
  val_param; forward;

function type1_sphere_intersect_check ( {check for ray/object intersection}
  in out  ray: type1_ray_t;            {ray descriptor}
  in      object: ray_object_t;        {object to intersect ray with}
  in      parms: type1_object_parms_t; {run-time parameters passed from above}
  out     hit_info: ray_hit_info_t;    {all returned information}
  out     shader: ray_shader_t)        {pointer to shader to resolve color}
  :boolean;                            {TRUE if ray does hit object}
  val_param; forward;

procedure type1_sphere_intersect_geom ( {return detailed geometry of intersection}
  in      hit_info: ray_hit_info_t;    {data saved by INTERSECT_CHECK}
  in      flags: ray_geom_flags_t;     {bit mask list of what is being requested}
  out     geom_info: ray_geom_info_t); {filled in geometric info}
  val_param; forward;

procedure type1_sphere_intersect_box ( {find object/box intersection status}
  in      box: ray_box_t;              {descriptor for a paralellpiped volume}
  in      object: ray_object_t;        {object to intersect with the box}
  out     here: boolean;               {TRUE if ISECT_CHECK could be true in box}
  out     enclosed: boolean);          {TRUE if solid obj, and completely encloses box}
  val_param; forward;

procedure type1_sphere_add_child (     {Add child to aggregate object (illegal)}
  in      object: ray_object_t;        {the object to add}
  in      parms: univ integer32);      {unused optional parameters}
  val_param; forward;
{
****************************************************************************
*
*   Subroutine TYPE1_SPHERE_ROUTINES_MAKE (POINTERS, SIZE)
*
*   Fill in the routines block for this class of objects.  SIZE is the size in bytes
*   of the data structure to be filled in.
}
procedure type1_sphere_routines_make ( {fill in object routines block}
  out     pointers: ray_object_routines_t; {block to fill in}
  in      size: sys_int_adr_t);        {number of bytes in POINTERS}
  val_param;

var
  ents: sys_int_machine_t;             {number of routine entries in POINTERS}
  i: sys_int_machine_t;                {loop counter}
  max_ofs: sys_int_machine_t;          {byte offset of last entry in POINTERS}
  p: ^ray_object_create_proc_t;        {pointer to a subroutine entry point}

begin
  ents := size div sizeof(p^);         {number of pointers in block}
  p := univ_ptr(addr(pointers));       {init pointer to first entry in POINTERS}
  for i := 1 to ents do begin          {once for each slot in POINTERS}
    p^ := nil;                         {init this slot in POINTERS to the nil pointer}
    p := univ_ptr(                     {point to next slot in POINTERS}
      sys_int_adr_t(p) + sizeof(p^));
    end;
  max_ofs := (ents - 1) * sizeof(univ_ptr); {byte offset of last entry}

  if (sys_int_adr_t(addr(pointers.version))-sys_int_adr_t(addr(pointers)))
    <= max_ofs                         {this slot within POINTERS ?}
    then pointers.version :=
      ray_object_version_proc_t(addr(type1_sphere_version));
    ;
  if (sys_int_adr_t(addr(pointers.create))-sys_int_adr_t(addr(pointers)))
    <= max_ofs                         {this slot within POINTERS ?}
    then pointers.create :=
      ray_object_create_proc_t(addr(type1_sphere_create));
    ;
  if (sys_int_adr_t(addr(pointers.intersect_check))-sys_int_adr_t(addr(pointers)))
    <= max_ofs                         {this slot within POINTERS ?}
    then pointers.intersect_check :=
      ray_object_isect_check_proc_t(addr(type1_sphere_intersect_check));
    ;
  if (sys_int_adr_t(addr(pointers.intersect_geom))-sys_int_adr_t(addr(pointers)))
    <= max_ofs                         {this slot within POINTERS ?}
    then pointers.intersect_geom :=
      ray_object_isect_geom_proc_t(addr(type1_sphere_intersect_geom));
    ;
  if (sys_int_adr_t(addr(pointers.intersect_box))-sys_int_adr_t(addr(pointers)))
    <= max_ofs                         {this slot within POINTERS ?}
    then pointers.intersect_box :=
      ray_object_isect_box_proc_t(addr(type1_sphere_intersect_box));
    ;
  if (sys_int_adr_t(addr(pointers.add_child))-sys_int_adr_t(addr(pointers)))
    <= max_ofs                         {this slot within POINTERS ?}
    then pointers.add_child :=
      ray_object_add_child_proc_t(addr(type1_sphere_add_child));
    ;
  end;
{
****************************************************************************
*
*   Local subroutine TYPE1_SPHERE_CREATE (OBJECT, DATA, STATUS)
*
*   Fill in the new object in OBJECT.  DATA is the user data parameters for
*   this object.  STATUS is the standard system error return code.
}
procedure type1_sphere_create (        {create new primitive with custom data}
  out     object: ray_object_t;        {newly filled in object block}
  in      data: type1_sphere_user_data_t; {parameters from user}
  out     stat: sys_err_t);            {completion status code}
  val_param;

var
  data_p: object_data_p_t;             {pointer to internal object data}

begin
  sys_error_none (stat);               {init to no error}

  util_mem_grab (                      {allocate data block for new object}
    sizeof(data_p^), ray_mem_p^, false, data_p);
  object.data_p := ray_obj_data_p_t(data_p); {set pointer to object data block}
  data_p^.visprop_p := data.visprop_p; {copy pointer to visual properties block}
  data_p^.center.x := data.center.x;   {copy coordinate of sphere center}
  data_p^.center.y := data.center.y;
  data_p^.center.z := data.center.z;
  data_p^.radius := data.radius;       {copy sphere radius}
  data_p^.radius2 := sqr(data.radius); {save radius squared for speedup}
  end;
{
****************************************************************************
*
*   Local subroutine TYPE1_SPHERE_VERSION (VERSION)
*
*   Return version information obout this class of objects.
}
procedure type1_sphere_version (       {return version information about object}
  out     version: ray_object_version_t); {returned version information}
  val_param;

begin
  version.year := 1997;
  version.month := 1;
  version.day := 28;
  version.hour := 14;
  version.minute := 15;
  version.second := 0;
  version.version := 0;
  version.name := string_v('SPHERE');
  version.aggregate := false;
  end;
{
****************************************************************************
*
*   Local function TYPE1_SPHERE_INTERSECT_CHECK (RAY, OBJECT, PARMS, HIT_INFO)
*
*   Check ray and object for an intersection.  If so, return TRUE, and save
*   any partial results in HIT_INFO.  These partial results may be used later
*   to get detailed information about the intersection geometry.
}
function type1_sphere_intersect_check ( {check for ray/object intersection}
  in out  ray: type1_ray_t;            {ray descriptor}
  in      object: ray_object_t;        {object to intersect ray with}
  in      parms: type1_object_parms_t; {run-time parameters passed from above}
  out     hit_info: ray_hit_info_t;    {all returned information}
  out     shader: ray_shader_t)        {pointer to shader to resolve color}
  :boolean;                            {TRUE if ray does hit object}
  val_param;

var
  dp: object_data_p_t;                 {pointer to object unique data block}
  close_l: real;                       {ray length to closest point to center}
  close_d2: real;                      {closest ray-center distance squared}
  ofs: real;                           {delta from close point to intersects}
  hit_l: real;                         {ray length to hit point}
  hit_geom_p: object_hit_geom_p_t;     {pointer to hit geometry block}

label
  no_hit, hit;

begin
  dp := object_data_p_t(object.data_p); {make local pointer to object data}
  close_l :=                           {make ray length to closest approach}
    (ray.vect.x * (dp^.center.x - ray.point.x)) +
    (ray.vect.y * (dp^.center.y - ray.point.y)) +
    (ray.vect.z * (dp^.center.z - ray.point.z));
  if (close_l + dp^.radius) < ray.min_dist {whole object before valid interval ?}
    then goto no_hit;
  if (close_l - dp^.radius) > ray.max_dist {whole object after valid interval ?}
    then goto no_hit;
  close_d2 :=                          {make square of center to ray distance}
    sqr(dp^.center.x - (ray.point.x + close_l*ray.vect.x)) +
    sqr(dp^.center.y - (ray.point.y + close_l*ray.vect.y)) +
    sqr(dp^.center.z - (ray.point.z + close_l*ray.vect.z));
  if close_d2 > dp^.radius2            {ray not intersect object at all ?}
    then goto no_hit;
  ofs :=                               {close point to hit points distance}
    sqrt(dp^.radius2 - close_d2);
{
*   Try first intersect point in the ray direction.
}
  hit_l := close_l - ofs;              {ray distance to first intersect point}
  if (hit_l >= ray.min_dist) and       {hit point within valid range ?}
      (hit_l <= ray.max_dist) then begin
    hit_info.enter := true;            {ray is entering object, not leaving}
    goto hit;                          {go process the hit}
    end;
{
*   Try second intersect point in the ray direction.
}
  hit_l := close_l + ofs;              {ray distance to second intersect point}
  if (hit_l >= ray.min_dist) and       {hit point within valid range ?}
      (hit_l <= ray.max_dist) then begin
    hit_info.enter := false;           {ray is leaving object, not entering}
    goto hit;                          {go process the hit}
    end;
{
*   The ray did not hit the object at all.  In this case, we don't have
*   to put anything into HIT_INFO.
}
no_hit:
  type1_sphere_intersect_check := false; {indicate no hit}
  return;
{
*   The ray did hit the object.  We therefore must completely fill in
*   HIT_INFO.  We also therefore must allocate and fill in our own private
*   hit geometry block.  This is allocated from array MEM, defined in the
*   RAY_TYPE1_2.INS.PAS insert file.
}
hit:
  ray.max_dist := hit_l;               {only closer hits are allowed from now on}
  hit_info.object_p := addr(object);   {return handle to object that got hit}
  hit_info.distance := hit_l;          {fill in ray distance to hit point}
  hit_geom_p := univ_ptr(addr(mem[next_mem])); {get pointer to hit geom block}
  next_mem := next_mem + sizeof(hit_geom_p^); {update index to next free mem}
  next_mem := (next_mem + 3) & ~3;     {round up to next multiple of 4 bytes}
  if next_mem > mem_block_size then begin {not enough room for HIT_GEOM block ?}
    writeln ('Insufficient space in array MEM (RAY_TYPE1_2.INS.PAS).');
    sys_bomb;                          {save traceback info and abort}
    end;
  hit_info.shader_parms_p :=           {fill in pointer to hit geometry save area}
    ray_shader_parms_p_t(hit_geom_p);
  hit_geom_p^.base.liparm_p := parms.liparm_p; {from run-time parameters}
  if dp^.visprop_p <> nil              {check where to get visprop pointer from}
    then hit_geom_p^.base.visprop_p := dp^.visprop_p {get it from object data}
    else hit_geom_p^.base.visprop_p := parms.visprop_p; {from run-time parameters}
  hit_geom_p^.ray_point := ray.point;  {save ray origin point in our coor space}
  hit_geom_p^.ray_vect := ray.vect;    {save ray unit vector in our coor space}
  shader := parms.shader;              {shader comes from run time parameters}
  type1_sphere_intersect_check := true; {indicate there was an intersection}
  end;
{
****************************************************************************
*
*   Local subroutine TYPE1_SPHERE_INTERSECT_GEOM (HIT_INFO, FLAGS, GEOM_INFO)
*
*   Return specific geometric information about a ray/object intersection
*   in GEOM_INFO.  In HIT_INFO are any useful results left by the ray/object
*   intersection check calculation.  FLAGS identifies what specific geometric
*   information is being requested.
}
procedure type1_sphere_intersect_geom ( {return detailed geometry of intersection}
  in      hit_info: ray_hit_info_t;    {data saved by INTERSECT_CHECK}
  in      flags: ray_geom_flags_t;     {bit mask list of what is being requested}
  out     geom_info: ray_geom_info_t); {filled in geometric info}
  val_param;

var
  hit_geom_p: object_hit_geom_p_t;     {pointer to hit geometry block}
  data_p: object_data_p_t;             {pointer to object specific data}

begin
  geom_info.flags := [];               {init what info we returned indicator}
  hit_geom_p :=                        {pointer to HIT_GEOM block}
    object_hit_geom_p_t(hit_info.shader_parms_p);
  data_p :=                            {pointer to object-specific data}
    object_data_p_t(hit_info.object_p^.data_p);
  with                                 {define abbreviations}
      hit_geom_p^:hit_geom,            {object specific hit geometry block}
      data_p^:data                     {object specific data block}
      do begin
{
*   Return intersection point coordinates.
}
  if ray_geom_point in flags then begin
    geom_info.flags :=                 {inidicate intersect point returned}
      geom_info.flags + [ray_geom_point];
    geom_info.point.x :=
      hit_geom.ray_point.x + (hit_info.distance * hit_geom.ray_vect.x);
    geom_info.point.y :=
      hit_geom.ray_point.y + (hit_info.distance * hit_geom.ray_vect.y);
    geom_info.point.z :=
      hit_geom.ray_point.z + (hit_info.distance * hit_geom.ray_vect.z);
    end;
{
*   Return unit normal vector of surface at intersection point.
}
  if ray_geom_unorm in flags then begin
    geom_info.flags :=                 {indicate unit normal returned}
      geom_info.flags + [ray_geom_unorm];
    if ray_geom_point in flags         {check for hit point already computed}

      then begin                       {hit point already sitting in GEOM_INFO}
        geom_info.unorm.x :=
          (geom_info.point.x - data.center.x)
          / data.radius;
        geom_info.unorm.y :=
          (geom_info.point.y - data.center.y)
          / data.radius;
        geom_info.unorm.z :=
          (geom_info.point.z - data.center.z)
          / data.radius;
        end                            {done if intersect point pre-computed}

      else begin                       {hit point not already pre-computed}
        geom_info.unorm.x :=
          (hit_geom.ray_point.x + (hit_info.distance * hit_geom.ray_vect.x)
          - data.center.x)
          / data.radius;
        geom_info.unorm.y :=
          (hit_geom.ray_point.y + (hit_info.distance * hit_geom.ray_vect.y)
          - data.center.y)
          / data.radius;
        geom_info.unorm.z :=
          (hit_geom.ray_point.z + (hit_info.distance * hit_geom.ray_vect.z)
          - data.center.z)
          / data.radius;
        end                            {done with hit point not pre-computed}
      ;                                {done with pre-computed descision}
    end;                               {done returning unit surface normal}
    end;                               {done using abbreviations}
  end;
{
***************************************************************************************
*
*   Local subroutine TYPE1_SPHERE_INTERSECT_BOX (BOX, OBJECT, HERE, ENCLOSED)
*
*   Find the intersection status between this object and a paralellpiped.
*   HERE is returned as TRUE if the intersect check routine for this object could
*   ever return TRUE for ray within the box volume.  ENCLOSED is returned as true
*   if the object completely encloses the box.
}
procedure type1_sphere_intersect_box ( {find object/box intersection status}
  in      box: ray_box_t;              {descriptor for a paralellpiped volume}
  in      object: ray_object_t;        {object to intersect with the box}
  out     here: boolean;               {TRUE if ISECT_CHECK could be true in box}
  out     enclosed: boolean);          {TRUE if solid obj, and completely encloses box}
  val_param;

var
  data_p: object_data_p_t;             {pointer to object specific data}
  cv: vect_3d_t;                       {vector from box corner to sphere center}
  i, j, k: sys_int_machine_t;          {loop counters}
  cd: array[1..3] of real;             {dist from each base plane to sphere center}
  v1, v2, v3: vect_3d_t;               {scratch 3D vectors}
  cirv: vect_3d_t;                     {vector from corner to circle center}
  r2: real;                            {circle radius squared}
  pm: real;                            {scale factor to project R2 perp to slice}
  pr2: real;                           {squared circle radius after projection}
  m, m2: real;                         {scratch scale factors}
  dot: real;                           {result of dot product}
  some_in, some_out: boolean;          {box verticies status for in/out sphere}

label
  all_outside, is_here;

begin
  data_p :=                            {pointer to object-specific data}
    object_data_p_t(object.data_p);
  with                                 {define abbreviations}
      data_p^:data                     {object specific data block}
      do begin
{
*   Abbreviations are:
*   DATA  -  Data block for this specific object.
}
  cv.x := data.center.x - box.point.x; {make vector from box corner to sphere center}
  cv.y := data.center.y - box.point.y;
  cv.z := data.center.z - box.point.z;
  for i := 1 to 3 do begin             {once for each set of paralell faces}
    cd[i] :=                           {perp dist from base plane to sphere center}
      cv.x*box.edge[i].unorm.x +
      cv.y*box.edge[i].unorm.y +
      cv.z*box.edge[i].unorm.z;
    if cd[i] < -data.radius            {completely behind this plane ?}
      then goto all_outside;
    if cd[i] > data.radius+box.edge[i].width {completely in front of front face ?}
      then goto all_outside;
    end;                               {back and do next set of paralell faces}
{
*   The sphere definately exists somewhere in each of the three slices of space
*   between the planes of opposite faces.  This means the trivial reject test failed.
*   CD has been set to the perpendicular distance from the base plane (the plane
*   the box corner point is on) of each slice to the sphere center.
*
*   Now check the in/out status of the box corner points, and set the flags SOME_IN
*   and SOME_OUT.  If all the corner points are inside the sphere, then the box
*   is completely enclosed, and we can return immediately.
}
  some_in := false;                    {init the inside/outside flags}
  some_out := false;
  if sqr(cv.x)+sqr(cv.y)+sqr(cv.z) > data.radius2 {check box origin point}
    then some_out := true
    else some_in := true;

  v1.x := box.edge[1].edge.x - cv.x;
  v1.y := box.edge[1].edge.y - cv.y;
  v1.z := box.edge[1].edge.z - cv.z;
  if sqr(v1.x)+sqr(v1.y)+sqr(v1.z) > data.radius2 {check point at V1}
    then some_out := true
    else some_in := true;

  v2.x := box.edge[2].edge.x - cv.x;
  v2.y := box.edge[2].edge.y - cv.y;
  v2.z := box.edge[2].edge.z - cv.z;
  if sqr(v2.x)+sqr(v2.y)+sqr(v2.z) > data.radius2 {check point at V2}
    then some_out := true
    else some_in := true;

  v3.x := box.edge[3].edge.x - cv.x;
  v3.y := box.edge[3].edge.y - cv.y;
  v3.z := box.edge[3].edge.z - cv.z;
  if sqr(v3.x)+sqr(v3.y)+sqr(v3.z) > data.radius2 {check point at V3}
    then some_out := true
    else some_in := true;

  if    sqr(v1.x+box.edge[2].edge.x)   {check point at V1,V2}
      + sqr(v1.y+box.edge[2].edge.y)
      + sqr(v1.z+box.edge[2].edge.z) > data.radius2
    then some_out := true
    else some_in := true;

  if    sqr(v1.x+box.edge[3].edge.x)   {check point at V1,V3}
      + sqr(v1.y+box.edge[3].edge.y)
      + sqr(v1.z+box.edge[3].edge.z) > data.radius2
    then some_out := true
    else some_in := true;

  if    sqr(v2.x+box.edge[3].edge.x)   {check point at V2,V3}
      + sqr(v2.y+box.edge[3].edge.y)
      + sqr(v2.z+box.edge[3].edge.z) > data.radius2
    then some_out := true
    else some_in := true;

  if    sqr(v1.x+box.edge[2].edge.x+box.edge[3].edge.x) {check point at V1,V2,V3}
      + sqr(v1.y+box.edge[2].edge.y+box.edge[3].edge.y)
      + sqr(v1.z+box.edge[2].edge.z+box.edge[3].edge.z) > data.radius2
    then some_out := true
    else some_in := true;

  if not some_out then begin           {box entirely within sphere ?}
    here := false;                     {no surface within box for rays to hit}
    enclosed := true;                  {object completely encloses box}
    return;
    end;
  if some_in then goto is_here;        {surface definately passes thru box ?}
{
*   None of the verticies of the box are inside the sphere.  Now determine whether
*   any part of the sphere is inside any part of the box.  First find a reasonably
*   small sphere that completely encloses the box.  If this extent sphere does not
*   intersect the object sphere, then we can do a trivial reject.  This is a rather
*   common case when the box is much smaller than the sphere.
}
  if                                   {obj sphere and extent sphere not intersect ?}
    (  sqr(cv.x - 0.5*(                {distance between sphere centers squared}
        box.edge[1].edge.x + box.edge[2].edge.x + box.edge[3].edge.x))
      +sqr(cv.y - 0.5*(
        box.edge[1].edge.y + box.edge[2].edge.y + box.edge[3].edge.y))
      +sqr(cv.z - 0.5*(
        box.edge[1].edge.z + box.edge[2].edge.z + box.edge[3].edge.z)) )
    >
    sqr(data.radius                    {sum of the radiuses squared}
      + 0.5*(box.edge[1].width + box.edge[2].width + box.edge[3].width))
    then goto all_outside;             {sphere definately not intersect box}
{
*   All the easy checks failed to determine whether the sphere intersects the box;
*   we must now do a rigorous intersection.  Start by finding a slice that does not
*   contain the sphere center (if there isn't one, then sphere obviously hits the box).
*   Then find the circle of intersection between the sphere and the closest plane
*   for that slice.  A different slice is then found that does not contain the circle
*   center (again, if there isn't one, then the sphere hits the box).  The line
*   segment of intersection between the circle and the nearest slice plane is then
*   checked for interesection with the third remaining slice.  Intersection of the
*   line segement and the third slice is a necessary and sufficient condition for
*   the sphere intersecting the box.
}
  for i := 1 to 3 do begin             {look for slice with sphere center outside}
    if (cd[i] >= 0.0) and (cd[i] <= box.edge[i].width) then next; {inside this slice ?}
    if cd[i] <= 0.0                    {which side of slice is sphere on ?}
      then begin                       {sphere is on the base plane side of this slice}
        cirv.x :=                      {vector from box corner to circle center}
          cv.x - box.edge[i].unorm.x*cd[i];
        cirv.y :=
          cv.y - box.edge[i].unorm.y*cd[i];
        cirv.z :=
          cv.z - box.edge[i].unorm.z*cd[i];
        r2 := data.radius2 - sqr(cd[i]); {squared radius of circle}
        end
      else begin                       {sphere is not on base plane side of this slice}
        m := cd[i] - box.edge[i].width; {distance from front plane to sphere center}
        cirv.x :=                      {vector from box corner to circle center}
          cv.x - box.edge[i].unorm.x*m;
        cirv.y :=
          cv.y - box.edge[i].unorm.y*m;
        cirv.z :=
          cv.z - box.edge[i].unorm.z*m;
        r2 := data.radius2 - sqr(m);   {squared radius of circle}
        end
      ;
{
*   CIRV is the vector from the box corner the center of a circle.  R2 is the radius
*   of the circle squared.  Now intersect this circle with any one of the two remaining
*   slices that also does not contain the circle center.
}
    for j := 1 to 3 do begin           {look for slice not containing circle center}
      if j = i then next;              {don't use slice used to make circle}
      m :=                             {distance from base plane to circle center}
        cirv.x*box.edge[j].unorm.x +
        cirv.y*box.edge[j].unorm.y +
        cirv.z*box.edge[j].unorm.z;
      if (m >= 0.0) and (m <= box.edge[j].width) {circle center in this slice ?}
        then next;                     {punt slice containing circle center}
      dot :=                           {dot product of two plane unit vectors}
        box.edge[i].unorm.x*box.edge[j].unorm.x +
        box.edge[i].unorm.y*box.edge[j].unorm.y +
        box.edge[i].unorm.z*box.edge[j].unorm.z;
      pm := 1.0 - sqr(dot);            {factor for projecting R2 perp to slice}
      pr2 := r2*pm;                    {make R2 projected perpendicular to this slice}
      if pr2 < 1.0E-16 then goto all_outside; {circle too small or paralell to slice ?}
      if m > 0.0 then begin            {circle is not on base plane side of slice ?}
        m := m - box.edge[j].width;    {make distance from front plane to cir center}
        end;
      if pr2 < sqr(m) then goto all_outside; {circle not hit this slice ?}
      m2 := dot/pm;                    {factor for making V1}
      v1.x := cirv.x - m*(             {vector from corner to line segment center}
        box.edge[j].unorm.x + m2*(dot*box.edge[j].unorm.x - box.edge[i].unorm.x));
      v1.y := cirv.y - m*(
        box.edge[j].unorm.y + m2*(dot*box.edge[j].unorm.y - box.edge[i].unorm.y));
      v1.z := cirv.z - m*(
        box.edge[j].unorm.z + m2*(dot*box.edge[j].unorm.z - box.edge[i].unorm.z));
      r2 := r2 - sqr(m*pm);            {half length of line segment squared}
      m2 := 1.0/sqrt(pm);              {factor for unitizing V2}
      v2.x := m2*(                     {make unit vector along line segment}
        box.edge[j].unorm.y*box.edge[i].unorm.z -
        box.edge[j].unorm.z*box.edge[i].unorm.y);
      v2.y := m2*(
        box.edge[j].unorm.z*box.edge[i].unorm.x -
        box.edge[j].unorm.x*box.edge[i].unorm.z);
      v2.z := m2*(
        box.edge[j].unorm.x*box.edge[i].unorm.y -
        box.edge[j].unorm.y*box.edge[i].unorm.x);
{
*   V1 is the vector from the box corner to the center of the line segment.  V2 is
*   a unit vector along the line segment.  R2 is half the length of the line segment
*   squared.  Now see if any part of this line segment intersects the remaining slice.
*   if so, the sphere intersects the box, and otherwise it doesn't.
}
      k := 6-i-j;                      {make number of remaining slice}
      m :=                             {distance from base plane to center of segment}
        v1.x*box.edge[k].unorm.x +
        v1.y*box.edge[k].unorm.y +
        v1.z*box.edge[k].unorm.z;
      if (m >= 0.0) and (m <= box.edge[k].width) {segment center point in slice ?}
        then goto is_here;             {at least one point definately in all slices}
      if m > 0.0                       {segment not on base plane side of slice ?}
        then m := m - box.edge[k].width; {make segment center distance to slice plane}
      if sqr(m) <=                     {segment hits the plane ?}
        r2*sqr(                        {perp projected segment radius squared}
          v2.x*box.edge[k].unorm.x +
          v2.y*box.edge[k].unorm.y +
          v2.z*box.edge[k].unorm.z)
        then goto is_here              {sphere definately hits box}
        else goto all_outside;         {sphere definately not hits box}
      end;                             {keep looking for slice without circle center}
    goto is_here;                      {circle center is in both remaining slices}
    end;                               {keep looking for slice without sphere center}
{
*   Jump to here if some part of the sphere's surface passes thru the box.  The
*   case of the box completely inside the sphere must already have been eliminated.
}
is_here:                               {jump here on sphere suface is in box}
  here := true;                        {rays can hit sphere inside the box volume}
  enclosed := false;                   {box is not wholly enclosed by sphere}
  return;
{
*   Jump to here on discovering that the box and sphere are completely disjoint.
}
all_outside:
  here := false;                       {no rays will hit sphere in box volume}
  enclosed := false;                   {box is not inside sphere}
  end;                                 {done using abbreviations}
  end;                                 {end of subroutine}
{
****************************************************************************
*
*   Local subroutine TYPE1_SPHERE_ADD_CHILD (OBJECT, PARMS)
}
procedure type1_sphere_add_child (     {Add child to aggregate object (illegal)}
  in      object: ray_object_t;        {the object to add}
  in      parms: univ integer32);      {unused optional parameters}
  val_param;

begin
  writeln ('TYPE1_SPHERE is not an aggregate object and does not support ADD_CHILD.');
  sys_bomb;
  end;
