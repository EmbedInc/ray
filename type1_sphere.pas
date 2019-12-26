{   All the routines to implement a dumb sphere object.
}
module type1_sphere;
define type1_sphere_class_make;
%include 'ray_type1_2.ins.pas';

type
  sphere_data_p_t = ^sphere_data_t;
  sphere_data_t = record               {data record pointed to by object block}
    visprop_p: type1_visprop_p_t;      {pointer to visual properties block}
    center: vect_3d_fp1_t;             {XYZ coordinates of center point}
    radius: single;                    {sphere radius}
    radius2: single;                   {sphere radius squared}
    end;

  sphere_hit_geom_p_t = ^sphere_hit_geom_t;
  sphere_hit_geom_t = record           {saved data from intersection check}
    base: type1_hit_geom_t;            {mandatory part of hit geometry save area}
    ray_point: vect_3d_t;              {ray origin point in object's space}
    ray_vect: vect_3d_t;               {ray unit vector in object's space}
    end;
{
********************************************************************************
*
*   Local subroutine TYPE1_SPHERE_CREATE (OBJECT, CREA, STAT)
*
*   Fill in the new object in OBJECT.  CREA is the user data parameters for this
*   object.  STAT is the standard system error return code.
}
procedure type1_sphere_create (        {create new primitive with custom data}
  in out  object: ray_object_t;        {object to be filled in}
  in      gcrea_p: univ_ptr;           {data for creating this object instance}
  out     stat: sys_err_t);            {completion status code}
  val_param;

var
  crea_p: type1_sphere_crea_data_p_t;  {pointer to creation data in our format}
  data_p: sphere_data_p_t;             {pointer to internal object data}

begin
  sys_error_none (stat);               {init to no error}
  crea_p := gcrea_p;                   {get pointer to create data in our format}

  util_mem_grab (                      {allocate data block for new object}
    sizeof(data_p^), ray_mem_p^, false, data_p);
  object.data_p := data_p;             {set pointer to object data block}

  data_p^.visprop_p := crea_p^.visprop_p; {copy pointer to visual properties block}
  data_p^.center.x := crea_p^.center.x; {copy coordinate of sphere center}
  data_p^.center.y := crea_p^.center.y;
  data_p^.center.z := crea_p^.center.z;
  data_p^.radius := crea_p^.radius;    {copy sphere radius}
  data_p^.radius2 := sqr(crea_p^.radius); {save radius squared for speedup}
  end;
{
********************************************************************************
*
*   Local function TYPE1_SPHERE_INTERSECT_CHECK (
*     GRAY, OBJECT, GPARMS, HIT_INFO, SHADER)
*
*   Check ray and object for an intersection.  If so, return TRUE, and save any
*   partial results in HIT_INFO.  These partial results may be used later to get
*   detailed information about the intersection geometry.
}
function type1_sphere_intersect_check ( {check for ray/object intersection}
  in out  gray: univ ray_base_t;       {input ray descriptor}
  in var  object: ray_object_t;        {input object to intersect ray with}
  in      gparms_p: univ_ptr;          {pointer to run time TYPEn-specific params}
  out     hit_info: ray_hit_info_t;    {handle to routines and data to get hit color}
  out     shader: ray_shader_t)        {pointer to shader to resolve color here}
  :boolean;                            {TRUE if ray does hit object}
  val_param;

var
  ray_p: type1_ray_p_t;                {pointer to ray in our format}
  parms_p: type1_object_parms_p_t;     {pointer to runtime parameters in our format}
  dp: sphere_data_p_t;                 {pointer to object unique data block}
  close_l: real;                       {ray length to closest point to center}
  close_d2: real;                      {closest ray-center distance squared}
  ofs: real;                           {delta from close point to intersects}
  hit_l: real;                         {ray length to hit point}
  hit_geom_p: sphere_hit_geom_p_t;     {pointer to hit geometry block}

label
  no_hit, hit;

begin
  ray_p := univ_ptr(addr(gray));       {make pointer to ray, our format}
  dp := object.data_p;                 {make local pointer to object data, our format}
  parms_p := gparms_p;                 {make pointer to runtime parameters, our format}

  close_l :=                           {make ray length to closest approach}
    (ray_p^.vect.x * (dp^.center.x - ray_p^.point.x)) +
    (ray_p^.vect.y * (dp^.center.y - ray_p^.point.y)) +
    (ray_p^.vect.z * (dp^.center.z - ray_p^.point.z));
  if (close_l + dp^.radius) < ray_p^.min_dist {whole object before valid interval ?}
    then goto no_hit;
  if (close_l - dp^.radius) > ray_p^.max_dist {whole object after valid interval ?}
    then goto no_hit;
  close_d2 :=                          {make square of center to ray distance}
    sqr(dp^.center.x - (ray_p^.point.x + close_l*ray_p^.vect.x)) +
    sqr(dp^.center.y - (ray_p^.point.y + close_l*ray_p^.vect.y)) +
    sqr(dp^.center.z - (ray_p^.point.z + close_l*ray_p^.vect.z));
  if close_d2 > dp^.radius2            {ray not intersect object at all ?}
    then goto no_hit;
  ofs :=                               {close point to hit points distance}
    sqrt(dp^.radius2 - close_d2);
{
*   Try first intersect point in the ray direction.
}
  hit_l := close_l - ofs;              {ray distance to first intersect point}
  if (hit_l >= ray_p^.min_dist) and    {hit point within valid range ?}
      (hit_l <= ray_p^.max_dist) then begin
    hit_info.enter := true;            {ray is entering object, not leaving}
    goto hit;                          {go process the hit}
    end;
{
*   Try second intersect point in the ray direction.
}
  hit_l := close_l + ofs;              {ray distance to second intersect point}
  if (hit_l >= ray_p^.min_dist) and    {hit point within valid range ?}
      (hit_l <= ray_p^.max_dist) then begin
    hit_info.enter := false;           {ray is leaving object, not entering}
    goto hit;                          {go process the hit}
    end;
{
*   The ray did not hit the object at all.  In this case, we don't have to put
*   anything into HIT_INFO.
}
no_hit:
  type1_sphere_intersect_check := false; {indicate no hit}
  return;
{
*   The ray did hit the object.  We therefore must completely fill in HIT_INFO.
*   We also therefore must allocate and fill in our own private hit geometry
*   block.  This is allocated from array MEM, defined in the RAY_TYPE1_2.INS.PAS
*   include file.
}
hit:
  ray_p^.max_dist := hit_l;            {only closer hits are allowed from now on}
  hit_info.object_p := addr(object);   {return handle to object that got hit}
  hit_info.distance := hit_l;          {fill in ray distance to hit point}
  hit_geom_p := univ_ptr(addr(mem[next_mem])); {get pointer to hit geom block}
  next_mem := next_mem + sizeof(hit_geom_p^); {update index to next free mem}
  next_mem := (next_mem + 3) & ~3;     {round up to next multiple of 4 bytes}
  if next_mem > mem_block_size then begin {not enough room for HIT_GEOM block ?}
    writeln ('Insufficient space in array MEM (RAY_TYPE1_2.INS.PAS).');
    sys_bomb;                          {save traceback info and abort}
    end;
  hit_info.shader_parms_p := hit_geom_p; {fill in pointer to hit geometry save area}
  hit_geom_p^.base.liparm_p := parms_p^.liparm_p; {from run-time parameters}
  if dp^.visprop_p <> nil              {check where to get visprop pointer from}
    then hit_geom_p^.base.visprop_p := dp^.visprop_p {get it from object data}
    else hit_geom_p^.base.visprop_p := parms_p^.visprop_p; {from run-time parameters}
  hit_geom_p^.ray_point := ray_p^.point; {save ray origin point in our coor space}
  hit_geom_p^.ray_vect := ray_p^.vect; {save ray unit vector in our coor space}
  shader := parms_p^.shader;           {shader comes from run time parameters}
  type1_sphere_intersect_check := true; {indicate there was an intersection}
  end;
{
********************************************************************************
*
*   Local subroutine TYPE1_SPHERE_INTERSECT_GEOM (HIT_INFO, FLAGS, GEOM_INFO)
*
*   Return specific geometric information about a ray/object intersection in
*   GEOM_INFO.  In HIT_INFO are any useful results left by the ray/object
*   intersection check calculation.  FLAGS identifies what specific geometric
*   information is being requested.
}
procedure type1_sphere_intersect_geom ( {return detailed geometry of intersection}
  in      hit_info: ray_hit_info_t;    {data saved by INTERSECT_CHECK}
  in      flags: ray_geom_flags_t;     {bit mask list of what is being requested}
  out     geom_info: ray_geom_info_t); {filled in geometric info}
  val_param;

var
  hit_geom_p: sphere_hit_geom_p_t;     {pointer to hit geometry block}
  data_p: sphere_data_p_t;             {pointer to object specific data}

begin
  geom_info.flags := [];               {init what info we returned indicator}
  hit_geom_p :=                        {pointer to HIT_GEOM block}
    sphere_hit_geom_p_t(hit_info.shader_parms_p);
  data_p :=                            {pointer to object-specific data}
    sphere_data_p_t(hit_info.object_p^.data_p);
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
********************************************************************************
*
*   Local subroutine TYPE1_SPHERE_INTERSECT_BOX (BOX, OBJECT, HERE, ENCLOSED)
*
*   Find the intersection status between this object and a paralellpiped.  HERE
*   is returned as TRUE if the intersect check routine for this object could
*   ever return TRUE for ray within the box volume.  ENCLOSED is returned as
*   true if the object completely encloses the box.
}
procedure type1_sphere_intersect_box ( {find object/box intersection status}
  in      box: ray_box_t;              {descriptor for a paralellpiped volume}
  in      object: ray_object_t;        {object to intersect with the box}
  out     here: boolean;               {TRUE if ISECT_CHECK could be true in box}
  out     enclosed: boolean);          {TRUE if solid obj, and completely encloses box}
  val_param;

var
  data_p: sphere_data_p_t;             {pointer to object specific data}
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
    sphere_data_p_t(object.data_p);
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
*   between the planes of opposite faces.  This means the trivial reject test
*   failed.  CD has been set to the perpendicular distance from the base plane
*   (the plane the box corner point is on) of each slice to the sphere center.
*
*   Now check the in/out status of the box corner points, and set the flags
*   SOME_IN and SOME_OUT.  If all the corner points are inside the sphere, then
*   the box is completely enclosed, and we can return immediately.
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
*   None of the verticies of the box are inside the sphere.  Now determine
*   whether any part of the sphere is inside any part of the box.  First find a
*   reasonably small sphere that completely encloses the box.  If this extent
*   sphere does not intersect the object sphere, then we can do a trivial
*   reject.  This is a rather common case when the box is much smaller than the
*   sphere.
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
*   All the easy checks failed to determine whether the sphere intersects the
*   box.  We must now do a rigorous intersection.  Start by finding a slice that
*   does not contain the sphere center (if there isn't one, then sphere
*   obviously hits the box).  Then find the circle of intersection between the
*   sphere and the closest plane for that slice.  A different slice is then
*   found that does not contain the circle center (again, if there isn't one,
*   then the sphere hits the box).  The line segment of intersection between the
*   circle and the nearest slice plane is then checked for interesection with
*   the third remaining slice.  Intersection of the line segement and the third
*   slice is a necessary and sufficient condition for the sphere intersecting
*   the box.
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
*   CIRV is the vector from the box corner the center of a circle.  R2 is the
*   radius of the circle squared.  Now intersect this circle with any one of the
*   two remaining slices that also does not contain the circle center.
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
*   V1 is the vector from the box corner to the center of the line segment.  V2
*   is a unit vector along the line segment.  R2 is half the length of the line
*   segment squared.  Now see if any part of this line segment intersects the
*   remaining slice.  If so, the sphere intersects the box, and otherwise it
*   doesn't.
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
*   case of the box completely inside the sphere must already have been
*   eliminated.
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
********************************************************************************
*
*   Subroutine TYPE1_SPHERE_CLASS_MAKE (CLASS)
*
*   Fill in the routines block for this class of objects.
}
procedure type1_sphere_class_make (    {fill in object class descriptor}
  out     class: ray_object_class_t);  {block to fill in}
  val_param;

begin
  class.create := addr(type1_sphere_create);
  class.intersect_check := addr(type1_sphere_intersect_check);
  class.hit_geom := addr(type1_sphere_intersect_geom);
  class.intersect_box := addr(type1_sphere_intersect_box);
  class.add_child := nil;
  end;
