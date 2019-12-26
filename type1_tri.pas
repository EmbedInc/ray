{   Ray tracer type 1 triangle object.
}
module type1_tri;
define type1_tri_routines_make;
%include 'ray_type1_2.ins.pas';

type
{
*   TRI_DATA_T is the minimum mandatory private data stored for each triangle.
*   This data structure is implicitly extended to include optional per-vertex
*   data.  Which data is present is indicated by the FLAGS field.  When present,
*   this data structure is extended to include VERT_OPT_RGB_T and
*   VERT_OPT_SHNORM_T, at the end, in that order.
}
  tri_data_p_t = ^tri_data_t;
  tri_data_t = record                  {mandatory fields of data for this object}
    flags: type1_tri_flags_t;          {indicates what optional vertex data exists
                                        this field MUST BE FIRST, since it is the
                                        only field if triangle is too small to see}
    visprop_p: type1_visprop_p_t;      {pointer to visual properties block}
    p1: vect_3d_fp1_t;                 {3D world space coor of one triangle vertex}
    norm: vect_3d_fp1_t;               {unit normal vector to triangle plane}
    xbx, xby: single;                  {object to canonical triangle space xform}
    ybx, yby: single;
    zbx, zby: single;
    end;

  rgba_t = record                      {RGB and/or alpha stored per vertex}
    red: int8u_t;
    grn: int8u_t;
    blu: int8u_t;
    alpha: int8u_t;
    end;

  vert_opt_rgb_p_t = ^vert_opt_rgb_t;
  vert_opt_rgb_t = record              {optional RGB diffuse data per vertex}
    v1, v2, v3: rgba_t;
    end;

  vert_opt_shnorm_p_t = ^vert_opt_shnorm_t;
  vert_opt_shnorm_t = record           {optional shading normal vector per vertex}
    v1, v2, v3: vect_3d_t;
    end;
{
*   Hit information returned when a ray is found to intersect a triangle.  Other
*   than the BASE field, this data is private to routines in this module.
}
  tri_hit_geom_p_t = ^tri_hit_geom_t;
  tri_hit_geom_t = record              {saved data from intersection check}
    base: type1_hit_geom_t;            {mandatory part of hit geometry save area}
    data_p: tri_data_p_t;              {pointer to object's private data block}
    hpnt: vect_3d_t;                   {hit point in object's space}
    hitx, hity: real;                  {hit point in triangle canonical space}
    flip_factor: real;                 {-1 or 1 to flip normal vectors to front}
    end;
{
********************************************************************************
*
*   Local subroutine TYPE1_TRI_CREATE (OBJECT, CREA, STAT)
*
*   Create a new instance of the TRI (triangle) object.
}
procedure type1_tri_create (           {create new primitive with custom data}
  in out  object: ray_object_t;        {object instance to be filled in}
  in      gcrea_p: univ_ptr;           {data for creating this object instance}
  out     stat: sys_err_t);            {completion status code}
  val_param;

var
  crea_p: type1_tri_crea_data_p_t;     {pointer to creation data, our format}
  size: sys_int_adr_t;                 {amount of mem needed for private data block}
  data_p: tri_data_p_t;                {pointer to mandatory part of private data}
  rgb_p: vert_opt_rgb_p_t;             {pointer to current vert RGB data}
  shnorm_p: vert_opt_shnorm_p_t;       {pointer to current vert shading normal data}
  opt_next: sys_int_adr_t;             {address of next optional data in vertex 1}
  fwd: vect_mat3x3_t;                  {canonical to obj transformation matrix}
  bak: vect_mat3x3_t;                  {obj to canonical transformation matrix}
  flags: type1_tri_flags_t;            {local copy of user's flags}
  bak_exists: boolean;                 {unused subroutine return flag}
  m: real;                             {scratch scale factor}

begin
  sys_error_none (stat);               {init to no error}
  crea_p := gcrea_p;                   {make pointer to creation data, our format}
{
*   To find the transform from the object's space to the canonical space, we
*   will construct the canonical to object transform and then take its inverse.
*
*   In the canonical space, the triangle exists from 0,0 to 1,0 to 0,1.  Since
*   this is a 2D space, the Z vector is assumed to be unit size and perpendicular
*   to the triangle plane.
*
*   Make the canonical to object transformation matrix.
}
  fwd.xb.x := crea_p^.p2.x - crea_p^.p1.x;
  fwd.xb.y := crea_p^.p2.y - crea_p^.p1.y;
  fwd.xb.z := crea_p^.p2.z - crea_p^.p1.z;

  fwd.yb.x := crea_p^.p3.x - crea_p^.p1.x;
  fwd.yb.y := crea_p^.p3.y - crea_p^.p1.y;
  fwd.yb.z := crea_p^.p3.z - crea_p^.p1.z;

  fwd.zb.x := fwd.xb.y*fwd.yb.z - fwd.xb.z*fwd.yb.y; {cross of X and Y vectors}
  fwd.zb.y := fwd.xb.z*fwd.yb.x - fwd.xb.x*fwd.yb.z;
  fwd.zb.z := fwd.xb.x*fwd.yb.y - fwd.xb.y*fwd.yb.x;
  m := sqrt(sqr(fwd.zb.x) + sqr(fwd.zb.y) + sqr(fwd.zb.z));
  if m < 1.0E-29                       {check triangle size}
    then begin                         {triangle is too small to see}
      object.data_p := nil;            {indicate this triangle will be ignored}
      return;
      end
    else begin                         {triangle is big enough to see}
      m := 1.0 / m;                    {make scale factor for unitizing vector}
      end
    ;
  fwd.zb.x := fwd.zb.x * m;            {unitize geometric normal vector}
  fwd.zb.y := fwd.zb.y * m;
  fwd.zb.z := fwd.zb.z * m;
{
*   This triangle is definately big enough to see.  Allocate our private data block.
}
  flags := crea_p^.flags;              {make local copy of user's flags}
  if                                   {we will need to store RGBA ?}
      (type1_tri_flag_rgb_k in flags) or
      (type1_tri_flag_alpha_k in flags)
      then begin
    flags := flags + [type1_tri_flag_rgba_k];
    end;

  size := sizeof(data_p^);             {init to size of required fields}
  if type1_tri_flag_rgba_k in flags then begin {storing RGB per vertex ?}
    size := size + sizeof(rgb_p^);     {add size needed for RGB per vertex}
    end;
  if type1_tri_flag_shnorm_k in flags then begin {storing normal per vertex ?}
    size := size + sizeof(shnorm_p^);  {add size needed for normal per vertex}
    end;
  util_mem_grab (                      {allocate private memory for this triangle}
    size,                              {amount of memory needed}
    ray_mem_p^,                        {parent memory context}
    false,                             {won't need to individually deallocate this}
    data_p);                           {returned point to new memory}
  opt_next := sys_int_adr_t(data_p) + sizeof(data_p^); {optional data starts here}
  object.data_p := data_p;             {set pointer to object data block}
{
*   The private memory block for this object has been allocated.  DATA_P points
*   to the mandatory fields.  OPT_START is the address where optional data will
*   start, if any.
*
*   The canonical to object space transform is in FWD.  Use its inverse to
*   stuff the gemometric information in the mandatory fields of the private
*   data block.
}
  vect_3x3_invert (fwd, m, bak_exists, bak); {make obj to canonical xform in BAK}
  data_p^.p1.x := crea_p^.p1.x;        {origin in canonical triangle space}
  data_p^.p1.y := crea_p^.p1.y;
  data_p^.p1.z := crea_p^.p1.z;
  data_p^.norm.x := fwd.zb.x;          {object space unit normal vector}
  data_p^.norm.y := fwd.zb.y;
  data_p^.norm.z := fwd.zb.z;
  data_p^.xbx := bak.xb.x;             {save the 2D part of obj to canonical xform}
  data_p^.xby := bak.xb.y;
  data_p^.ybx := bak.yb.x;
  data_p^.yby := bak.yb.y;
  data_p^.zbx := bak.zb.x;
  data_p^.zby := bak.zb.y;

  data_p^.visprop_p := crea_p^.visprop_p; {copy visprop block pointer from user data}
  data_p^.flags := flags;              {save optional data flags in tri data block}
{
*   All the mandatory fields of our private data for this triangle have been
*   filled in.  Now fill in the optional per-vertex crea_p^.
}
  if type1_tri_flag_rgba_k in flags then begin {RGB and/or A per vertex ?}
    rgb_p := univ_ptr(opt_next);       {get start of RGB data}
    opt_next := opt_next + sizeof(rgb_p^); {update where next optional data goes}

    if type1_tri_flag_rgb_k in flags then begin {RGB present per vertex ?}
      rgb_p^.v1.red := trunc(256.0 * min(0.999, max(0.0, crea_p^.v1.red)));
      rgb_p^.v1.grn := trunc(256.0 * min(0.999, max(0.0, crea_p^.v1.grn)));
      rgb_p^.v1.blu := trunc(256.0 * min(0.999, max(0.0, crea_p^.v1.blu)));

      rgb_p^.v2.red := trunc(256.0 * min(0.999, max(0.0, crea_p^.v2.red)));
      rgb_p^.v2.grn := trunc(256.0 * min(0.999, max(0.0, crea_p^.v2.grn)));
      rgb_p^.v2.blu := trunc(256.0 * min(0.999, max(0.0, crea_p^.v2.blu)));

      rgb_p^.v3.red := trunc(256.0 * min(0.999, max(0.0, crea_p^.v3.red)));
      rgb_p^.v3.grn := trunc(256.0 * min(0.999, max(0.0, crea_p^.v3.grn)));
      rgb_p^.v3.blu := trunc(256.0 * min(0.999, max(0.0, crea_p^.v3.blu)));
      end;

    if type1_tri_flag_alpha_k in flags then begin {ALPHA present per vertex ?}
      rgb_p^.v1.alpha := trunc(256.0 * min(0.999, max(0.0, crea_p^.v1.alpha)));
      rgb_p^.v2.alpha := trunc(256.0 * min(0.999, max(0.0, crea_p^.v2.alpha)));
      rgb_p^.v3.alpha := trunc(256.0 * min(0.999, max(0.0, crea_p^.v3.alpha)));
      end;
    end;                               {done handling optional RGBA data}

  if type1_tri_flag_shnorm_k in flags then begin {storing shading normals ?}
    shnorm_p := univ_ptr(opt_next);    {get pointer to start of shading normals}
(*
    opt_next := opt_next + sizeof(shnorm_p^); {update where next optional data goes}
*)
    shnorm_p^.v1 := crea_p^.v1.shnorm;
    shnorm_p^.v2 := crea_p^.v2.shnorm;
    shnorm_p^.v3 := crea_p^.v3.shnorm;
    end;
  end;
{
********************************************************************************
*
*   Local function TYPE1_TRI_INTERSECT_CHECK (
*     GRAY, OBJECT, GPARMS, HIT_INFO, SHADER)
*
*   Check ray and object for an intersection.  If so, return TRUE, and save
*   any partial results in HIT_INFO.  These partial results may be used later
*   to get detailed information about the intersection geometry.
}
function type1_tri_intersect_check (   {check for ray/object intersection}
  in out  gray: univ ray_base_t;       {input ray descriptor}
  in var  object: ray_object_t;        {input object to intersect ray with}
  in      gparms_p: univ_ptr;          {pointer to run time TYPEn-specific params}
  out     hit_info: ray_hit_info_t;    {handle to routines and data to get hit color}
  out     shader: ray_shader_t)        {pointer to shader to resolve color here}
  :boolean;                            {TRUE if ray does hit object}
  val_param;

var
  ray_p: type1_ray_p_t;                {pointer to ray, our format}
  parms_p: type1_object_parms_p_t;     {pointer to runtime parameters, our format}
  dp: tri_data_p_t;                    {pointer to object's private data block}
  rndot: real;                         {dot product of ray and tri normal vectors}
  dis: real;                           {ray distance to triangle plane}
  hpnt: vect_3d_t;                     {hit point in object space}
  hrel: vect_3d_t;                     {hit point relative to vertex 1}
  hx, hy: real;                        {hit point in canonical 2D triangle space}
  hit_geom_p: tri_hit_geom_p_t;        {pointer to our private hit info}

label
  no_hit;

begin
  ray_p := univ_ptr(addr(gray));       {make pointer to ray info, our format}
  parms_p := gparms_p;                 {make poitner to runtime parameters, our format}

  stats_tri.isect_ray := stats_tri.isect_ray + 1; {one more ray intersect check}
  dp := object.data_p;                 {make local pointer to private data}

  rndot :=                             {dot product of ray and tri normal vectors}
    (ray_p^.vect.x * dp^.norm.x) +
    (ray_p^.vect.y * dp^.norm.y) +
    (ray_p^.vect.z * dp^.norm.z);
  if abs(rndot) < 1.0E-20 then goto no_hit; {triangle plane too paralell to ray ?}

  dis := (                             {ray distance to triangle plane}
    ((dp^.p1.x - ray_p^.point.x) * dp^.norm.x) +
    ((dp^.p1.y - ray_p^.point.y) * dp^.norm.y) +
    ((dp^.p1.z - ray_p^.point.z) * dp^.norm.z)) / rndot;

  if dis <= ray_p^.min_dist then goto no_hit; {intersection is too close ?}
  if dis >= ray_p^.max_dist then goto no_hit; {intersection is too far away ?}

  hpnt.x := ray_p^.point.x + (ray_p^.vect.x * dis); {object space plane intersection point}
  hpnt.y := ray_p^.point.y + (ray_p^.vect.y * dis);
  hpnt.z := ray_p^.point.z + (ray_p^.vect.z * dis);

  hrel.x := hpnt.x - dp^.p1.x;         {hit point relative to vertex 1}
  hrel.y := hpnt.y - dp^.p1.y;
  hrel.z := hpnt.z - dp^.p1.z;

  hx :=                                {transform hit point to canonical tri space}
    (hrel.x * dp^.xbx) + (hrel.y * dp^.ybx) + (hrel.z * dp^.zbx);
  if hx < 0.0 then goto no_hit;        {hit point outside triangle ?}
  hy :=
    (hrel.x * dp^.xby) + (hrel.y * dp^.yby) + (hrel.z * dp^.zby);
  if hy < 0.0 then goto no_hit;
  if (hx + hy) > 1.0 then goto no_hit;
{
*   The ray DOES hit the triangle.  HPNT is the object space hit point.  HX,HY
*   is the canonical triangle space hit point.
}
  hit_geom_p := univ_ptr(addr(mem[next_mem])); {get pointer to hit geom block}
  next_mem := ((sizeof(tri_hit_geom_t)+3) & 16#0FFFFFFFC)
    + next_mem;                        {allocate 4 byte chunks for HIT_GEOM block}
  if next_mem > mem_block_size then begin {not enough room for HIT_GEOM block ?}
    sys_message_bomb ('ray_type1', 'mem_overflow', nil, 0);
    end;

  hit_info.object_p := addr(object);   {return handle to object that got hit}
  hit_info.distance := dis;            {ray distance to hit point}
  hit_info.shader_parms_p := hit_geom_p; {fill in pointer to hit geometry save area}
  if dp^.visprop_p <> nil              {check where to get visprop pointer from}
    then hit_geom_p^.base.visprop_p := dp^.visprop_p {get it from object data}
    else hit_geom_p^.base.visprop_p := parms_p^.visprop_p; {from run-time parameters}

  if rndot > 0.0                       {which side of the triangle did we hit ?}
    then begin                         {ray hit back side of triangle}
      hit_info.enter := false;         {indicate we are leaving an object}
      hit_geom_p^.flip_factor := -1.0; {normal vector will need to be flipped}
      if hit_geom_p^.base.visprop_p^.back_p <> nil then begin {back props exist ?}
        hit_geom_p^.base.visprop_p :=  {switch to properties for back side}
          hit_geom_p^.base.visprop_p^.back_p;
        end;
      end
    else begin                         {ray hit front side of triangle}
      hit_info.enter := true;          {indicate we are entering an object}
      hit_geom_p^.flip_factor := 1.0;  {normal vectors already pointing right dir}
      end
    ;

  hit_geom_p^.base.liparm_p := parms_p^.liparm_p; {save pointer to light sources block}
  hit_geom_p^.data_p := dp;            {save pointer to object's private data}
  hit_geom_p^.hpnt := hpnt;            {save hit point in object coor space}
  hit_geom_p^.hitx := hx;              {save hit point in 2D canonical tri space}
  hit_geom_p^.hity := hy;

  shader := parms_p^.shader;           {default to from run time parameters}
  type1_tri_intersect_check := true;   {indicate there was an intersection}
  stats_tri.hit_ray := stats_tri.hit_ray + 1; {one more ray/tri intersect found}
  return;

no_hit:                                {jump here if ray doesn't intersect triangle}
  type1_tri_intersect_check := false;
  end;
{
********************************************************************************
*
*   Local subroutine TYPE1_TRI_INTERSECT_GEOM (HIT_INFO, FLAGS, GEOM_INFO)
*
*   Return specific geometric information about a ray/object intersection
*   in GEOM_INFO.  In HIT_INFO are any useful results left by the ray/object
*   intersection check calculation.  FLAGS identifies what specific geometric
*   information is being requested.
}
procedure type1_tri_intersect_geom (   {return detailed geometry of intersection}
  in      hit_info: ray_hit_info_t;    {data saved by INTERSECT_CHECK}
  in      flags: ray_geom_flags_t;     {bit mask list of what is being requested}
  out     geom_info: ray_geom_info_t); {filled in geometric info}
  val_param;

var
  hit_geom_p: tri_hit_geom_p_t;        {pointer to hit geometry block}
  opt_next: sys_int_adr_t;             {address of next optional per-vertex data}
  rgb_p: vert_opt_rgb_p_t;             {pointer to current vert RGB data}
  shn_p: vert_opt_shnorm_p_t;          {pointers to user-supplied shading normals}
  vp_p: type1_visprop_p_t;             {pointer to our customized visprop block}
  sz: sys_int_adr_t;                   {amount of memory needed}
  m1: real;                            {mult factor for vertex 1 in linear combo}
  m: real;                             {scale factor for unitizing vector}

begin
  hit_geom_p :=                        {pointer to HIT_GEOM block}
    tri_hit_geom_p_t(hit_info.shader_parms_p);
  with
      hit_geom_p^: geom,               {GEOM is hit geometry block}
      hit_geom_p^.data_p^: data        {DATA is object private data block}
      do begin
  geom_info.flags := [];               {init to nothing returned}
  m1 := 1.0 - geom.hitx - geom.hity;   {weighting factor for vertex 1}
  opt_next :=                          {address of optional vertex data, if any}
    sys_int_adr_t(addr(data)) + sizeof(data);
{
*   Make a temporary copy of the current VISPROP block, and set it to the values
*   at the hit point if this triangle has any customized data per vertex.
}
  if type1_tri_flag_rgba_k in data.flags then begin {data customized per vertex ?}
    vp_p :=                            {allocate space for temporary visprop block}
      type1_visprop_p_t(addr(mem[next_mem]));
    sz := (sizeof(vp_p^) + 3) & (~3);  {round to nice multiple}
    next_mem := next_mem + sz;         {update index to next available memory}
    if next_mem > max_mem_index then begin
      sys_message_bomb ('ray_type1', 'mem_overflow', nil, 0);
      end;
    vp_p^ := geom.base.visprop_p^;     {make copy of visprop block}
    geom.base.visprop_p := vp_p;       {indicate to use temp visprop block}
    rgb_p := univ_ptr(opt_next);       {get pointer to RGB optional data}
    opt_next := opt_next + sizeof(rgb_p^); {next optional data starts after RGB}
    if type1_tri_flag_rgb_k in data.flags then begin {RGB data exists per vertex ?}
      vp_p^.diff_red :=                {interpolate and stuff RED}
        (m1 * rgb_p^.v1.red / 255.0) +
        (geom.hitx * rgb_p^.v2.red / 255.0) +
        (geom.hity * rgb_p^.v3.red / 255.0);
      vp_p^.diff_grn :=                {interpolate and stuff GREEN}
        (m1 * rgb_p^.v1.grn / 255.0) +
        (geom.hitx * rgb_p^.v2.grn / 255.0) +
        (geom.hity * rgb_p^.v3.grn / 255.0);
      vp_p^.diff_blu :=                {interpolate and stuff BLUE}
        (m1 * rgb_p^.v1.blu / 255.0) +
        (geom.hitx * rgb_p^.v2.blu / 255.0) +
        (geom.hity * rgb_p^.v3.blu / 255.0);
      end;
    if type1_tri_flag_alpha_k in data.flags then begin {alpha data exists per vert ?}
      vp_p^.opac_front :=              {interpolate and stuff opacity}
        (m1 * rgb_p^.v1.alpha / 255.0) +
        (geom.hitx * rgb_p^.v2.alpha / 255.0) +
        (geom.hity * rgb_p^.v3.alpha / 255.0);
      vp_p^.opac_side := vp_p^.opac_front; {set front/side opacity to same value}
      end;
    end;
{
*   Return intersection point if requested.
}
  if ray_geom_point in flags then begin
    geom_info.point := geom.hpnt;
    geom_info.flags :=                 {inidicate intersect point returned}
      geom_info.flags + [ray_geom_point];
    end;
{
*   Return shading normal vector if requested.
}
  if ray_geom_unorm in flags then begin
    if type1_tri_flag_shnorm_k in data.flags
      then begin                       {shading normal supplied at each vertex}
        shn_p := univ_ptr(opt_next);   {get pointer to shading normal optional data}
        geom_info.unorm.x :=           {interpolate shading normal}
          (m1 * shn_p^.v1.x) + (geom.hitx * shn_p^.v2.x) + (geom.hity * shn_p^.v3.x);
        geom_info.unorm.y :=
          (m1 * shn_p^.v1.y) + (geom.hitx * shn_p^.v2.y) + (geom.hity * shn_p^.v3.y);
        geom_info.unorm.z :=
          (m1 * shn_p^.v1.z) + (geom.hitx * shn_p^.v2.z) + (geom.hity * shn_p^.v3.z);
        m := geom.flip_factor / sqrt(  {shading normal unitizing scale factor}
          sqr(geom_info.unorm.x) + sqr(geom_info.unorm.y) + sqr(geom_info.unorm.z));
        geom_info.unorm.x := geom_info.unorm.x * m; {unitize shading normal vect}
        geom_info.unorm.y := geom_info.unorm.y * m;
        geom_info.unorm.z := geom_info.unorm.z * m;
        end
      else begin                       {no shading normals supplied, use geometric}
        geom_info.unorm.x := data.norm.x * geom.flip_factor;
        geom_info.unorm.y := data.norm.y * geom.flip_factor;
        geom_info.unorm.z := data.norm.z * geom.flip_factor;
        end
      ;
    geom_info.flags :=                 {indicate unit normal returned}
      geom_info.flags + [ray_geom_unorm];
    end;
  end;                                 {done with GEOM and DATA abbreviations}
  end;
{
********************************************************************************
}
procedure type1_tri_intersect_box (    {find object/box intersection status}
  in      box: ray_box_t;              {descriptor for a paralellpiped volume}
  in      object: ray_object_t;        {object to intersect with the box}
  out     here: boolean;               {TRUE if ISECT_CHECK could be true in box}
  out     enclosed: boolean);          {TRUE if solid obj, and completely encloses box}
  val_param;

type
  box_corner_t = record                {data we build about each box corner}
    front: boolean;                    {TRUE if point is on front side of triangle}
    dis: real;                         {signed distance from triangle plane to here}
    x, y, z: real;                     {coordinate of this box corner}
    end;

  canonical_2d_t = record              {coordinate in 2D canonical triangle space}
    x: real;
    y: real;
    clip: sys_int_machine_t;           {clip to triangle edge lines status mask}
    end;

var
  data_p: tri_data_p_t;                {pointer to private object data}
  b0: box_corner_t;                    {at box point}
  b1: box_corner_t;                    {at box point + edge 1}
  b2: box_corner_t;                    {at box point + edge 2}
  b3: box_corner_t;                    {at box point + edge 3}
  b4: box_corner_t;                    {at box point + edges 1,2}
  b5: box_corner_t;                    {at box point + edges 1,3}
  b6: box_corner_t;                    {at box point + edges 2,3}
  b7: box_corner_t;                    {at box point + edges 1,2,3}
  pdis0: real;                         {plane distance for box point}
  pdis1: real;                         {plane distance increment for edge 1}
  pdis2: real;                         {plane distance increment for edge 2}
  pdis3: real;                         {plane distance increment for edge 3}
  r1, r2: real;                        {scratch real numbers}
  c: array[1..6] of canonical_2d_t;    {intersection points with box edges and plane}
  n_c: sys_int_machine_t;              {number of intersection points in C array}
  x, y, z: real;                       {scratch 3D object space coordinate}
  clip_acc: sys_int_machine_t;         {low three bits are clip IN flags}
  i, j: sys_int_machine_t;             {loop counters}

label
  in_box, out_box;

begin
  stats_tri.isect_box := stats_tri.isect_box + 1; {one more box intersect check}
  enclosed := false;                   {triangle can never enclose a box}
  data_p := tri_data_p_t(object.data_p); {get pointer to our private data block}
  if data_p = nil then goto out_box;   {no triangle really here ?}
  with data_p^: data do begin          {DATA is our private data block}
{
*   Find the triangle plane distance for the box point, and the plane distance
*   increments for each of the edges.  The plane distance is zero for any point
*   on the plane, positive if on the front side of the plane, and negative if
*   on the back side.  The magnitude is the perpendicular distance from the plane.
*
*   These four scalars will be used to classify each of the box corners as
*   being in front of or behind the triangle plane.  A trivial reject occurs if
*   all 8 box corners are on the same side of the triangle plane.
}
  pdis0 :=                             {distance from plane to box point}
    ((box.point.x - data.p1.x) * data.norm.x) +
    ((box.point.y - data.p1.y) * data.norm.y) +
    ((box.point.z - data.p1.z) * data.norm.z);
  pdis1 :=                             {incremental plane distance for edge 1}
    (box.edge[1].edge.x * data.norm.x) +
    (box.edge[1].edge.y * data.norm.y) +
    (box.edge[1].edge.z * data.norm.z);
  pdis2 :=                             {incremental plane distance for edge 2}
    (box.edge[2].edge.x * data.norm.x) +
    (box.edge[2].edge.y * data.norm.y) +
    (box.edge[2].edge.z * data.norm.z);
  pdis3 :=                             {incremental plane distance for edge 3}
    (box.edge[3].edge.x * data.norm.x) +
    (box.edge[3].edge.y * data.norm.y) +
    (box.edge[3].edge.z * data.norm.z);

  b0.dis := pdis0;                     {save distances from tri plane to box corners}
  b1.dis := pdis0 + pdis1;
  b2.dis := pdis0 + pdis2;
  b3.dis := pdis0 + pdis3;
  b4.dis := b1.dis + pdis2;
  b5.dis := b1.dis + pdis3;
  b6.dis := b2.dis + pdis3;
  b7.dis := b4.dis + pdis3;

  b0.front := b0.dis >= 0.0;           {set flags for corners on front/back side}
  b1.front := b1.dis >= 0.0;
  b2.front := b2.dis >= 0.0;
  b3.front := b3.dis >= 0.0;
  b4.front := b4.dis >= 0.0;
  b5.front := b5.dis >= 0.0;
  b6.front := b6.dis >= 0.0;
  b7.front := b7.dis >= 0.0;

  if                                   {all points are on front side of plane ?}
      b0.front and b1.front and b2.front and b3.front and
      b4.front and b5.front and b6.front and b7.front
    then goto out_box;

  if not (                             {all points are on back side of plane ?}
      b0.front or b1.front or b2.front or b3.front or
      b4.front or b5.front or b6.front or b7.front)
    then goto out_box;
{
*   We now know the box straddles the triangle plane.  Make a list of the
*   intersection points between the plane and the box edges that straddle the
*   plane.  There may be from 3 to 6 of these points.
*
*   We will start by filling in the coordinate of each box point.
}
  b0.x := box.point.x;                 {make box point coordinates}
  b0.y := box.point.y;
  b0.z := box.point.z;

  b1.x := b0.x + box.edge[1].edge.x;
  b1.y := b0.y + box.edge[1].edge.y;
  b1.z := b0.z + box.edge[1].edge.z;

  b2.x := b0.x + box.edge[2].edge.x;
  b2.y := b0.y + box.edge[2].edge.y;
  b2.z := b0.z + box.edge[2].edge.z;

  b3.x := b0.x + box.edge[3].edge.x;
  b3.y := b0.y + box.edge[3].edge.y;
  b3.z := b0.z + box.edge[3].edge.z;

  b4.x := b1.x + box.edge[2].edge.x;
  b4.y := b1.y + box.edge[2].edge.y;
  b4.z := b1.z + box.edge[2].edge.z;

  b5.x := b1.x + box.edge[3].edge.x;
  b5.y := b1.y + box.edge[3].edge.y;
  b5.z := b1.z + box.edge[3].edge.z;

  b6.x := b2.x + box.edge[3].edge.x;
  b6.y := b2.y + box.edge[3].edge.y;
  b6.z := b2.z + box.edge[3].edge.z;

  b7.x := b4.x + box.edge[3].edge.x;
  b7.y := b4.y + box.edge[3].edge.y;
  b7.z := b4.z + box.edge[3].edge.z;

  n_c := 0;                            {init number of intersection points found}

  if b0.front <> b1.front then begin
    n_c := n_c + 1;                    {make index of this new intersect point}
    r1 := b1.dis / (b1.dis - b0.dis);  {weighting factor for first box point}
    r2 := 1.0 - r1;                    {weighting factor for second box point}
    x := (r1 * b0.x) + (r2 * b1.x);    {3D object space intersection coordinate}
    y := (r1 * b0.y) + (r2 * b1.y);
    z := (r1 * b0.z) + (r2 * b1.z);
    x := x - data.p1.x;                {make relative to triangle origin}
    y := y - data.p1.y;
    z := z - data.p1.z;
    c[n_c].x := (x * data.xbx) + (y * data.ybx) + (z * data.zbx); {2D canonical coor}
    c[n_c].y := (x * data.xby) + (y * data.yby) + (z * data.zby);
    end;

  if b0.front <> b2.front then begin
    n_c := n_c + 1;                    {make index of this new intersect point}
    r1 := b2.dis / (b2.dis - b0.dis);  {weighting factor for first box point}
    r2 := 1.0 - r1;                    {weighting factor for second box point}
    x := (r1 * b0.x) + (r2 * b2.x);    {3D object space intersection coordinate}
    y := (r1 * b0.y) + (r2 * b2.y);
    z := (r1 * b0.z) + (r2 * b2.z);
    x := x - data.p1.x;                {make relative to triangle origin}
    y := y - data.p1.y;
    z := z - data.p1.z;
    c[n_c].x := (x * data.xbx) + (y * data.ybx) + (z * data.zbx); {2D canonical coor}
    c[n_c].y := (x * data.xby) + (y * data.yby) + (z * data.zby);
    end;

  if b0.front <> b3.front then begin
    n_c := n_c + 1;                    {make index of this new intersect point}
    r1 := b3.dis / (b3.dis - b0.dis);  {weighting factor for first box point}
    r2 := 1.0 - r1;                    {weighting factor for second box point}
    x := (r1 * b0.x) + (r2 * b3.x);    {3D object space intersection coordinate}
    y := (r1 * b0.y) + (r2 * b3.y);
    z := (r1 * b0.z) + (r2 * b3.z);
    x := x - data.p1.x;                {make relative to triangle origin}
    y := y - data.p1.y;
    z := z - data.p1.z;
    c[n_c].x := (x * data.xbx) + (y * data.ybx) + (z * data.zbx); {2D canonical coor}
    c[n_c].y := (x * data.xby) + (y * data.yby) + (z * data.zby);
    end;

  if b4.front <> b1.front then begin
    n_c := n_c + 1;                    {make index of this new intersect point}
    r1 := b1.dis / (b1.dis - b4.dis);  {weighting factor for first box point}
    r2 := 1.0 - r1;                    {weighting factor for second box point}
    x := (r1 * b4.x) + (r2 * b1.x);    {3D object space intersection coordinate}
    y := (r1 * b4.y) + (r2 * b1.y);
    z := (r1 * b4.z) + (r2 * b1.z);
    x := x - data.p1.x;                {make relative to triangle origin}
    y := y - data.p1.y;
    z := z - data.p1.z;
    c[n_c].x := (x * data.xbx) + (y * data.ybx) + (z * data.zbx); {2D canonical coor}
    c[n_c].y := (x * data.xby) + (y * data.yby) + (z * data.zby);
    end;

  if b4.front <> b2.front then begin
    n_c := n_c + 1;                    {make index of this new intersect point}
    r1 := b2.dis / (b2.dis - b4.dis);  {weighting factor for first box point}
    r2 := 1.0 - r1;                    {weighting factor for second box point}
    x := (r1 * b4.x) + (r2 * b2.x);    {3D object space intersection coordinate}
    y := (r1 * b4.y) + (r2 * b2.y);
    z := (r1 * b4.z) + (r2 * b2.z);
    x := x - data.p1.x;                {make relative to triangle origin}
    y := y - data.p1.y;
    z := z - data.p1.z;
    c[n_c].x := (x * data.xbx) + (y * data.ybx) + (z * data.zbx); {2D canonical coor}
    c[n_c].y := (x * data.xby) + (y * data.yby) + (z * data.zby);
    end;

  if b4.front <> b7.front then begin
    n_c := n_c + 1;                    {make index of this new intersect point}
    r1 := b7.dis / (b7.dis - b4.dis);  {weighting factor for first box point}
    r2 := 1.0 - r1;                    {weighting factor for second box point}
    x := (r1 * b4.x) + (r2 * b7.x);    {3D object space intersection coordinate}
    y := (r1 * b4.y) + (r2 * b7.y);
    z := (r1 * b4.z) + (r2 * b7.z);
    x := x - data.p1.x;                {make relative to triangle origin}
    y := y - data.p1.y;
    z := z - data.p1.z;
    c[n_c].x := (x * data.xbx) + (y * data.ybx) + (z * data.zbx); {2D canonical coor}
    c[n_c].y := (x * data.xby) + (y * data.yby) + (z * data.zby);
    end;

  if b5.front <> b1.front then begin
    n_c := n_c + 1;                    {make index of this new intersect point}
    r1 := b1.dis / (b1.dis - b5.dis);  {weighting factor for first box point}
    r2 := 1.0 - r1;                    {weighting factor for second box point}
    x := (r1 * b5.x) + (r2 * b1.x);    {3D object space intersection coordinate}
    y := (r1 * b5.y) + (r2 * b1.y);
    z := (r1 * b5.z) + (r2 * b1.z);
    x := x - data.p1.x;                {make relative to triangle origin}
    y := y - data.p1.y;
    z := z - data.p1.z;
    c[n_c].x := (x * data.xbx) + (y * data.ybx) + (z * data.zbx); {2D canonical coor}
    c[n_c].y := (x * data.xby) + (y * data.yby) + (z * data.zby);
    end;

  if b5.front <> b3.front then begin
    n_c := n_c + 1;                    {make index of this new intersect point}
    r1 := b3.dis / (b3.dis - b5.dis);  {weighting factor for first box point}
    r2 := 1.0 - r1;                    {weighting factor for second box point}
    x := (r1 * b5.x) + (r2 * b3.x);    {3D object space intersection coordinate}
    y := (r1 * b5.y) + (r2 * b3.y);
    z := (r1 * b5.z) + (r2 * b3.z);
    x := x - data.p1.x;                {make relative to triangle origin}
    y := y - data.p1.y;
    z := z - data.p1.z;
    c[n_c].x := (x * data.xbx) + (y * data.ybx) + (z * data.zbx); {2D canonical coor}
    c[n_c].y := (x * data.xby) + (y * data.yby) + (z * data.zby);
    end;

  if b5.front <> b7.front then begin
    n_c := n_c + 1;                    {make index of this new intersect point}
    r1 := b7.dis / (b7.dis - b5.dis);  {weighting factor for first box point}
    r2 := 1.0 - r1;                    {weighting factor for second box point}
    x := (r1 * b5.x) + (r2 * b7.x);    {3D object space intersection coordinate}
    y := (r1 * b5.y) + (r2 * b7.y);
    z := (r1 * b5.z) + (r2 * b7.z);
    x := x - data.p1.x;                {make relative to triangle origin}
    y := y - data.p1.y;
    z := z - data.p1.z;
    c[n_c].x := (x * data.xbx) + (y * data.ybx) + (z * data.zbx); {2D canonical coor}
    c[n_c].y := (x * data.xby) + (y * data.yby) + (z * data.zby);
    end;

  if b6.front <> b2.front then begin
    n_c := n_c + 1;                    {make index of this new intersect point}
    r1 := b2.dis / (b2.dis - b6.dis);  {weighting factor for first box point}
    r2 := 1.0 - r1;                    {weighting factor for second box point}
    x := (r1 * b6.x) + (r2 * b2.x);    {3D object space intersection coordinate}
    y := (r1 * b6.y) + (r2 * b2.y);
    z := (r1 * b6.z) + (r2 * b2.z);
    x := x - data.p1.x;                {make relative to triangle origin}
    y := y - data.p1.y;
    z := z - data.p1.z;
    c[n_c].x := (x * data.xbx) + (y * data.ybx) + (z * data.zbx); {2D canonical coor}
    c[n_c].y := (x * data.xby) + (y * data.yby) + (z * data.zby);
    end;

  if b6.front <> b3.front then begin
    n_c := n_c + 1;                    {make index of this new intersect point}
    r1 := b3.dis / (b3.dis - b6.dis);  {weighting factor for first box point}
    r2 := 1.0 - r1;                    {weighting factor for second box point}
    x := (r1 * b6.x) + (r2 * b3.x);    {3D object space intersection coordinate}
    y := (r1 * b6.y) + (r2 * b3.y);
    z := (r1 * b6.z) + (r2 * b3.z);
    x := x - data.p1.x;                {make relative to triangle origin}
    y := y - data.p1.y;
    z := z - data.p1.z;
    c[n_c].x := (x * data.xbx) + (y * data.ybx) + (z * data.zbx); {2D canonical coor}
    c[n_c].y := (x * data.xby) + (y * data.yby) + (z * data.zby);
    end;

  if b6.front <> b7.front then begin
    n_c := n_c + 1;                    {make index of this new intersect point}
    r1 := b7.dis / (b7.dis - b6.dis);  {weighting factor for first box point}
    r2 := 1.0 - r1;                    {weighting factor for second box point}
    x := (r1 * b6.x) + (r2 * b7.x);    {3D object space intersection coordinate}
    y := (r1 * b6.y) + (r2 * b7.y);
    z := (r1 * b6.z) + (r2 * b7.z);
    x := x - data.p1.x;                {make relative to triangle origin}
    y := y - data.p1.y;
    z := z - data.p1.z;
    c[n_c].x := (x * data.xbx) + (y * data.ybx) + (z * data.zbx); {2D canonical coor}
    c[n_c].y := (x * data.xby) + (y * data.yby) + (z * data.zby);
    end;
{
*   All the intersection points between the box edges and the triangle plane
*   have been found.  Their 2D canonical triangle space coordinates are in the
*   C array.  N_C is the number of these intersection points, which must be from
*   3 to 6.
*
*   Now classify each point as being inside or outside the triangle.  If any
*   point is inside, then we are done immediately.  A clip status mask, CLIP_ACC,
*   be kept to accumulate all the IN clip results for each line.  If each point
*   is OUT for the same triangle edge line, then there is definately no intersection.
}
  clip_acc := 0;                       {init to no clip IN conditions found}

  for i := 1 to n_c do begin           {once for each intersection point}
    c[i].clip := 0;                    {init to no clip IN conditions for this point}
    if c[i].x >= 0.0
      then c[i].clip := 1;
    if c[i].y >= 0.0
      then c[i].clip := c[i].clip ! 2;
    if (c[i].x + c[i].y) <= 1.0
      then c[i].clip := c[i].clip ! 4;
    if c[i].clip = 7 then goto in_box; {point is within triangle ?}
    clip_acc := clip_acc ! c[i].clip;  {accumulate clip IN conditions}
    end;

  if clip_acc <> 7 then goto out_box;  {all points are clipped by same edge ?}
{
*   All the easy stuff has failed to decide whether the triangle is in the box.
*   We have reduced it to a 2D problem where the points in C are the
*   edges of the box intersected with the triangle plane in the canonical
*   triangle space.  In this space, the triangle extends from 0,0 to 1,0 to 0,1.
*
*   We also know that none of the polygon points lie inside the triangle, and
*   that the polygon is convex.  Unfortunately we have no idea about the order
*   of the polygon verticies.
*
*   We will check each possible edge by whether it intersects any of the triangle
*   sides.  If none of them do, then the polygon either completely encloses
*   the triangle or is completely disjoint.  To determine which is which, we
*   will keep track of whether the polygon intersected the X axis on both sides
*   of the triangle or not.  If it did, then the polygon is enclosing the triangle.
*
*   Since we don't know which line segments between the polygon verticies are
*   the polygon sides, we will check all possible line segments.  At worst, this
*   will cause us to check 15 segments instead of 6.
*
*   The bit values in the CLIP field for each vertex have the following meaning:
*     1  -  X >= 0
*     2  -  Y >= 0
*     4  -  X+Y <= 1
*
*   We will be using bits in CLIP_ACC in the following way:
*     1  -  X intersect > 1
*     2  -  X intersect < 0
}
  clip_acc := 0;
  for i := 1 to n_c-1 do begin         {outer loop for first vertex of segment}
    c[i].clip := c[i].clip & 3;        {select only X and Y clip check bits}
    for j := i+1 to n_c do begin       {inner loop for second vertex of segment}

      if (c[i].clip & 2) <> (c[j].clip & 2) then begin {segment crosses X axis ?}
        r1 := c[j].y / (c[j].y - c[i].y); {weighting factor for first vertex}
        x := (r1 * c[i].x) + ((1.0 - r1) * c[j].x); {X axis intersect}
        if x >= 0.0
          then begin
            if x <= 1.0 then goto in_box; {intersected triangle directly ?}
            clip_acc := clip_acc ! 1;
            end
          else begin
            clip_acc := clip_acc ! 2
            end
          ;
        if clip_acc = 3 then goto in_box; {polygon is enclosing triangle ?}
        end;                           {done handling X axis intersection}

      if (c[i].clip & 1) <> (c[j].clip & 1) then begin {segment crosses Y axis ?}
        r1 := c[j].x / (c[j].x - c[i].x); {weighting factor for first vertex}
        y := (r1 * c[i].y) + ((1.0 - r1) * c[j].y); {Y axis intersect}
        if (y >= 0.0) and (y <= 1.0) then goto in_box; {intersects tri directly ?}
        end;                           {done handling Y axis intersection}

      end;                             {back for new end of segment vertex}
    end;                               {back for new start of segment vertex}
  end;                                 {done with DATA abbreviation}

out_box:                               {jump here if no part of tri is inside box}
  here := false;                       {the triangle is not in the box}
  return;

in_box:                                {jump here if some part of tri is inside box}
  here := true;                        {some part of triangle is in the box}
  stats_tri.hit_box := stats_tri.hit_box + 1; {one more tri/box intersect found}
  end;
{
********************************************************************************
}
procedure type1_tri_add_child (        {Add child to this object (illegal)}
  in      aggr_obj: ray_object_t;      {aggregate object to add child to}
  var     object: ray_object_t);       {child object to add to aggregate object}
  val_param;

begin
  sys_message_bomb ('ray_type1', 'tri_not_aggregate', nil, 0);
  end;
{
********************************************************************************
}
procedure type1_stats_init;
  val_param;

begin
  stats_tri.isect_ray := 0;            {init triangle stats}
  stats_tri.hit_ray := 0;
  stats_tri.isect_box := 0;
  stats_tri.hit_box := 0;

  stats_oct.n_parent := 0;             {init octree stats}
  stats_oct.n_leaf := 0;
  stats_oct.mem := 0;
  end;
{
********************************************************************************
}
procedure type1_stats_print;
  val_param;

var
  i: sys_int_machine_t;
  r: real;

begin
  writeln;
  writeln ('Ray tracing triangle statistics:');
  writeln ('  Ray intersect checks: ', stats_tri.isect_ray:9);
  writeln ('  Ray hits:             ', stats_tri.hit_ray:9);
  writeln ('  Box intersect checks: ', stats_tri.isect_box:9);
  writeln ('  Box hits:             ', stats_tri.hit_box:9);

  writeln;
  writeln ('Ray tracing octree statistics:');
  writeln ('  Parent nodes:         ', stats_oct.n_parent:9);
  writeln ('  Leaf voxels:          ', stats_oct.n_leaf:9);
  i := stats_oct.n_parent + stats_oct.n_leaf;
  writeln ('  Total voxels:         ', i:9);
  r := stats_oct.mem / 1048576.0;
  writeln ('  Mbytes dynamic memory ', r:12:2);
  end;
{
********************************************************************************
*
*   Subroutine TYPE1_TRI_ROUTINES_MAKE (CLASS)
*
*   Fill in the routines block for this class of objects.
}
procedure type1_tri_routines_make (    {fill in object routines block}
  out     class: ray_object_class_t);  {block to fill in}
  val_param;

begin
  class.create := addr(type1_tri_create);
  class.intersect_check := addr(type1_tri_intersect_check);
  class.hit_geom := addr(type1_tri_intersect_geom);
  class.intersect_box := addr(type1_tri_intersect_box);
  class.add_child := addr(type1_tri_add_child);
  end;
