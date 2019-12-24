{   Type 1 OCTREE aggregate object.  The octree is rectangular and axis aligned.
}
module type1_octree;
define type1_octree_routines_make;
define type1_octree_geom;
%include 'ray_type1_2.ins.pas';
%include 'type1_octree.ins.pas';

const
  max_coor_level = 15;                 {how many subdivision levels to keep data for}
  int_scale = 268435456.0;             {octree floating point to integer coordinates}
  box_radius_scale = 1.0/int_scale;    {pick mask to voxel radius scale factor}
  iter_below_minvox = 3;               {coor iterations to do beyond min voxel size}
{
*   Static storage.  This storage is allocated at load time and therefore should
*   not be altered.
}
var
  coor_masks:                          {mask in address bits for each voxel size}
    array[0..max_coor_level] of integer32 := [
    16#F0000000,
    16#F8000000,
    16#FC000000,
    16#FE000000,
    16#FF000000,
    16#FF800000,
    16#FFC00000,
    16#FFE00000,
    16#FFF00000,
    16#FFF80000,
    16#FFFC0000,
    16#FFFE0000,
    16#FFFF0000,
    16#FFFF8000,
    16#FFFFC000,
    16#FFFFE000];
  pick_masks:                          {mask in bit for selecting next voxel down}
    array[0..max_coor_level] of integer32 := [
    16#08000000,
    16#04000000,
    16#02000000,
    16#01000000,
    16#00800000,
    16#00400000,
    16#00200000,
    16#00100000,
    16#00080000,
    16#00040000,
    16#00020000,
    16#00010000,
    16#00008000,
    16#00004000,
    16#00002000,
    16#00001000];
{
********************************************************************************
*
*   Local subroutine TYPE1_OCTREE_VERSION (VERSION)
*
*   Return version information obout this class of objects.
}
procedure type1_octree_version (       {return version information about object}
  out     version: ray_object_version_t); {returned version information}
  val_param;

begin
  version.year := 1991;
  version.month := 5;
  version.day := 3;
  version.hour := 10;
  version.minute := 0;
  version.second := 0;
  version.version := 0;
  version.name := string_v('OCTREE');
  version.aggregate := true;
  end;
{
********************************************************************************
*
*   Local subroutine TYPE1_OCTREE_CREATE (OBJECT, CREA, STAT)
*
*   Fill in the new object in OBJECT.  CREA is the user data parameters for
*   this object.  STAT is the standard system error return code.
}
procedure type1_octree_create (        {create new primitive with custom data}
  in out  object: ray_object_t;        {newly filled in object block}
  in var  crea: univ ray_crea_data_t;  {data for creating the new object}
  out     stat: sys_err_t);            {completion status code}
  val_param;

var
  crea_p: type1_octree_crea_data_p_t;  {pointer to our specific creation data}
  data_p: oct_data_p_t;                {pointer to internal object data}
  step_level: sys_int_machine_t;       {voxel level of iteration step size}
  box_err: real;                       {size of voxel intersect box size grow}
  sz: sys_int_adr_t;                   {memory size}

begin
  sys_error_none (stat);               {init to no error}

  crea_p := univ_ptr(addr(crea));      {pointer to our specific creation data}

  sz := sizeof(data_p^);               {amount of memory to allocate}
  util_mem_grab (                      {allocate data block for new object}
    sz, ray_mem_p^, false, data_p);
  stats_oct.mem := stats_oct.mem + sz;

  object.data_p := data_p;             {set pointer to object data block}
  data_p^.shader := crea_p^.shader;    {copy pointer to shader to use}
  data_p^.liparm_p := crea_p^.liparm_p; {copy pointer to light source block}
  data_p^.visprop_p := crea_p^.visprop_p; {copy pointer to visual properties block}
  data_p^.min_gen := crea_p^.min_gen;  {copy other data specific to this object}
  data_p^.max_gen := crea_p^.max_gen;
  data_p^.min_miss := crea_p^.min_miss;
  data_p^.origin := crea_p^.origin;
  data_p^.size := crea_p^.size;
{
*   All the user's data has been copied.  Now init the rest of the local data for this
*   object.
}
  data_p^.recip_size.x := 1.0/data_p^.size.x; {make reciprocal sizes}
  data_p^.recip_size.y := 1.0/data_p^.size.y;
  data_p^.recip_size.z := 1.0/data_p^.size.z;
  sz := sizeof(data_p^.ar_p^);         {amount of memory to allocate}
  util_mem_grab (                      {allocate an initial block of nodes}
    sz, ray_mem_p^, false, data_p^.ar_p);
  stats_oct.mem := stats_oct.mem + sz;
  data_p^.top_node_p := addr(data_p^.ar_p^[1]); {set pointer to top level node}
  data_p^.n_ar := 1;                   {init to one node allocated in current array}
  data_p^.top_node_p^.n_obj := 0;      {init node to empty leaf node}
  data_p^.top_node_p^.hits := 0;       {init to no rays passed thru here}
  data_p^.top_node_p^.misses := 0;
  data_p^.top_node_p^.next_p := nil;   {init to no data nodes chained on}
  data_p^.free_p := nil;               {init to no nodes on free chain}
  data_p^.next_p := addr(data_p^.top_node_p^.obj_p[1]); {init adr of next obj pointer}
  data_p^.last_left := obj_per_leaf_node; {init num object pointers left this node}

  step_level :=                        {voxel level of iteration steps}
    min(max_coor_level, data_p^.max_gen + iter_below_minvox);
  box_err :=                           {intersect box size increment in octree space}
    1.0 / lshft(1, step_level);
  data_p^.box_err.x := box_err * data_p^.size.x;
  data_p^.box_err.y := box_err * data_p^.size.y;
  data_p^.box_err.z := box_err * data_p^.size.z;
  data_p^.box_err_half.x := data_p^.box_err.x / 2.0;
  data_p^.box_err_half.y := data_p^.box_err.y / 2.0;
  data_p^.box_err_half.z := data_p^.box_err.z / 2.0;

  stats_oct.n_leaf := stats_oct.n_leaf + 1; {one more leaf node voxel}
  end;
{
********************************************************************************
*
*   Local function TYPE1_OCTREE_INTERSECT_CHECK (
*     GRAY, OBJECT, GPARMS, HIT_INFO, SHADER)
*
*   Check ray and object for an intersection.  If so, return TRUE, and save any
*   partial results in HIT_INFO.  These partial results may be used later to get
*   detailed information about the intersection geometry.
}
function type1_octree_intersect_check ( {check for ray/object intersection}
  in out  gray: univ ray_desc_t;       {ray descriptor}
  in      object: ray_object_t;        {object to intersect ray with}
  in      gparms: univ ray_object_parms_t; {run-time parameters passed from above}
  out     hit_info: ray_hit_info_t;    {all returned information}
  out     shader: ray_shader_t)        {pointer to shader to resolve color}
  :boolean;                            {TRUE if ray does hit object}
  val_param;

const
  mic = 4.76837E-7;                    {min representable coor in 0.0 to 1.0 space}
  mac = 1.0 - mic;                     {max representable coor in 0.0 to 1.0 space}
  cached_checks = 8;                   {previously checked objects list max size}

type
  icoor_t = record case integer of     {integer XYZ coordinate with array overlay}
    1:(                                {individually named fields}
      x, y, z: sys_int_machine_t);
    2:(                                {overlay array of algorithmic indexing}
      coor: array[1..3] of sys_int_machine_t);
    end;

  vstack_entry_t = record              {data about current voxel at one level}
    icoor: icoor_t;                    {voxel coordinate with unused bits set to zero}
    p: node_p_t;                       {pointer to voxel data}
    parent_p: node_pp_t;               {adr of pointer to the node at this level}
    end;

var
  ray_p: type1_ray_p_t;                {pointer to the ray in our format}
  uparms_p: type1_object_parms_p_t;    {pointer to object parameters in our format}
  r: real;                             {scratch floating point number}
  data_p: oct_data_p_t;                {pointer to object's specific data area}
  p: vect3_t;                          {octree ray start point}
  orayp: vect_3d_t;                    {ray point in octree 0,0,0 to 1,1,1 space}
  v: vect3_t;                          {non-unit ray vector in 0 to 1 space}
  vsq: vect3_t;                        {V with each component squared}
  maj, min1, min2: sys_int_machine_t;  {1-3 subscripts for major and minor axies}
  slope: vect3_t;                      {slope of each axis with respect to major axis}
  dmaj: real;                          {major axis delta}
  p1, p2: vect3_t;                     {scratch points}
  level: sys_int_machine_t;            {nesting level of current node, top = 0}
  coor_mask: integer32;                {masks in relevant adr bits for this level}
  icoor: icoor_t;                      {32 bit integer XYZ coordinate}
  icoor2: icoor_t;                     {scratch integer coordinate}
  max_iter_level: sys_int_machine_t;   {max level for iterating new coordinate}
  vox_icoor: icoor_t;                  {voxel coordinate with unused bits set to zero}
  coor_step:                           {delta ICOOR when major axis steps one voxel}
    array[0..max_coor_level] of icoor_t;
  vstack:                              {data about each nested voxel level}
    array[0..max_coor_level] of vstack_entry_t;
  vp: node_p_t;                        {pointer to current voxel}
  i, j: sys_int_machine_t;             {loop counters and scratch integers}
  box: ray_box_t;                      {paralellpiped for voxel intersection checks}
  box_coor: array[0..1] of vect_3d_t;  {possible box corner points}
  newp: node_p_t;                      {pointer to new subdivided voxel}
  obj_pp: ^ray_object_p_t;             {adr of next obj pnt in scanning voxel list}
  vlnp: node_p_t;                      {pointer to curr node in scanning voxel list}
  nleft: sys_int_machine_t;            {obj pointers left this node in scanning list}
  new_left: sys_int_machine_t;         {num obj pointers left in node being built}
  n_new: sys_int_machine_t;            {total number of objects in voxel being built}
  new_pp: ^ray_object_p_t;             {adr of next obj pointer in node being built}
  nnewp: node_p_t;                     {pointer to node being built}
  parms: type1_object_parms_t;         {parameters to pass to subordinate objects}
  old_mem: sys_int_adr_t;              {MEM index before any object hits}
  new_mem: sys_int_adr_t;              {MEM index after last object hit}
  last_checks:                         {cached object pointers of recent misses}
    array[1..cached_checks] of ray_object_p_t;
  n_cached: sys_int_machine_t;         {number of object pointers in LAST_CHECKS array}
  next_cache: sys_int_machine_t;       {LAST_checks index of where to put next obj pnt}
  old_max_dist: real;                  {saved copy of RAY.MAX_DIST}
  parent_p: node_pp_t;                 {address of pointer to current voxel}
  sz: sys_int_adr_t;                   {memory size}
  ray_ends: boolean;                   {TRUE if ray ends withing octree}
  here: boolean;                       {flag indicating ray can hit object in box}
  enclosed: boolean;                   {flag indicating object completely encloses box}
  hit: boolean;                        {the ray hit an object}

label
  got_p, new_coor, got_start_level, new_voxel, leaf_node, subdivide_voxel,
  trace_voxel, next_obj, next_coor, no_hit, leave;

begin
  data_p := oct_data_p_t(object.data_p); {make pointer to object's data area}
  ray_p := univ_ptr(addr(gray));       {pointer to ray descriptor}
  uparms_p := univ_ptr(addr(gparms));  {pointer to runtime parameters for this object}
  with                                 {set up abbreviations}
    data_p^:d,
    ray_p^:ray
    do begin
{
*   Abbreviations:
*   D  -  Specific data block for OCTREE object
*
*   Now transform the ray into the (0,0,0) to (1,1,1) octree space.
}
  orayp.x := (ray.point.x  - d.origin.x)*d.recip_size.x; {octree space ray point}
  orayp.y := (ray.point.y  - d.origin.y)*d.recip_size.y;
  orayp.z := (ray.point.z  - d.origin.z)*d.recip_size.z;
  v.x := ray.vect.x*d.recip_size.x;    {transform ray vector to our space (not unity)}
  v.y := ray.vect.y*d.recip_size.y;
  v.z := ray.vect.z*d.recip_size.z;
  p.x := orayp.x + v.x*ray.min_dist;   {first valid ray point in octree space}
  p.y := orayp.y + v.y*ray.min_dist;
  p.z := orayp.z + v.z*ray.min_dist;
  ray_ends := ray.max_dist < 1.0E20;   {set flag if ray effectively has ending point}
  vsq.x := sqr(v.x);                   {square each component of unscaled vector}
  vsq.y := sqr(v.y);
  vsq.z := sqr(v.z);
{
*   The ray point and ray vector have been transformed into a space in which the
*   octree occupies (0,0,0) to (1,1,1).  The transformed ray start point is in P,
*   and the transformed ray vector is in V.  V is a unit vector.  This transformed
*   space is only used to decide which voxels the ray hits.  The original ray is
*   passed directly to the subordinate object intersection routines.
*
*   Start by determining which is the major axis and the two minor axies.
}
  if vsq.x > vsq.y                     {sort based on X and Y alone}
    then begin
      maj := 1;
      min2 := 2;
      end
    else begin
      maj := 2;
      min2 := 1;
      end
    ;
  if vsq.z > vsq.coor[maj]             {sort biggest against Z}
    then maj := 3;
  if vsq.z < vsq.coor[min2]            {sort smallest against Z}
    then min2 := 3;
  min1 := 6 - maj - min2;              {middle size must be the one left over}
{
*   MAJ, MIN1, MIN2 are indicies for the most major to the most minor axies.
}
  slope.coor[maj] := 1.0;              {find slopes with respect to major axis}
  slope.coor[min1] := v.coor[min1]/v.coor[maj];
  slope.coor[min2] := v.coor[min2]/v.coor[maj];
  if (p.x >= mic) and (p.x <= mac)     {is ray point inside octree ?}
      and (p.y >= mic) and (p.y <= mac)
      and (p.z >= mic) and (p.z <= mac)
      then begin                       {start point is just the ray point}
    goto got_p;                        {point P is all set}
    end;
{
*   The ray start point is not inside the octree.  Make points P1 and P2 which are
*   the intersection points between the ray and the front and back planes perpendicular
*   to the major axis.  P1 and P2 are clipped to the ray starting point.
}
  if (p.coor[maj] < mic) and (v.coor[maj] < 0.0) {start behind and heading away ?}
    then goto no_hit;
  if (p.coor[maj] > mac) and (v.coor[maj] > 0.0) {start in front and heading more so ?}
    then goto no_hit;
  if ((p.coor[maj] < mic) and (v.coor[maj] > 0.0)) {intersecting MAJ=MIC plane ?}
      or ((p.coor[maj] > mic) and (v.coor[maj] < 0.0))
    then begin                         {the ray hits the MAJ=MIC plane}
      dmaj := mic - p.coor[maj];       {major axis distance to plane}
      p1.coor[maj] := mic;             {find intersect point with  MAJ=MIC plane}
      p1.coor[min1] := p.coor[min1] + dmaj*slope.coor[min1];
      p1.coor[min2] := p.coor[min2] + dmaj*slope.coor[min2];
      end
    else begin                         {ray does not hit the MAJ=MIC plane}
      p1 := p;                         {ray start is the MAJ minimum point}
      end
    ;                                  {P1 set to major axis minimum point}
  if ((p.coor[maj] > mac) and (v.coor[maj] < 0.0)) {intersecting MAJ=MAC plane ?}
      or ((p.coor[maj] < mac) and (v.coor[maj] > 0.0))
    then begin                         {ray hits the MAJ=MAC plane}
      dmaj := mac - p.coor[maj];       {major axis distance to plane}
      p2.coor[maj] := mac;             {find intersect point with  MAJ=MAC plane}
      p2.coor[min1] := p.coor[min1] + dmaj*slope.coor[min1];
      p2.coor[min2] := p.coor[min2] + dmaj*slope.coor[min2];
      end
    else begin                         {ray does not hit the MAJ=MAC plane}
      p2 := p;                         {ray start is the MAJ maximum point}
      end
    ;                                  {P2 set to major axis maximum point}
{
*   The line segment P1 to P2 is the segment of the ray passing thru the major axis
*   MIC to MAC slice.  P1 to P2 are in order of increasing major axis.  Now clip the
*   line segment against the MIN1 and MIN2 MIC to MAC slices.  The resulting line
*   segment is the intersection of the ray and the outside of the octree.
}
  if slope.coor[min1] >= 0.0           {check P1-->P2 direction relative to MIN1 axis}
    then begin                         {P1 --> P2 goes along increasing MIN1}
      if p1.coor[min1] > mac then goto no_hit; {ray starts past MIN1 slice ?}
      if p2.coor[min1] < mic then goto no_hit; {ray ends before MIN1 slice ?}
      if p1.coor[min1] < mic then begin {ray starts before hitting MIN1 slice ?}
        dmaj :=                        {major axis delta from P1 to MIN1=MIC}
          (mic - p.coor[min1])/slope.coor[min1];
        p1.coor[maj] := p.coor[maj] + dmaj; {P1-->P2 intersect point with MIN1=MIC}
        p1.coor[min1] := mic;
        p1.coor[min2] := p.coor[min2] + dmaj*slope.coor[min2];
        end;                           {done adjusting P1 to MIN1 slice}
      if p2.coor[min1] > mac then begin {need to adjust P2 coordinate ?}
        dmaj :=                        {major axis delta from P1 to MIN1=MAC}
          (mac - p.coor[min1])/slope.coor[min1];
        p2.coor[maj] := p.coor[maj] + dmaj; {P1-->P2 intersect point with MIN1=MAC}
        p2.coor[min1] := mac;
        p2.coor[min2] := p.coor[min2] + dmaj*slope.coor[min2];
        end;                           {done adjusting P2 to MIN1 slice}
      end
    else begin                         {P2 --> P1 goes along decreasing MIN1}
      if p2.coor[min1] > mac then goto no_hit; {ray starts past MIN1 slice ?}
      if p1.coor[min1] < mic then goto no_hit; {ray ends before MIN1 slice ?}
      if p2.coor[min1] < mic then begin {ray starts before hitting MIN1 slice ?}
        dmaj :=                        {major axis delta from P2 to MIN1=MIC}
          (mic - p.coor[min1])/slope.coor[min1];
        p2.coor[maj] := p.coor[maj] + dmaj; {P2-->P1 intersect point with MIN1=MIC}
        p2.coor[min1] := mic;
        p2.coor[min2] := p.coor[min2] + dmaj*slope.coor[min2];
        end;                           {done adjusting P2 to MIN1 slice}
      if p1.coor[min1] > mac then begin {need to adjust P1 coordinate ?}
        dmaj :=                        {major axis delta from P2 to MIN1=MAC}
          (mac - p.coor[min1])/slope.coor[min1];
        p1.coor[maj] := p.coor[maj] + dmaj; {P2-->P1 intersect point with MIN1=MAC}
        p1.coor[min1] := mac;
        p1.coor[min2] := p.coor[min2] + dmaj*slope.coor[min2];
        end;                           {done adjusting P1 to MIN1 slice}
      end
    ;                                  {done clipping segment to MIN1 slice}

  if slope.coor[min2] >= 0.0           {check P1-->P2 direction relative to MIN2 axis}
    then begin                         {P1 --> P2 goes along increasing MIN2}
      if p1.coor[min2] > mac then goto no_hit; {ray starts past MIN2 slice ?}
      if p2.coor[min2] < mic then goto no_hit; {ray ends before MIN2 slice ?}
      if p1.coor[min2] < mic then begin {ray starts before hitting MIN2 slice ?}
        dmaj :=                        {major axis delta from P1 to MIN2=MIC}
          (mic - p.coor[min2])/slope.coor[min2];
        p1.coor[maj] := p.coor[maj] + dmaj; {P1-->P2 intersect point with MIN2=MIC}
        p1.coor[min2] := mic;
        p1.coor[min1] := p.coor[min1] + dmaj*slope.coor[min1];
        end;                           {done adjusting P1 to MIN2 slice}
      if p2.coor[min2] > mac then begin {need to adjust P2 coordinate ?}
        dmaj :=                        {major axis delta from P1 to MIN2=MAC}
          (mac - p.coor[min2])/slope.coor[min2];
        p2.coor[maj] := p.coor[maj] + dmaj; {P1-->P2 intersect point with MIN2=MAC}
        p2.coor[min2] := mac;
        p2.coor[min1] := p.coor[min1] + dmaj*slope.coor[min1];
        end;                           {done adjusting P2 to MIN2 slice}
      end
    else begin                         {P2 --> P1 goes along decreasing MIN2}
      if p2.coor[min2] > mac then goto no_hit; {ray starts past MIN2 slice ?}
      if p1.coor[min2] < mic then goto no_hit; {ray ends before MIN2 slice ?}
      if p2.coor[min2] < mic then begin {ray starts before hitting MIN2 slice ?}
        dmaj :=                        {major axis delta from P2 to MIN2=MIC}
          (mic - p.coor[min2])/slope.coor[min2];
        p2.coor[maj] := p.coor[maj] + dmaj; {P2-->P1 intersect point with MIN2=MIC}
        p2.coor[min2] := mic;
        p2.coor[min1] := p.coor[min1] + dmaj*slope.coor[min1];
        end;                           {done adjusting P2 to MIN2 slice}
      if p1.coor[min2] > mac then begin {need to adjust P1 coordinate ?}
        dmaj :=                        {major axis delta from P2 to MIN2=MAC}
          (mac - p.coor[min2])/slope.coor[min2];
        p1.coor[maj] := p.coor[maj] + dmaj; {P2-->P1 intersect point with MIN2=MAC}
        p1.coor[min2] := mac;
        p1.coor[min1] := p.coor[min1] + dmaj*slope.coor[min1];
        end;                           {done adjusting P1 to MIN2 slice}
      end
    ;                                  {done clipping segment to MIN2 slice}
{
*   The line segment P1 to P2 is the part of the ray that intersects the outer box
*   of the octree.  P1 to P2 is in order of increasing major axis.  Now set P to the
*   ray start point inside the octree.
}
  if v.coor[maj] >= 0.0                {check MAJ direction with respect to ray dir}
    then begin                         {ray is in increasing MAJ direction}
      p := p1;
      end
    else begin                         {ray is in decreasing MAJ direction}
      p := p2;
      end
    ;
{
*   P is the first point along the ray where we are allowed to look for hits that
*   is also inside the octree.  P is in the octree (0,0,0) to (1,1,1) space.
*   Now do remaining initialization before entering main voxel stepping loop.
}
got_p:                                 {jump here if P started out inside octree}
  old_max_dist := ray.max_dist;        {save field we will corrupt later}
  if d.shader = nil                    {resolve shader pointer inheritance}
    then parms.shader := uparms_p^.shader
    else parms.shader := d.shader;
  if d.liparm_p = nil                  {resolve LIPARM pointer inheritance}
    then parms.liparm_p := uparms_p^.liparm_p
    else parms.liparm_p := d.liparm_p;
  if d.visprop_p = nil                 {resolve VISPROP pointer inheritance}
    then parms.visprop_p := uparms_p^.visprop_p
    else parms.visprop_p := d.visprop_p;
  n_cached := 0;                       {init checked objects cache to empty}
  next_cache := 1;                     {init where to cache next checked obj pointer}
  hit := false;                        {init to ray not hit anything in octree}
  old_mem := next_mem;                 {save MEM index before any obj save blocks}
  max_iter_level := d.max_gen + iter_below_minvox; {set max coordinate level to use}
  if max_iter_level > max_coor_level   {clip to max coordinate level allocated}
    then max_iter_level := max_coor_level;
  if v.coor[maj] >= 0                  {check sign of major axis}
    then begin                         {major axis is positive}
      icoor2.x := round(slope.x*int_scale);
      icoor2.y := round(slope.y*int_scale);
      icoor2.z := round(slope.z*int_scale);
      end
    else begin                         {major axis is negative}
      icoor2.x := -round(slope.x*int_scale);
      icoor2.y := -round(slope.y*int_scale);
      icoor2.z := -round(slope.z*int_scale);
      end
    ;
  for i := 0 to max_iter_level do begin {once for each used coordinate level}
    coor_step[i] := icoor2;            {set step sizes for this level}
    icoor2.x := icoor2.x div 2;        {make step size for next level down}
    icoor2.y := icoor2.y div 2;
    icoor2.z := icoor2.z div 2;
    end;                               {back and fill in next COOR_STEP entry}
  icoor.x := trunc(p.x*int_scale);     {init current ray point integer coordinate}
  icoor.y := trunc(p.y*int_scale);
  icoor.z := trunc(p.z*int_scale);
  vstack[0].icoor.x := 0;              {set coordinate of top level voxel}
  vstack[0].icoor.y := 0;
  vstack[0].icoor.z := 0;
  vstack[0].p := d.top_node_p;         {set pointer to top level voxel}
  vstack[0].parent_p := addr(d.top_node_p); {init parent pointer of top level voxel}
  level := 0;                          {init to we are at top level voxel}
{
*   Voxel loop.  Come back here for each new voxel to check.  ICOOR is set to
*   an integer coordinate inside the next voxel.  LEVEL is set to the level of
*   the previous voxel, and is also the index for the current voxel into VSTACK,
*   COOR_MASKS, PICK_MASKS, and COOR_STEP.
*
}
new_coor:                              {jump back here to check voxel at each new coor}
  for i := level-1 downto 1 do begin   {scan back up the stack for new parent voxel}
    if  (icoor.x & coor_masks[i] = vstack[i].icoor.x) and
        (icoor.y & coor_masks[i] = vstack[i].icoor.y) and
        (icoor.z & coor_masks[i] = vstack[i].icoor.z)
        then begin                     {new coordinate is within this voxel}
      level := i;                      {indicate new current level}
      goto got_start_level;            {try scanning down from here}
      end;
    end;                               {try next parent voxel up the stack}
  level := 0;                          {nothing matched, re-start at top voxel}
got_start_level:
{
*   LEVEL indicates the lowest valid level in the voxel stack for the new
*   coordinate.  Now scan down to find the leaf node voxel for this coordinate.
}
  vp := vstack[level].p;               {init pointer to voxel at current level}
new_voxel:                             {jump back here after subdividing curr voxel}
  while vp^.n_obj < 0 do begin         {keep looping while not leaf node voxel}
    j := pick_masks[level];            {get mask for picking child voxel index}
    if j & icoor.x <> 0                {set I to index for proper child voxel}
      then i := 1
      else i := 0;
    if j & icoor.y <> 0
      then i := i+2;
    if j & icoor.z <> 0
      then i := i+4;
    parent_p := addr(vp^.node_p[i]);   {adr of pointer to new child voxel}
    level := level + 1;                {now one level further down in voxels}
    vp := parent_p^;                   {make pointer to new current voxel}
    coor_mask := coor_masks[level];    {set coordinate mask for current level}
    vstack[level].icoor.x := icoor.x & coor_mask; {make coordinate of new voxel}
    vstack[level].icoor.y := icoor.y & coor_mask;
    vstack[level].icoor.z := icoor.z & coor_mask;
    vstack[level].p := vp;             {set pointer to new voxel}
    vstack[level].parent_p := parent_p; {save adr of pointer to this voxel}
    if vp = nil then begin             {this child node does not exist or is empty ?}
      vox_icoor := vstack[level].icoor; {set coordinate of current voxel}
      goto next_coor;                  {find coordinate in next voxel along ray}
      end;
    end;                               {back and find next child voxel down}
  vox_icoor := vstack[level].icoor;    {set coordinate of current voxel}
{
*   VP is pointing to the leaf node containing the coordinate in ICOOR.  Check whether
*   the voxel needs to be subdivided.  If so, subdivide it and then jump back to
*   NEW_VOXEL to find the voxel at the next level down.
}
leaf_node:
  if level >= d.max_gen then goto trace_voxel; {already divided max allowed levels ?}
  if level < d.min_gen then goto subdivide_voxel; {not deeper than min level ?}
  if vp^.n_obj > (d.min_miss * 8) then goto subdivide_voxel; {too many obj in voxel ?}
  if vp^.misses < d.min_miss then goto trace_voxel; {not enough rays missed here ?}
  if vp^.hits >= (vp^.misses-d.min_miss)*3 {hit ratio is good enough ?}
    then goto trace_voxel;
{
*   This voxel needs to be subdivided.
}
subdivide_voxel:
  if d.free_p = nil                    {check for available nodes on free list}
    then begin                         {no nodes currently on free list}
      if d.n_ar >= node_array_size then begin {this node array filled up ?}
        sz := sizeof(d.ar_p^);         {amount of memory to allocate}
        util_mem_grab (                {allocate a new node array}
          sz, ray_mem_p^, false, d.ar_p);
        stats_oct.mem := stats_oct.mem + sz;
        d.n_ar := 0;                   {new array starts out empty}
        end;
      d.n_ar := d.n_ar+1;              {make index for new node}
      newp := addr(d.ar_p^[d.n_ar]);   {get address of new node}
      end
    else begin                         {there are nodes currently on the free list}
      newp := d.free_p;                {get pointer to a free node}
      d.free_p := newp^.free_p;        {remove this node from free chain}
      end
    ;                                  {NEWP points to new subdivided voxel node}
  newp^.n_obj := -1;                   {indicate this is a subdivided node}

  stats_oct.n_leaf := stats_oct.n_leaf - 1; {this leaf node becomes parent voxel}
  stats_oct.n_parent := stats_oct.n_parent + 1;
{
*   NEWP points to what will be the new node for this voxel.  It has already been
*   flagged as a subdivided node.  The 8 child pointers need to be filled in.
*   The box checks need to be performed before this can be done.  Since the boxes
*   share common coordinate values, these will be precomputed into array BOX_COOR.
*   The box descriptor BOX will be all set up except for the box corner point.
*   This will be put into the BOX_COOR array separately for each box.
}
  r :=                                 {size of raw bounds box in octree space}
    pick_masks[level]*box_radius_scale;

  box.edge[1].width := r*d.size.x + d.box_err.x;
  box.edge[2].width := r*d.size.y + d.box_err.y;
  box.edge[3].width := r*d.size.z + d.box_err.z;

  box.edge[1].edge.x := box.edge[1].width; {fill in box edge vectors}
  box.edge[1].edge.y := 0.0;
  box.edge[1].edge.z := 0.0;

  box.edge[2].edge.x := 0.0;
  box.edge[2].edge.y := box.edge[2].width;
  box.edge[2].edge.z := 0.0;

  box.edge[3].edge.x := 0.0;
  box.edge[3].edge.y := 0.0;
  box.edge[3].edge.z := box.edge[3].width;

  box.edge[1].unorm.x := 1.0;          {fill in box unit normal vectors}
  box.edge[1].unorm.y := 0.0;
  box.edge[1].unorm.z := 0.0;
  box.edge[2].unorm.x := 0.0;
  box.edge[2].unorm.y := 1.0;
  box.edge[2].unorm.z := 0.0;
  box.edge[3].unorm.x := 0.0;
  box.edge[3].unorm.y := 0.0;
  box.edge[3].unorm.z := 1.0;

  box_coor[0].x :=
    (vox_icoor.x * box_radius_scale * d.size.x) - d.box_err_half.x + d.origin.x;
  box_coor[0].y :=
    (vox_icoor.y * box_radius_scale * d.size.y) - d.box_err_half.y + d.origin.y;
  box_coor[0].z :=
    (vox_icoor.z * box_radius_scale * d.size.z) - d.box_err_half.z + d.origin.z;

  box_coor[1].x := box_coor[0].x + box.edge[1].width - d.box_err.x;
  box_coor[1].y := box_coor[0].y + box.edge[2].width - d.box_err.y;
  box_coor[1].z := box_coor[0].z + box.edge[3].width - d.box_err.z;

  for i := 0 to 7 do begin             {once for each subordinate voxel to create}
    box.point.x := box_coor[i&1].x;    {fill in coordinate of box corner point}
    box.point.y := box_coor[rshft(i, 1)&1].y;
    box.point.z := box_coor[rshft(i, 2)&1].z;
    vlnp := vp;                        {init pointer to current node in voxel list}
    obj_pp := addr(vp^.obj_p[1]);      {get pointer to first object pointer}
    nleft := obj_per_leaf_node;        {init number of pointers left for this node}
    if d.free_p = nil                  {check for available nodes on free list}
      then begin                       {no nodes currently on free list}
        if d.n_ar >= node_array_size then begin {this node array filled up ?}
          sz := sizeof(d.ar_p^);       {amount of memory to allocate}
          util_mem_grab (              {allocate a new node array}
            sz, ray_mem_p^, false, d.ar_p);
          stats_oct.mem := stats_oct.mem + sz;
          d.n_ar := 0;                 {new array starts out empty}
          end;
        d.n_ar := d.n_ar+1;            {make index for new node}
        nnewp := addr(d.ar_p^[d.n_ar]); {get address of new node}
        end
      else begin                       {there are nodes currently on the free list}
        nnewp := d.free_p;             {get pointer to a free node}
        d.free_p := nnewp^.free_p;     {remove this node from free chain}
        end
      ;                                {NNEWP points to new subdivided voxel node}
    nnewp^.next_p := nil;              {init to no data nodes chained on}
    newp^.node_p[i] := nnewp;          {set pointer to this child voxel}
    n_new := 0;                        {init number of objects in child voxel}
    new_pp := addr(nnewp^.obj_p[1]);   {address of next object pointer to fill in}
    new_left := obj_per_leaf_node;     {obj pointers left to fill in this node}
    for j := 1 to vp^.n_obj do begin   {once for each object in this voxel}
      if nleft <= 0 then begin         {no more object pointers left this voxel ?}
        vlnp := vlnp^.next_p;          {point to next node in voxel data chain}
        obj_pp := addr(vlnp^.ch_p[1]); {get address of first object pointer this node}
        nleft := obj_per_data_node;    {reset number of object pointers available}
        end;                           {done switching to next data node}
      obj_pp^^.routines_p^.intersect_box^ ( {check object intersect with this voxel}
        box,                           {box descriptor for this voxel}
        obj_pp^^,                      {object to intersect box with}
        here,                          {true if ray can bump into object here}
        enclosed);                     {true if object completely encloses box}
      if here then begin               {object needs to be added to new voxel ?}
        if new_left <= 0 then begin    {no more object pointers left in this node ?}
          if d.free_p = nil            {check for available nodes on free list}
            then begin                 {no nodes currently on free list}
              if d.n_ar >= node_array_size then begin {this node array filled up ?}
                sz := sizeof(d.ar_p^); {amount of memory to allocate}
                util_mem_grab (        {allocate a new node array}
                  sz, ray_mem_p^, false, d.ar_p);
                stats_oct.mem := stats_oct.mem + sz;
                d.n_ar := 0;           {new array starts out empty}
                end;
              d.n_ar := d.n_ar+1;      {make index for new node}
              nnewp^.next_p := addr(d.ar_p^[d.n_ar]); {get address of new node}
              end
            else begin                 {there are nodes currently on the free list}
              nnewp^.next_p := d.free_p; {get pointer to a free node}
              d.free_p := nnewp^.next_p^.free_p; {remove this node from free chain}
              end
            ;                          {new node has been linked onto end of chain}
          nnewp := nnewp^.next_p;      {point NNEWP to newly allocated node}
          nnewp^.next_p := nil;        {indicate that this node is end of data chain}
          new_pp := addr(nnewp^.ch_p[1]); {make address of next obj pointer to fill in}
          new_left := obj_per_data_node; {init to all obj pointers available this node}
          end;                         {done allocating a new node on chain}
        new_pp^ := obj_pp^;            {copy obj pointer into new voxel}
        new_left := new_left-1;        {one less obj pointer left this node}
        new_pp := univ_ptr(integer32(new_pp)+sizeof(new_pp^)); {point to next slot}
        n_new := n_new+1;              {one more object in this child voxel}
        end;                           {done handling object found in child voxel}
      obj_pp :=                        {make address of next object pointer this node}
        univ_ptr(integer32(obj_pp)+sizeof(obj_pp^));
      nleft := nleft-1;                {one less obj left to read from this node}
      end;                             {back and check next object in old parent voxel}
    if n_new > 0                       {check for object found in this child voxel}
      then begin                       {the child voxel is not empty}
        newp^.node_p[i]^.n_obj := n_new; {set number of objects in child voxel}
        newp^.node_p[i]^.hits := 0;    {init to no rays hit anything here}
        newp^.node_p[i]^.misses := 0;  {init to no rays missed anything here}
        stats_oct.n_leaf := stats_oct.n_leaf + 1; {one more leaf node created}
        end
      else begin                       {the newly created child voxel is empty}
        newp^.node_p[i]^.free_p := d.free_p; {put this node onto free chain}
        d.free_p := newp^.node_p[i];
        newp^.node_p[i] := nil;        {indicate this child voxel is empty}
        end
      ;
    end;                               {back and do next child voxel}
{
*   The voxel at VP has been subdivided.  The subdivided voxel is at NEWP.  Now put
*   all the nodes of the voxel at VP on the free list, and link the new voxel into
*   the structure in place of the old voxel.
}
  vstack[level].parent_p^ := newp;     {link new voxel into octree data structure}
  vstack[level].p := newp;
  while vp <> nil do begin             {keep looping until end of data chain}
    vlnp := vp^.next_p;                {save pointer to next node in data chain}
    vp^.free_p := d.free_p;            {put node at VP onto free chain}
    d.free_p := vp;
    vp := vlnp;                        {point VP to next node in voxel}
    end;                               {back do deallocate this new node}
  vp := newp;                          {set the resulting subdivided voxel as current}
  goto new_voxel;                      {back and process this new subdivided voxel}
{
*   VP is pointing at a the leaf node voxel that the ray is supposed to be traced
*   thru.
}
trace_voxel:                           {jump here to trace ray thru voxel at VP}
  obj_pp := addr(vp^.obj_p[1]);        {init address of next object pointer}
  vlnp := vp;                          {init current node pointer}
  nleft := obj_per_leaf_node;          {init number of obj pointers left this node}
  for i := 1 to vp^.n_obj do begin     {once for each object in this voxel}
    if nleft <= 0 then begin           {no more object pointers in this node ?}
      vlnp := vlnp^.next_p;            {make pointer to next node in chain}
      obj_pp := addr(vlnp^.ch_p[1]);   {make adr of first object pointer in new node}
      nleft := obj_per_data_node;      {init number of object pointers left this node}
      end;
    for j := 1 to n_cached do begin    {check the cache of previous object checks}
      if obj_pp^ = last_checks[j] then goto next_obj; {already checked this object ?}
      end;                             {back and check next cached miss}
    checks := checks+1;                {log one more ray/object intersect check}
    if obj_pp^^.routines_p^.intersect_check^ ( {run object's intersect check routine}
        ray,                           {the ray to intersect object with}
        obj_pp^^,                      {the object to intersect ray with}
        parms,                         {run time specific parameters}
        hit_info,                      {partial results returned}
        shader)                        {returned shader}
      then begin                       {the ray did hit the object}
        ray.max_dist := hit_info.distance; {future hits must be closer than this one}
        hit := true;                   {remember that the ray hit something}
        new_mem := next_mem;           {save MEM index after this hit data}
        next_mem := old_mem;           {restore mem index to before hit data}
        hits := hits+1;                {log one more ray/object hit}
        vp^.hits := vp^.hits + 1;      {one more ray/object hit in this voxel}
        end
      else begin                       {the ray did not hit the object}
        vp^.misses := vp^.misses + 1;  {one more ray/object miss in this voxel}
        end
      ;
    last_checks[next_cache] := obj_pp^; {save pointer to object we just checked}
    next_cache := next_cache+1;        {advance where to put next checked obj pointer}
    if next_cache > cached_checks      {wrap NEXT_CACHE back to 1}
      then next_cache := 1;
    if n_cached < cached_checks        {LAST_CHECKS not full yet ?}
      then n_cached := n_cached+1;     {one more object in checked objects cache}
next_obj:                              {skip to here to test next object in voxel}
    obj_pp :=                          {make adr of next obj pointer in this node}
      univ_ptr(integer32(obj_pp)+sizeof(obj_pp^));
    nleft := nleft-1;                  {one less object pointer left in this node}
    end;                               {back and process next object in this voxel}
{
*   We are done with the current voxel pointed to by VP.  Update point ICOOR to a
*   coordinate inside the next voxel, and then jump back to NEW_COOR.  COOR_MASK
*   is set to the coordinate mask for the level of the current voxel.  END_COOR
*   is the ending coordinate of the ray within this octree.  It is only valid
*   if RAY_END is TRUE.
}
next_coor:                             {jump here to step to the next voxel}
  if ray_ends or hit then begin        {ray may end in this current voxel ?}
    p1.x := orayp.x + v.x*ray.max_dist; {make ray end point in octree space}
    p1.y := orayp.y + v.y*ray.max_dist;
    p1.z := orayp.z + v.z*ray.max_dist;
    if  (p1.x >= mic) and (p1.x <= mac) and {ray ends within the octree ?}
        (p1.y >= mic) and (p1.y <= mac) and
        (p1.z >= mic) and (p1.z <= mac) then begin
      icoor2.x := trunc(p1.x*int_scale); {integer octree ray end coordinate}
      icoor2.y := trunc(p1.y*int_scale);
      icoor2.z := trunc(p1.z*int_scale);
      if  (vox_icoor.x = icoor2.x & coor_mask) and {ray ends in the current voxel ?}
          (vox_icoor.y = icoor2.y & coor_mask) and
          (vox_icoor.z = icoor2.z & coor_mask) then begin
        goto leave;
        end;                           {done with ray ends in this voxel}
      end;                             {done with ray ends within octree}
    end;                               {done with checking ray end for termination}

  icoor.x := icoor.x + coor_step[level].x; {advance by one current voxel size}
  icoor.y := icoor.y + coor_step[level].y;
  icoor.z := icoor.z + coor_step[level].z;

  for i := level+1 to max_iter_level do begin {one iteration at each coordinate level}
    icoor2.x := icoor.x - coor_step[i].x; {make small step backwards}
    icoor2.y := icoor.y - coor_step[i].y;
    icoor2.z := icoor.z - coor_step[i].z;
    if  (icoor2.x & coor_mask <> vox_icoor.x) or
        (icoor2.y & coor_mask <> vox_icoor.y) or
        (icoor2.z & coor_mask <> vox_icoor.z)
        then begin                     {did not step back into source voxel ?}
      icoor := icoor2;                 {update current coordinate}
      end;
    end;                               {back and try a step half as large}

  if (icoor.x ! icoor.y ! icoor.z) & 16#F0000000 <> 0 then begin {outside octree ?}
    goto leave;                        {no more voxels left to do}
    end;
  goto new_coor;                       {point P is new coordinate to find voxel at}
{
*   The ray has ended, or we checked all the voxels in its path.  Now return to the
*   caller.
}
leave:
  ray.max_dist := old_max_dist;        {restore field we corrupted}
  if hit                               {all done with ray, did we hit anything ?}
    then begin                         {the ray hit something}
      next_mem := new_mem;             {set next mem after hit data for this hit}
      type1_octree_intersect_check := true; {indicate we hit something}
      return;                          {return with hit}
      end
    else begin                         {the ray hit nothing}
      type1_octree_intersect_check := false; {indicate no hit for this ray}
      return;                          {return with no hit}
      end
    ;
{
*   Jump here if the ray never intersected the outside box of the octree in the first
*   place.
}
no_hit:
  type1_octree_intersect_check := false; {indicate no hit}
  end;                                 {done with abbreviations}
  end;
{
********************************************************************************
*
*   Local subroutine TYPE1_OCTREE_INTERSECT_GEOM (HIT_INFO, FLAGS, GEOM_INFO)
*
*   Return specific geometric information about a ray/object intersection
*   in GEOM_INFO.  In HIT_INFO are any useful results left by the ray/object
*   intersection check calculation.  FLAGS identifies what specific geometric
*   information is being requested.
}
procedure type1_octree_intersect_geom ( {return detailed geometry of intersection}
  in      hit_info: ray_hit_info_t;    {data saved by INTERSECT_CHECK}
  in      flags: ray_geom_flags_t;     {bit mask list of what is being requested}
  out     geom_info: ray_geom_info_t); {filled in geometric info}
  val_param;

begin
  writeln ('Intersect geom entry point to OCTREE object called.');
  sys_bomb;
  end;
{
********************************************************************************
*
*   Local subroutine TYPE1_OCTREE_INTERSECT_BOX (BOX, OBJECT, HERE, ENCLOSED)
*
*   Find the intersection status between this object and a paralellpiped.
*   HERE is returned as TRUE if the intersect check routine for this object could
*   ever return TRUE for ray within the box volume.  ENCLOSED is returned as true
*   if the object completely encloses the box.
}
procedure type1_octree_intersect_box ( {find object/box intersection status}
  in      box:    ray_box_t;           {descriptor for a paralellpiped volume}
  in      object: ray_object_t;        {object to intersect with the box}
  out     here:   boolean;             {TRUE if ISECT_CHECK could be true in box}
  out     enclosed: boolean);          {TRUE if solid obj, and completely encloses box}
  val_param;

begin
  here := true;
  enclosed := false;
  end;
{
********************************************************************************
*
*   Local subroutine TYPE1_OCTREE_ADD_CHILD (AGGR_OBJ, OBJECT)
*
*   This call is illegal after we have already started subdividing the top node.
}
procedure type1_octree_add_child (     {Add child to this object}
  in      aggr_obj: ray_object_t;      {the object to add child to}
  var     object: ray_object_t);       {the object to add}
  val_param;

var
  data_p: oct_data_p_t;                {pointer to internal object data}
  ln_p: node_p_t;                      {pointer to initial last node in chain}
  sz: sys_int_adr_t;                   {memory size}

begin
  data_p := oct_data_p_t(aggr_obj.data_p); {make local pointer to our data}
  with                                 {set up abbreviations}
    data_p^:d
    do begin
{
*   Abbreviations:
*   D  -  Object data block
}
  if d.last_left <= 0 then begin       {no more room in current last node on chain ?}
    ln_p := addr(d.ar_p^[d.n_ar]);     {save adr of current last node in chain}
    if d.n_ar >= node_array_size then begin {no more room in current nodes array ?}
      sz := sizeof(d.ar_p^);           {amount of memory to allocate}
      util_mem_grab (                  {allocate a new nodes array}
        sz, ray_mem_p^, false, d.ar_p);
      stats_oct.mem := stats_oct.mem + sz;
      d.n_ar := 0;                     {init to no nodes allocated from new array}
      end;                             {done handling nodes array overflow}
    d.n_ar := d.n_ar+1;                {allocate one more entry in nodes array}
    ln_p^.next_p := addr(d.ar_p^[d.n_ar]); {link new entry onto end of chain}
    ln_p^.next_p^.next_p := nil;       {indicate new node is end of data chain}
    d.next_p := addr(d.ar_p^[d.n_ar].ch_p[1]); {get adr of next obj pointer to fill in}
    d.last_left := obj_per_data_node;  {init num object pointers left in new node}
    end;
  d.next_p^ := addr(object);           {put pointer to new object into data base}
  d.next_p := univ_ptr(integer32(d.next_p)+sizeof(d.next_p^)); {point to next slot}
  d.last_left := d.last_left-1;        {one less object pointer left in curr node}
  d.top_node_p^.n_obj := d.top_node_p^.n_obj+1; {one more object in the data base}
  end;                                 {done with D abbreviation}
  end;
{
********************************************************************************
*
*   Subroutine TYPE1_OCTREE_GEOM (ORIGIN, SIZE, OBJECT)
*
*   This subroutine is an externally visible kluge to reset the octree outer
*   geometry.  It is used by RENDlib because it doesn't know the bounding box
*   around the data until after all the ADD_CHILD operations.
}
procedure type1_octree_geom (          {back door to stomp on octree geometry}
  in      origin: vect_3d_t;           {most negative corner for all 3 axies}
  in      size: vect_3d_t;             {outer octree dimension for each axis}
  in      object: ray_object_t);       {handle to specific octree object}
  val_param;

var
  data_p: oct_data_p_t;                {pointer to data for this octree}
  step_level: sys_int_machine_t;       {voxel level of iteration step size}
  box_err: real;                       {size of voxel intersect box size grow}

begin
  data_p := oct_data_p_t(object.data_p); {get pointer to data for this octree}

  data_p^.origin := origin;            {save new octree geometry info}
  data_p^.size := size;
{
*   The caller's data has been saved.  Some internal fields need to be
*   re-computed based on the octree geometry.
}
  data_p^.recip_size.x := 1.0/data_p^.size.x; {make reciprocal sizes}
  data_p^.recip_size.y := 1.0/data_p^.size.y;
  data_p^.recip_size.z := 1.0/data_p^.size.z;
  step_level :=                        {voxel level of iteration steps}
    min(max_coor_level, data_p^.max_gen + iter_below_minvox);
  box_err :=                           {intersect box size increment in octree space}
    1.0 / lshft(1, step_level);
  data_p^.box_err.x := box_err * data_p^.size.x;
  data_p^.box_err.y := box_err * data_p^.size.y;
  data_p^.box_err.z := box_err * data_p^.size.z;
  data_p^.box_err_half.x := data_p^.box_err.x / 2.0;
  data_p^.box_err_half.y := data_p^.box_err.y / 2.0;
  data_p^.box_err_half.z := data_p^.box_err.z / 2.0;
  end;
{
****************************************************************************
*
*   Subroutine TYPE1_OCTREE_ROUTINES_MAKE (POINTERS)
*
*   Fill in the routines block for this class of objects.
}
procedure type1_octree_routines_make ( {fill in object routines block}
  out     pointers: ray_object_routines_t); {block to fill in}
  val_param;

begin
  pointers.create := addr(type1_octree_create);
  pointers.version := addr(type1_octree_version);
  pointers.intersect_check := addr(type1_octree_intersect_check);
  pointers.intersect_geom := addr(type1_octree_intersect_geom);
  pointers.intersect_box := addr(type1_octree_intersect_box);
  pointers.add_child := addr(type1_octree_add_child);
  end;
