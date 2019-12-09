{   WARNING:  This code is old, and some data structures have changed out from
*   under it.  The source code is here to preserve it, but it is currently not
*   being built.
}

{   Type 1 OCTREE_DATA aggregate object.  The octree is rectangular and axis
*   aligned.
*
*   The octree stucture itself is displayed instead of the objects referenced by
*   the octree.  The data structure must have been previously set by object
*   TYPE1_OCTREE.
}
module type1_octree_data;
define type1_octree_data_routines_make;
%include 'ray_type1_2.ins.pas';

const
  obj_per_leaf_node = 5;               {number of object pointers in leaf node object}
  obj_per_data_node = 8;               {num of object pointers in chained data node}
  node_array_size = 30000;             {approximately 1 Mb worth of node descriptors}

type
  vect3_t = record case integer of     {3D vector with array overlay}
    1:(                                {separate named fields}
      x: real;
      y: real;
      z: real);
    2:(                                {overlay array for arithmetic indexing}
      coor: array[1..3] of real);
    end;

  node_p_t =                           {pointer to an octree node}
    ^node_t;

  node_t = record case integer of      {template for one octree node}
    1:(                                {the node has been subdivided}
      unused1: sys_int_machine_t;      {set to -1, overlay onto N_OBJ below}
      node_p:                          {pntrs to child nodes, index msb is Z, lsb is X}
        array[0..7] of node_p_t);      {NIL means child node is empty}
    2:(                                {the node is a leaf node}
      n_obj: sys_int_machine_t;        {total number of objects at this voxel}
      hits: sys_int_machine_t;         {num of rays thru this voxel that hit something}
      misses: sys_int_machine_t;       {num of rays thru this voxel that hit nothing}
      obj_p:                           {pointers to objects at this voxel}
        array[1..obj_per_leaf_node] of ray_object_p_t;
      next_p: node_p_t);               {pointer to first chained data node}
    3:(                                {the node is a chained data node}
      ch_p:                            {more pointers to objects at this voxel}
        array[1..obj_per_data_node] of ray_object_p_t;
      unused2: node_p_t);              {use NEXT_P field above}
    4:(                                {unused node, on the free nodes chain}
      free_p: node_p_t);               {pointer to next node on free chain}
    end;

  node_array_t =                       {approximately 1Mb of node descriptors}
    array[1..node_array_size] of node_t;

  node_array_p_t =                     {pointer to a node array block}
    ^node_array_t;

  oct_obj_data_t = record              {OCTREE object's data record}
    shader: ray_shader_t;              {pointer to shader entry point}
    liparm_p: type1_liparm_p_t;        {pointer to light source parameters block}
    visprop_p: type1_visprop_p_t;      {pointer to visual properties block}
    min_gen: sys_int_machine_t;        {minimum subdivision level for non-empty voxels}
    max_gen: sys_int_machine_t;        {max allowed voxel subdivision level 0 = none}
    min_miss: sys_int_machine_t;       {min ray misses before subdividing}
    origin: vect_3d_t;                 {most negative corner point for all 3 axis}
    size: vect_3d_t;                   {outside octree size in each dimension}
    recip_size: vect_3d_t;             {reciprocal size for each dimension}
    box_err: vect_3d_t;                {amount to grow size of voxel intersect box}
    box_err_half: vect_3d_t;           {amount to displace intersect box corner}
    top_node_p: node_p_t;              {pointer to top level node}
    ar_p: node_array_p_t;              {pointer to current nodes array}
    n_ar: sys_int_machine_t;           {number of nodes used from current array so far}
    free_p: node_p_t;                  {pointer to start of free chain}
    next_p: ^ray_object_p_t;           {pointer to next obj pointer to fill in}
    last_left: sys_int_machine_t;      {num of obj pointers left in last node of
                                        chain.  Only valid before any subdivision}
    end;

  oct_obj_data_p_t =                   {pointer to OCTREE object data block}
    ^oct_obj_data_t;

  object_data_t = record               {data record pointer to by object block}
    shader: ray_shader_t;              {pointer to shader entry point}
    liparm_p: type1_liparm_p_t;        {pointer to light source parameters block}
    visprop_p: type1_visprop_p_t;      {pointer to visual properties block}
    oct_data_p: oct_obj_data_p_t;      {pointer to OCTREE object's data}
    box_size: real;                    {0 to 1 relative displayed voxel size}
    box_gap: real;                     {gap from displayed box to voxel edge}
    show_objects: boolean;             {objects in octree also displayed if TRUE}
    end;

  object_data_p_t =                    {pointer to object data block}
    ^object_data_t;

  object_hit_info_t = record           {saved data from intersection check}
    base: type1_hit_info_t;            {mandatory part of hit geometry save area}
    point: vect_3d_t;                  {intersection point}
    unorm: vect_3d_t;                  {unit normal at intersect point}
    end;

  object_hit_geom_p_t =                {pointer to hit geometry block}
    ^object_hit_info_t;

procedure type1_octree_data_create (   {create new primitive with custom data}
  out     object: ray_object_t;        {newly filled in object block}
  in      data: type1_octree_data_user_data_t; {parameters from user}
  out     stat: sys_err_t);            {completion status code}
  val_param; forward;

function type1_octree_data_intersect_check ( {check for ray/object intersection}
  in out  ray: type1_ray_t;            {ray descriptor}
  in      object: ray_object_t;        {object to intersect ray with}
  in      uparms: type1_object_parms_t; {run-time parameters passed from above}
  out     hit_info: ray_hit_info_t;    {all returned information}
  out     shader: ray_shader_t)        {pointer to shader to resolve color}
  :boolean;                            {TRUE if ray does hit object}
  val_param; forward;

procedure type1_octree_data_intersect_geom ( {return detailed geometry of intersection}
  in      hit_info: ray_hit_info_t;    {data saved by INTERSECT_CHECK}
  in      flags: ray_geom_flags_t;     {bit mask octree of what is being requested}
  out     geom_info: ray_geom_info_t); {filled in geometric info}
  val_param; forward;

procedure type1_octree_data_version (  {return version information about object}
  out     version: ray_object_version_t); {returned version information}
  val_param; forward;
{
****************************************************************************
*
*   Subroutine TYPE1_OCTREE_ROUTINES_MAKE (POINTERS, SIZE)
*
*   Fill in the routines block for this class of objects.  SIZE is the size in bytes
*   of the data structure to be filled in.
}
procedure type1_octree_data_routines_make ( {fill in object routines block}
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
    p := univ_ptr(integer32(p)+sizeof(p^)); {point to next slot in POINTERS}
    end;
  max_ofs := (ents - 1)*4;             {byte offset of last entry}

  if (sys_int_adr_t(addr(pointers.version))-sys_int_adr_t(addr(pointers)))
    <= max_ofs                         {this slot within POINTERS ?}
    then pointers.version :=
      ray_object_version_proc_t(addr(type1_octree_data_version));
    ;
  if (sys_int_adr_t(addr(pointers.create))-sys_int_adr_t(addr(pointers)))
    <= max_ofs                         {this slot within POINTERS ?}
    then pointers.create :=
      ray_object_create_proc_t(addr(type1_octree_data_create));
    ;
  if (sys_int_adr_t(addr(pointers.intersect_check))-sys_int_adr_t(addr(pointers)))
    <= max_ofs                         {this slot within POINTERS ?}
    then pointers.intersect_check :=
      ray_object_isect_check_proc_t(addr(type1_octree_data_intersect_check));
    ;
  if (sys_int_adr_t(addr(pointers.intersect_geom))-sys_int_adr_t(addr(pointers)))
    <= max_ofs                         {this slot within POINTERS ?}
    then pointers.intersect_geom :=
      ray_object_isect_geom_proc_t(addr(type1_octree_data_intersect_geom));
    ;
  end;
{
****************************************************************************
*
*   Local subroutine TYPE1_OCTREE_CREATE (OBJECT, DATA, STATUS)
*
*   Fill in the new object in OBJECT.  DATA is the user data parameters for
*   this object.  STATUS is the standard system error return code.
}
procedure type1_octree_data_create (   {create new primitive with custom data}
  out     object: ray_object_t;        {newly filled in object block}
  in      data: type1_octree_data_user_data_t; {parameters from user}
  out     stat: sys_err_t);            {completion status code}
  val_param;

var
  data_p: object_data_p_t;             {pointer to internal object data}

begin
  sys_error_none (stat);               {init to no error}

  util_mem_grab (                      {create new data block for this object}
    sizeof(data_p^), ray_mem_p^, false, data_p);
  data_p^.shader := data.shader;       {copy pointer to shader to use}
  data_p^.liparm_p := data.liparm_p;   {copy pointer to light source block}
  data_p^.visprop_p := data.visprop_p; {copy pointer to visual properties block}
  data_p^.oct_data_p :=                {get pointer to OCTREE object's data block}
    oct_obj_data_p_t(data.oct_obj_p^.data_p);
  data_p^.box_size := data.box_size;   {copy relative size of displayed voxel}
  data_p^.box_gap := (1.0-data.box_size)/2.0; {make voxel edge to box size gap}
  data_p^.show_objects := data.show_objects; {copy show objects in octree flag}
  object.data_p :=                     {set data pointer in object block}
    ray_obj_data_p_t(data_p);
  end;
{
****************************************************************************
*
*   Local subroutine TYPE1_OCTREE_VERSION (VERSION)
*
*   Return version information obout this class of objects.
}
procedure type1_octree_data_version (  {return version information about object}
  out     version: ray_object_version_t); {returned version information}
  val_param;

begin
  version.year := 1987;
  version.month := 12;
  version.day := 27;
  version.hour := 14;
  version.minute := 45;
  version.second := 0;
  version.version := 0;
  version.name := string_v('OCTREE_DATA');
  version.aggregate := false;
  end;
{
***************************************************************************************
*
*   Local function TYPE1_OCTREE_INTERSECT_CHECK (RAY, OBJECT, UPARMS, HIT_INFO, SHADER)
*
*   Check ray and object for an intersection.  If so, return TRUE, and save
*   any partial results in HIT_INFO.  These partial results may be used later
*   to get detailed information about the intersection geometry.
}
function type1_octree_data_intersect_check ( {check for ray/object intersection}
  in out  ray: type1_ray_t;            {ray descriptor}
  in      object: ray_object_t;        {object to intersect ray with}
  in      uparms: type1_object_parms_t; {run-time parameters passed from above}
  out     hit_info: ray_hit_info_t;    {all returned information}
  out     shader: ray_shader_t)        {pointer to shader to resolve color}
  :boolean;                            {TRUE if ray does hit object}
  val_param;

const
  min_voxel_size = 4.768372E-7;        {dimension of minimum LSB sized voxel}
  mic = 4.76837E-7;                    {min representable coor in 0.0 to 1.0 space}
  mac = 1.0 - mic;                     {max representable coor in 0.0 to 1.0 space}
  cached_checks = 4;                   {previously checked objects list max size}

type
  icoor_t = record case integer of     {integer XYZ coordinate with array overlay}
    1:(                                {individually named fields}
      x, y, z: sys_int_machine_t);
    2:(                                {overlay array of algorithmic indexing}
      coor: array[1..3] of sys_int_machine_t);
    end;

var
  data_p: object_data_p_t;             {pointer to object's specific data area}
  p: vect3_t;                          {ray point in 0 to 1 space}
  v: vect3_t;                          {non-uint ray vector in 0 to 1 space}
  rv: vect3_t;                         {reciprocal of V, =1E20 if V=0}
  maj, min1, min2: integer32;          {1-3 subscripts for major and minor axies}
  slope: vect3_t;                      {slope of each axis with respect to major axis}
  rslope: vect3_t;                     {reciprocal of SLOPE, above}
  max_odist: real;                     {max octree space ray distance to hit point}
  dist_m, dist_b: real;                {for equation RAY_DIST = OCT_DIST*DIST_M+DIST_B}
  dmaj: real;                          {major axis delta}
  dmin: real;                          {minor axis delta}
  p1, p2: vect3_t;                     {scratch points}
  level: integer32;                    {nesting level of current node, top = 0}
  bit_mask: integer32;                 {mask for picking off decision bit at this lev}
  coor_mask: integer32;                {masks in relevant adr bits for this level}
  icoor: icoor_t;                      {32 bit integer XYZ coordinate}
  vp: node_p_t;                        {pointer to current voxel}
  i, j: integer32;                     {loop counters and scratch integers}
  box: ray_box_t;                      {paralellpiped for voxel intersection checks}
  box_coor: array[0..1] of vect_3d_t;  {possible box corner points}
  newp: node_p_t;                      {pointer to new subdivided voxel}
  obj_pp: ^ray_object_p_t;             {adr of next obj pnt in scanning voxel list}
  vlnp: node_p_t;                      {pointer to curr node in scanning voxel list}
  nleft: integer32;                    {obj pointers left this node in scanning list}
  new_left: integer32;                 {num obj pointers left in node being built}
  n_new: integer32;                    {total number of objects in voxel being built}
  new_pp: ^ray_object_p_t;             {adr of next obj pointer in node being built}
  nnewp: node_p_t;                     {pointer to node being built}
  here: boolean;                       {flag indicating ray can hit object in box}
  enclosed: boolean;                   {flag indicating object completely encloses box}
  hit: boolean;                        {the ray hit an object}
  parms: type1_object_parms_t;         {parameters to pass to subordinate objects}
  old_mem: integer32;                  {MEM index before any object hits}
  new_mem: integer32;                  {MEM index after last object hit}
  sz: real;                            {used for size of current voxel}
  last_checks:                         {cached object pointers of recent misses}
    array[1..cached_checks] of ray_object_p_t;
  n_cached: integer32;                 {number of object pointers in LAST_CHECKS array}
  next_cache: integer32;               {LAST_checks index of where to put next obj pnt}
  visprop_p: type1_visprop_p_t;        {pointer to local customized visprop block}

  dist: real;                          {scratch distance}
  d1, d2, d3, d4: real;                {ray distances to intersect points}
  hit_axis: integer32;                 {axis perpendicular to box plane that got hit}
  hit_geom_p: object_hit_geom_p_t;     {pointer to hit geom block in MEM array}

label
  got_p, new_coor, new_voxel, leaf_node, subdivide_voxel, trace_voxel, next_obj,
  next_coor, no_hit, not_hit_box, skip_objects, leave;

begin
  data_p := object_data_p_t(object.data_p); {make pointer to object's data area}
  with                                 {set up abbreviations}
    data_p^:dd,
    data_p^.oct_data_p^:d
    do begin
{
*   Abbreviations:
*   DD  -  Specific data block for this object
*   D  -  Specidic data block for OCTREE object
*
*   Now transform the ray into the (0,0,0) to (1,1,1) octree space.
}
  p.x := (ray.point.x + ray.vect.x*ray.min_dist - d.origin.x)*d.recip_size.x;
  p.y := (ray.point.y + ray.vect.y*ray.min_dist - d.origin.y)*d.recip_size.y;
  p.z := (ray.point.z + ray.vect.z*ray.min_dist - d.origin.z)*d.recip_size.z;
  v.x := ray.vect.x*d.recip_size.x;    {transform ray vector to our space (not unity)}
  v.y := ray.vect.y*d.recip_size.y;
  v.z := ray.vect.z*d.recip_size.z;
  dist_m := 1.0/sqrt(                  {make octree to ray space ray length factor}
    sqr(v.x) + sqr(v.y) + sqr(v.z));
  v.x := v.x*dist_m;                   {make V a unit vector}
  v.y := v.y*dist_m;
  v.z := v.z*dist_m;
  if abs(v.x) > 1.0E-10                {make reciprocal of V used for voxel stepping}
    then rv.x := 1.0/v.x
    else rv.x := 1.0E20;
  if abs(v.y) > 1.0E-10
    then rv.y := 1.0/v.y
    else rv.y := 1.0E20;
  if abs(v.z) > 1.0E-10
    then rv.z := 1.0/v.z
    else rv.z := 1.0E20;
{
*   The ray point and ray vector have been transformed into a space in which the
*   octree occupies (0,0,0) to (1,1,1).  The transformed ray point is in P, and the
*   transformed ray vector is in V.  V is a unit vector.  This transformed space
*   is only used to decide which voxels the ray hits.  The original ray is passed
*   directly to the subordinate object intersection routines.
*
*   Start by determining which is the major axis and the two minor axies.
}
  if abs(v.x) > abs(v.y)               {sort based on X and Y alone}
    then begin
      maj := 1;
      min2 := 2;
      end
    else begin
      maj := 2;
      min2 := 1;
      end
    ;
  if abs(v.z) > abs(v.coor[maj])       {sort biggest against Z}
    then maj := 3;
  if abs(v.z) < abs(v.coor[min2])      {sort smallest against Z}
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
    dist_b := 0.0;                     {ray space distance from ray point to point P}
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
  dist_b := sqrt(                      {ray space distance from ray point to point P}
    sqr(p.x*d.size.x + d.origin.x - ray.point.x) +
    sqr(p.y*d.size.y + d.origin.y - ray.point.y) +
    sqr(p.z*d.size.z + d.origin.z - ray.point.z) );
{
*   P is the first point along the ray that is also inside the octree.  P is in the
*   octree (0,0,0) to (1,1,1) space.
}
got_p:                                 {jump here if P started out inside octree}
  if abs(slope.coor[min1]) > mic       {check for slope useable or not}
    then begin                         {slope is big enough to be useable}
      rslope.coor[min1] := 1.0/slope.coor[min1];
      end
    else begin                         {slope is too small to use inverse}
      slope.coor[min1] := 0.0;         {make it zero so it doesn't cause trouble}
      end
    ;                                  {done making MIN1 reciprocal slope}
  if abs(slope.coor[min2]) > mic       {check for slope useable or not}
    then begin                         {slope is big enough to be useable}
      rslope.coor[min2] := 1.0/slope.coor[min2];
      end
    else begin                         {slope is too small to use inverse}
      slope.coor[min2] := 0.0;         {make it zero so it doesn't cause trouble}
      end
    ;                                  {done making MIN2 reciprocal slope}
  p1 := p;                             {save ray starting point inside octree}
  if dd.shader = nil                   {resolve shader pointer inheritance}
    then parms.shader := uparms.shader
    else parms.shader := dd.shader;
  if dd.liparm_p = nil                 {resolve LIPARM pointer inheritance}
    then parms.liparm_p := uparms.liparm_p
    else parms.liparm_p := dd.liparm_p;
  if dd.visprop_p = nil                {resolve VISPROP pointer inheritance}
    then parms.visprop_p := uparms.visprop_p
    else parms.visprop_p := dd.visprop_p;
  n_cached := 0;                       {init checked objects cache to empty}
  next_cache := 1;                     {init where to cache next checked obj pointer}
  hit := false;                        {init to ray not hit anything in octree}
  old_mem := next_mem;                 {save MEM index before any obj save blocks}
{
*   Point P1 has been set to the first point along the ray inside the octree.  This
*   will be used later to recompute subsequent points along the ray.
*
*   Point P contains the floating point coordinates at which to check for objects
*   to intersect the ray with.  First convert these to integer coordinates that can
*   be used to index down the octree to the specific leaf node voxel that contains
*   this point.  P will be converted to 32 bit integer coordinates, where the octree
*   0 to 1 space will map to 0 to 32767.  Therefore, if the high 16 bits of the
*   32 bit coordinate are non-zero, then this coordinate is outside the octree.
*   If the coordinate is outside the octree, then all possible voxels have already
*   been checked, and the ray has hit nothing.
}
  icoor.x := trunc(p.x*32768.0);       {make integer X coor}
  icoor.y := trunc(p.y*32768.0);       {make integer Y coor}
  icoor.z := trunc(p.z*32768.0);       {make integer Z coor}

new_coor:                              {jump back here to check voxel at each new coor}
  vp := d.top_node_p;                  {init current node to top level node}
  bit_mask := 16#8000;                 {init bit mask to highest address bit}
  coor_mask := 16#FFFF8000;            {init mask to no significant address bits}
  level := 0;                          {init to at top level node}

new_voxel:                             {jump here to process the voxel at adr VP}
  if vp^.n_obj >= 0 then goto leaf_node; {this voxel is a leaf node ?}
  bit_mask := rshft(bit_mask, 1);      {position mask to pick off proper decision bits}
  if bit_mask & icoor.x <> 0           {set I to index for proper child voxel}
    then i := 1
    else i := 0;
  if bit_mask & icoor.y <> 0
    then i := i+2;
  if bit_mask & icoor.z <> 0
    then i := i+4;
  vp := vp^.node_p[i];                 {get pointer to child voxel containing coor}
  coor_mask := rshft(coor_mask, 1);    {update mask of significant coordinate bits}
  level := level+1;                    {indicate we are one more level down the tree}
  if vp = nil then goto next_coor;     {new nested node does not exist ?}
  goto new_voxel;                      {back and process this new voxel}
{
*   VP is pointing to the leaf node containing the coordinate in ICOOR.  Check whether
*   the voxel needs to be subdivided.  If so, subdivide it and then jump back to
*   NEW_VOXEL to find the voxel at the next level down.
}
leaf_node:
  if level >= d.max_gen then goto trace_voxel; {already divided max allowed levels ?}
  if level < d.min_gen then goto subdivide_voxel; {not deeper than min level ?}
  if vp^.misses < d.min_miss then goto trace_voxel; {not enough rays missed here ?}
  if vp^.hits > vp^.misses then goto trace_voxel; {more than half the rays hit here ?}
{
*   This voxel needs to be subdivided.
}
subdivide_voxel:
{
*   VP is pointing at a the leaf node voxel that the ray is supposed to be traced
*   thru.
}
trace_voxel:                           {jump here to trace ray thru voxel at VP}
  sz := bit_mask*3.051758E-5;          {make full width of this voxel in octree space}
  box.edge[1].width := dd.box_size*sz*d.size.x; {make length of box sides}
  box.edge[2].width := dd.box_size*sz*d.size.y;
  box.edge[3].width := dd.box_size*sz*d.size.z;
  box.point.x := ((coor_mask & icoor.x)*3.051758E-5 + sz*dd.box_gap)*d.size.x
    + d.origin.x;
  box.point.y := ((coor_mask & icoor.y)*3.051758E-5 + sz*dd.box_gap)*d.size.y
    + d.origin.y;
  box.point.z := ((coor_mask & icoor.z)*3.051758E-5 + sz*dd.box_gap)*d.size.z
    + d.origin.z;
  p2.x := box.point.x - ray.point.x;   {make vector from box corner to ray}
  p2.y := box.point.y - ray.point.y;
  p2.z := box.point.z - ray.point.z;
  dist := p2.coor[maj]/ray.vect.coor[maj]; {MAJ ray distance to box corner MAJ plane}
  if ray.vect.coor[maj] >= 0.0         {check ray direction along MAJ axis}
    then begin                         {ray is heading towards positive MAJ}
      d1 := dist;                      {ray distance to first MAJ plane}
      d2 := dist + box.edge[maj].width/ray.vect.coor[maj]; {ray dist to 2nd MAJ plane}
      end
    else begin                         {ray is heading towards negative MAJ}
      d1 := dist + box.edge[maj].width/ray.vect.coor[maj]; {ray dist to 1st MAJ plane}
      d2 := dist;                      {ray distance to second MAJ plane}
      end
    ;
  hit_axis := maj;                     {D1 currenlty represents hit with MAJ axis}
{
*   D1 and D2 are now the distance along the ray to the two planes at the box faces
*   perpendicular to the MAJ axis.  Now succesively clip this line segment to the
*   MIN1 and MIN2 slices.  If anything is left over, then the hit point is at ray
*   distance D1.  HIT_AXIS is kept current to indicate which axis plane clip D1
*   represents.
}
  if abs(ray.vect.coor[min1]) > 1E-6 then begin {ray not paralell to this axis ?}
    if ray.vect.coor[min1] >= 0.0      {check ray direction along MIN1 axis}
      then begin                       {ray is heading in positive MIN1 direction}
        d3 := p2.coor[min1]/ray.vect.coor[min1];
        d4 := d3 + box.edge[min1].width/ray.vect.coor[min1];
        end
      else begin                       {ray is heading in negative MIN1 direction}
        d4 := p2.coor[min1]/ray.vect.coor[min1];
        d3 := d4 + box.edge[min1].width/ray.vect.coor[min1];
        end
      ;
    if d3 > d1 then begin
      d1 := d3;
      hit_axis := min1;
      end;
    if d4 < d2 then d2 := d4;
    end;                               {done with ray not paralell to MIN1 plane}
  if abs(ray.vect.coor[min2]) > 1E-6 then begin {ray not paralell to this axis ?}
    if ray.vect.coor[min2] >= 0.0      {check ray direction along MIN2 axis}
      then begin                       {ray is heading in positive MIN2 direction}
        d3 := p2.coor[min2]/ray.vect.coor[min2];
        d4 := d3 + box.edge[min2].width/ray.vect.coor[min2];
        end
      else begin                       {ray is heading in negative MIN2 direction}
        d4 := p2.coor[min2]/ray.vect.coor[min2];
        d3 := d4 + box.edge[min2].width/ray.vect.coor[min2];
        end
      ;
    if d3 > d1 then begin
      d1 := d3;
      hit_axis := min2;
      end;
    if d4 < d2 then d2 := d4;
    end;                               {done with ray not paralell to MIN2 plane}
  if d1 > d2 then goto not_hit_box;    {ray does not hit the voxel box at all ?}
  if (d1 < ray.min_dist) or (d1 > ray.max_dist)
    then goto not_hit_box;             {hit point not inside legal interval}
{
*   The ray has hit the box.
}
  hit := true;                         {indicate that this ray hit something}
  ray.max_dist := d1;                  {only closer hits are allowed from now on}
  hit_info.object_p := addr(object);   {return handle to object that got hit}
  hit_info.distance := d1;             {fill in ray distance to hit point}
  hit_info.enter := true;
  hit_geom_p := addr(mem[next_mem]);   {get pointer to hit geom block}
  next_mem := ((sizeof(object_hit_info_t)+3) & 16#0FFFFFFFC)
    + next_mem;                        {allocate 4 byte chunks for HIT_GEOM block}
  visprop_p := addr(mem[next_mem]);    {allocate space for customized visprop block}
  next_mem := ((sizeof(type1_visprop_t)+3) & 16#0FFFFFFFC)
    + next_mem;                        {allocate 4 byte chunks for HIT_GEOM block}
  if next_mem > mem_block_size then begin {not enough room for HIT_GEOM block ?}
    writeln ('Insufficient space in array MEM (RAY_TYPE1_2.INS.PAS).');
    sys_bomb;                          {save traceback info and abort}
    end;
  new_mem := next_mem;                 {save NEXT_MEM after this hit allocation}
  next_mem := old_mem;                 {restore MEM allocation to before this hit data}
  hit_info.shader_parms_p :=           {fill in pointer to hit geometry save area}
    ray_shader_parms_p_t(hit_geom_p);
  hit_geom_p^.base.liparm_p := parms.liparm_p;
  hit_geom_p^.base.visprop_p := visprop_p;
  hit_geom_p^.point.x := ray.point.x + ray.vect.x*d1;
  hit_geom_p^.point.y := ray.point.y + ray.vect.y*d1;
  hit_geom_p^.point.z := ray.point.z + ray.vect.z*d1;
  hit_geom_p^.unorm.x := 0.0;          {init hit point normal vector}
  hit_geom_p^.unorm.y := 0.0;
  hit_geom_p^.unorm.z := 0.0;
  if ray.vect.coor[hit_axis] > 0.0
    then hit_geom_p^.unorm.coor[hit_axis] := -1.0
    else hit_geom_p^.unorm.coor[hit_axis] := 1.0;
  shader := parms.shader;
{
*   Fudge the visprop block to color code parameters at this voxel.
}
  visprop_p^ := parms.visprop_p^;      {make local copy of visprop block}
  sz := vp^.hits + vp^.misses;         {total number of rays thru here}
  if sz > 0.0                          {check for enough rays to form ratio}
    then begin                         {there was at least one ray thru this voxel}
      if vp^.hits = 0                  {check for any hits at all}
        then begin                     {none of the rays thru here hit anything}
          visprop_p^.diff_col.red := 1.0;
          visprop_p^.diff_col.grn := 1.0;
          visprop_p^.diff_col.blu := 0.0;
          end
        else begin
          visprop_p^.diff_col.red :=
            vp^.misses / sz;
          visprop_p^.diff_col.grn :=
            vp^.hits / sz;
          visprop_p^.diff_col.blu := 0.5;
          end
        ;
      end
    else begin                         {there were no rays thru here}
      visprop_p^.diff_col.red := 0.2;
      visprop_p^.diff_col.grn := 0.2;
      visprop_p^.diff_col.blu := 1.0;
      end
    ;
{
*   HIT is set to true if the ray hit the displayable voxel box.
}
not_hit_box:                           {jump here if ray not hit this voxel box}
  if not dd.show_objects then goto skip_objects;
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
        hit := true;                   {remember that the ray hit something}
        new_mem := next_mem;           {save MEM index after this hit data}
        next_mem := old_mem;           {restore mem index to before hit data}
        hits := hits+1;                {log one more ray/object hit}
        end
      else begin                       {the ray did not hit the object}
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
skip_objects:                          {jump to here to skip checking objects in voxel}
{
*   We are done with the current voxel pointed to by VP.  Update point P to a
*   coordinate inside the next voxel, and then jump back to NEW_COOR.  P1 is the first
*   ray point inside the octree.
}
next_coor:                             {jump here to step to the next voxel}
  if v.coor[maj] >= 0.0
    then begin                         {major axis is heading in positive direction}
      max_odist :=                     {make octree ray dist to voxel MAJ limit}
        ( ((icoor.coor[maj] & coor_mask)+bit_mask)*3.051758E-5
        - p1.coor[maj] + mic)*rv.coor[maj];
      end
    else begin                         {major axis is heading in negative direction}
      max_odist :=                     {make octree ray dist to voxel MAJ limit}
        ( (icoor.coor[maj] & coor_mask)*3.051758E-5
        - p1.coor[maj] - mic)*rv.coor[maj];
      end
    ;
  if v.coor[min1] >= 0.0
    then begin                         {ray is heading in postive MIN1 direction}
      dmin :=                          {make octree ray dist to voxel MIN1 limit}
        ( ((icoor.coor[min1] & coor_mask)+bit_mask)*3.051758E-5
        - p1.coor[min1] + mic)*rv.coor[min1];
      end
    else begin                         {ray is heading in negative MIN1 direction}
      dmin :=                          {make octree ray dist to voxel MIN1 limit}
        ( (icoor.coor[min1] & coor_mask)*3.051758E-5
        - p1.coor[min1] - mic)*rv.coor[min1];
      end
    ;
  if dmin < max_odist then max_odist := dmin; {clip max dist in voxel to shortest}
  if v.coor[min2] >= 0.0
    then begin                         {ray is heading in postive MIN2 direction}
      dmin :=                          {make octree ray dist to voxel MIN2 limit}
        ( ((icoor.coor[min2] & coor_mask)+bit_mask)*3.051758E-5
        - p1.coor[min2] + mic)*rv.coor[min2];
      end
    else begin                         {ray is heading in negative MIN2 direction}
      dmin :=                          {make octree ray dist to voxel MIN2 limit}
        ( (icoor.coor[min2] & coor_mask)*3.051758E-5
        - p1.coor[min2] - mic)*rv.coor[min2];
      end
    ;
  if dmin < max_odist then max_odist := dmin; {clip max dist in voxel to shortest}
  if (max_odist*dist_m+dist_b) >= ray.max_dist then begin {ray ends in this voxel ?}
    goto leave;
    end;                               {done with ray ending in this voxel}
  icoor.x := round((p1.x + v.x*max_odist)*32768.0-0.5);
  icoor.y := round((p1.y + v.y*max_odist)*32768.0-0.5);
  icoor.z := round((p1.z + v.z*max_odist)*32768.0-0.5);
  if (icoor.x ! icoor.y ! icoor.z) & 16#FFFF8000 <> 0 then begin {outside octree ?}
    goto leave;
    end;
  goto new_coor;                       {point P is new coordinate to find voxel at}
{
*   The ray has ended, or we checked all the voxels in its path.  Now return to the
*   caller.
}
leave:
  if hit                               {all done with ray, did we hit anything ?}
    then begin                         {the ray hit something}
      next_mem := new_mem;             {set next mem after hit data for this hit}
      type1_octree_data_intersect_check := true; {indicate we hit something}
      return;                          {return with hit}
      end
    else begin                         {the ray hit nothing}
      type1_octree_data_intersect_check := false; {indicate no hit for this ray}
      return;                          {return with no hit}
      end
    ;
{
*   Jump here if the ray never intersected the outside box of the octree in the first
*   place.
}
no_hit:
  type1_octree_data_intersect_check := false; {indicate no hit}
  end;                                 {done with abbreviations}
  end;
{
****************************************************************************
*
*   Local subroutine TYPE1_OCTREE_INTERSECT_GEOM (HIT_INFO, FLAGS, GEOM_INFO)
*
*   Return specific geometric information about a ray/object intersection
*   in GEOM_INFO.  In HIT_INFO are any useful results left by the ray/object
*   intersection check calculation.  FLAGS identifies what specific geometric
*   information is being requested.
}
procedure type1_octree_data_intersect_geom ( {return detailed geometry of intersection}
  in      hit_info: ray_hit_info_t;    {data saved by INTERSECT_CHECK}
  in      flags: ray_geom_flags_t;     {bit mask octree of what is being requested}
  out     geom_info: ray_geom_info_t); {filled in geometric info}
  val_param;

var
  hit_geom_p: object_hit_geom_p_t;     {pointer to hit geometry block}

begin
  geom_info.flags := [];               {init what info we returned indicator}
  hit_geom_p :=                        {pointer to HIT_GEOM block}
    object_hit_geom_p_t(hit_info.shader_parms_p);
  with                                 {define abbreviations}
    hit_geom_p^:hit_geom               {object specific hit geometry block}
      do begin
{
*   Abbreviations:
*     HIT_GEOM  -  Our hit geometry block in the MEM array.
*
*   Return intersection point coordinates.
}
  if ray_geom_point in flags then begin
    geom_info.flags :=                 {inidicate intersect point returned}
      geom_info.flags + [ray_geom_point];
    geom_info.point := hit_geom.point;
    end;
{
*   Return unit normal vector of surface at intersection point.
}
  if ray_geom_unorm in flags then begin
    geom_info.flags :=                 {indicate unit normal returned}
      geom_info.flags + [ray_geom_unorm];
    geom_info.unorm := hit_geom.unorm;
    end;                               {done returning unit surface normal}
  end;                                 {done using abbreviations}
  end;
