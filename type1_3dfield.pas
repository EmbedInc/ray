{   WARNING:  This code is old, and some data structures have changed out from
*   under it.  The source code is here to preserve it, but it is currently not
*   being built.
}

{   Type 1 3DFIELD primitive object.
}
module type1_3dfield;
define type1_3dfield_routines_make;
%include 'ray_type1_2.ins.pas';

const
  max_iso_iterate = 20;                {max iterations to find ray/iso intersect}
  iterate_dist_err = 1.0E-7;           {max allowable error on ray distance}
  iter_below_minvox = 6;               {coor iterations to do beyond min voxel size}

  obj_per_leaf_node = 7;               {number of object pointers in leaf node object}
  obj_per_data_node = 9;               {num of object pointers in chained data node}
  node_array_size = 30000;             {approximately 1 Mb worth of node descriptors}
  max_coor_level = 15;                 {how many subdivision levels to keep data for}
  int_scale = 268435456.0;             {octree floating point to integer coordinates}
  box_radius_scale = 1.0/int_scale;    {pick mask to voxel radius scale factor}
  box_width_scale = box_radius_scale * 2.0; {scale factor for full voxel width}
  node_subdiv_k = -1;                  {flag value indicating that voxel subdivided}
  iflag_mini_k = 1;                    {this array entry is at min I limit}
  iflag_minj_k = 2;                    {this array entry is at min J limit}
  iflag_mink_k = 4;                    {this array entry is at min K limit}
  iflag_maxi_k = 8;                    {this array entry is at max I limit}
  iflag_maxj_k = 16;                   {this array entry is at max J limit}
  iflag_maxk_k = 32;                   {this array entry is at max K limit}
  iflag_unorm_set_k = 64;              {UNORM field has been previously set}

type
  vect3_t = record case integer of     {3D vector with array overlay}
    1:(                                {separate named fields}
      x: real;
      y: real;
      z: real);
    2:(                                {overlay array for arithmetic indexing}
      coor: array[1..3] of real);
    end;

  array_coor_p_t =                     {pointer to XYZ coordinates in user array}
    ^vect_3d_t;

  array_coor_pp_t =                    {address of user array entry pointer}
    ^array_coor_p_t;

  array_data_p_t =                     {pointer to data value in user array}
    ^real;

  local_data_t = record                {data kept locally for each array entry}
    flags: integer32;                  {flags about this entry, use IFLAG_xxx_K}
    unorm: vect_3d_t;                  {iso value unit normal vector at this point}
    min_x, min_y, min_z: real;         {min bounds values of influence box}
    max_x, max_y, max_z: real;         {max bounds values of influence box}
    end;

const
  local_size_mult =                    {size factor for pointer array to local data}
    sizeof(local_data_t) div sizeof(array_coor_pp_t);

type
  local_data_p_t =                     {pointer to local data for one array entry}
    ^local_data_t;

  node_p_t =                           {pointer to an octree node}
    ^node_t;

  node_pp_t =                          {address of a node pointer}
    ^node_p_t;

  node_t = record case integer of      {template for one octree node}
    1:(                                {the node has been subdivided}
      min_val: real;                   {minimum data value over this voxel}
      max_val: real;                   {maximum data value over this voxel}
      subdiv_flag: integer32;          {set to NODE_SUBDIV_K for subdivided node}
      node_p:                          {pntrs to child nodes, index msb is Z, lsb is X}
        array[0..7] of node_p_t);      {NIL means child node is empty}
    2:(                                {the node is a leaf node}
      min_val2: real;                  {minimum data value over this voxel}
      max_val2: real;                  {maximum data value over this voxel}
      n_obj: integer32;                {total number of objects at this voxel}
      obj_p:                           {pointers to objects at this voxel}
        array[1..obj_per_leaf_node] of array_coor_pp_t;
      next_p: node_p_t);               {pointer to first chained data node}
    3:(                                {the node is a chained data node}
      ch_p:                            {more pointers to objects at this voxel}
        array[1..obj_per_data_node] of array_coor_pp_t;
      next_p2: node_p_t);              {pointer to next chained data node}
    4:(                                {unused node, on the free nodes chain}
      free_p: node_p_t);               {pointer to next node on free chain}
    end;

  node_array_t =                       {approximately 1Mb of node descriptors}
    array[1..node_array_size] of node_t;

  node_array_p_t =                     {pointer to a node array block}
    ^node_array_t;

  object_data_t = record               {data record pointed to by object block}
    shader: ray_shader_t;              {pointer to shader entry point}
    liparm_p: type1_liparm_p_t;        {pointer to light source parameters block}
    visprop_p: type1_visprop_p_t;      {pointer to visual properties block}
    min_gen: integer32;                {minimum subdivision level for non-empty voxels}
    max_gen: integer32;                {max allowed voxel subdivision level 0 = none}
    origin: vect_3d_t;                 {most negative corner point for all 3 axis}
    size: vect_3d_t;                   {outside octree size in each dimension}
    recip_size: vect_3d_t;             {reciprocal size for each dimension}
    top_node_p: node_p_t;              {pointer to top level node}
    ar_p: node_array_p_t;              {pointer to current nodes array}
    n_ar: integer32;                   {number of nodes used from current array so far}
    free_p: node_p_t;                  {pointer to start of free chain}
    i, j, k: integer32;                {user array dimensions}
    i_ofs: integer32;                  {array byte offset for +1 in I direction}
    j_ofs: integer32;                  {array byte offset for +1 in J direction}
    k_ofs: integer32;                  {array byte offset for +1 in K direction}
    first_array_pp: array_coor_pp_t;   {address of first user array index pointer}
    first_local_p: local_data_p_t;     {pointer to first local data entry}
    val_offset: integer32;             {byte offset from array entry to data value}
    n_iso: integer32;                  {number of iso-surfaces to draw}
    iso:                               {descriptor for each possible iso value}
      array[1..type1_3dfield_max_iso_vals_k] of type1_3dfield_iso_t;
    end;

  object_data_p_t =                    {pointer to object data block}
    ^object_data_t;

  shader_parms_t = record              {data saved in MEM array on ray hit}
    base: type1_hit_info_t;            {mandatory TYPE1 stuff (LIPARM_P, VISPROP_P)}
    iso: integer32;                    {index of iso-value that ray hit}
    voxel_p: node_p_t;                 {pointer to voxel where hit occurred}
    hit_coor: vect_3d_t;               {coordinate of hit point}
    ray_vect: vect_3d_t;               {unit ray vector of ray that hit}
    end;

  shader_parms_p_t =                   {pointer to data block in MEM array}
    ^shader_parms_t;
{
*   Static storage.  This storage is allocated at load time and therefore should not
*   be altered.
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
*   Subroutine entry point definitions.
}
procedure type1_3dfield_create (       {create new primitive with custom data}
  out     object: ray_object_t;        {newly filled in object block}
  in      data: type1_3dfield_user_data_t; {parameters from user}
  out     stat: sys_err_t);            {completion status code}
  forward;

function type1_3dfield_intersect_check ( {check for ray/object intersection}
  in out  ray: type1_ray_t;            {ray descriptor}
  in      object: ray_object_t;        {object to intersect ray with}
  in      parms: type1_object_parms_t; {run-time parameters passed from above}
  out     hit_info: ray_hit_info_t;    {all returned information}
  out     shader: ray_shader_t)        {pointer to shader to resolve color}
  :boolean; forward;                   {TRUE if ray does hit object}

procedure type1_3dfield_intersect_geom ( {return detailed geometry of intersection}
  in      hit_info: ray_hit_info_t;    {data saved by INTERSECT_CHECK}
  in      flags: ray_geom_flags_t;     {bit mask list of what is being requested}
  out     geom_info: ray_geom_info_t); {filled in geometric info}
  forward;

procedure type1_3dfield_intersect_box ( {find object/box intersection status}
  in      box:    ray_box_t;           {descriptor for a paralellpiped volume}
  in      object: ray_object_t;        {object to intersect with the box}
  out     here:   boolean;             {TRUE if ISECT_CHECK could be true in box}
  out     enclosed: boolean);          {TRUE if solid obj, and completely encloses box}
  forward;

procedure type1_3dfield_version (      {return version information about object}
  out     version: ray_object_version_t); {returned version information}
  forward;
{
****************************************************************************
*
*   Subroutine TYPE1_3DFIELD_ROUTINES_MAKE (POINTERS, SIZE)
*
*   Fill in the routines block for this class of objects.  SIZE is the size in bytes
*   of the data structure to be filled in.
}
procedure type1_3dfield_routines_make ( {fill in object routines block}
  out     pointers: ray_object_routines_t; {block to fill in}
  in      size: sys_int_adr_t);        {number of bytes in POINTERS}
  val_param;

var
  ents: integer32;                     {number of routine entries in POINTERS}
  i: integer32;                        {loop counter}
  max_ofs: integer32;                  {byte offset of last entry in POINTERS}
  p: ^ray_object_create_proc_t;        {pointer to a subroutine entry point}

begin
  ents := size div sizeof(p^);         {number of pointers in block}
  p := univ_ptr(addr(pointers));       {init pointer to first entry in POINTERS}
  for i := 1 to ents do begin          {once for each slot in POINTERS}
    p^ := nil;                         {init this slot in POINTERS to the nil pointer}
    p := univ_ptr(integer32(p)+sizeof(p^)); {point to next slot in POINTERS}
    end;
  max_ofs := (ents - 1)*4;             {byte offset of last entry}

  if (integer32(addr(pointers.version))-integer32(addr(pointers)))
    <= max_ofs                         {this slot within POINTERS ?}
    then pointers.version :=
      ray_object_version_proc_t(addr(type1_3dfield_version));
    ;
  if (integer32(addr(pointers.create))-integer32(addr(pointers)))
    <= max_ofs                         {this slot within POINTERS ?}
    then pointers.create :=
      ray_object_create_proc_t(addr(type1_3dfield_create));
    ;
  if (integer32(addr(pointers.intersect_check))-integer32(addr(pointers)))
    <= max_ofs                         {this slot within POINTERS ?}
    then pointers.intersect_check :=
      ray_object_isect_check_proc_t(addr(type1_3dfield_intersect_check));
    ;
  if (integer32(addr(pointers.intersect_geom))-integer32(addr(pointers)))
    <= max_ofs                         {this slot within POINTERS ?}
    then pointers.intersect_geom :=
      ray_object_isect_geom_proc_t(addr(type1_3dfield_intersect_geom));
    ;
  if (integer32(addr(pointers.intersect_box))-integer32(addr(pointers)))
    <= max_ofs                         {this slot within POINTERS ?}
    then pointers.intersect_box :=
      ray_object_isect_box_proc_t(addr(type1_3dfield_intersect_box));
    ;
  end;
{
****************************************************************************
*
*   Local subroutine TYPE1_3DFIELD_CREATE (OBJECT, DATA, STAT)
*
*   Fill in the new object in OBJECT.  DATA is the user data parameters for
*   this object.  STAT is the standard system error return code.
}
procedure type1_3dfield_create (       {create new primitive with custom data}
  out     object: ray_object_t;        {newly filled in object block}
  in      data: type1_3dfield_user_data_t; {parameters from user}
  out     stat: sys_err_t);            {completion status code}

var
  data_p: object_data_p_t;             {pointer to internal object data}
  i, j, k, kk: integer32;              {loop counters and scratch integers}
  array_pp: array_coor_pp_t;           {adr of current array entry pointer}
  local_p: local_data_p_t;             {pointer to current local data entry}
  coor_p: array_coor_p_t;              {pointer to current array entry coordinate}
  coor2_p: array_coor_p_t;             {pointer to coor of a bounds limit point}
  coor_pp: array_coor_pp_t;            {index pointer for a bounds limit point}
  val_p: array_data_p_t;               {pointer to scalar value for current entry}
  min_x, min_y, min_z: real;           {min coordinate accumulators}
  max_x, max_y, max_z: real;           {max coordinate accumulators}
  min_v, max_v: real;                  {min/max data value accumulators}
  minb_x, minb_y, minb_z: real;        {min influence bounds accumulators}
  maxb_x, maxb_y, maxb_z: real;        {max influence bounds accumulators}
  next_p: ^array_coor_pp_t;            {adr of next pointer to fill in octree}
  last_left: integer32;                {number of pointers left in curr octree node}
  node_p: node_p_t;                    {scratch pointer to an octree node}

begin
  sys_error_none (stat);               {init to no errors}

  util_mem_grab (                      {allocate data block for new object}
    sizeof(data_p^), ray_mem_p^, false, data_p);
  object.data_p := ray_obj_data_p_t(data_p); {set pointer to object data block}
  with data_p^:d do begin              {D abbrev stands for local data block}
{
*   Fill in all the local data that is just copied from the DATA call argument.
}
  d.shader := data.shader;             {shader to use for this object}
  d.liparm_p := data.liparm_p;         {lighting for this object}
  d.visprop_p := data.visprop_p;       {default visual properties}
  d.min_gen := data.min_gen;           {min octree subdivision generations}
  d.max_gen := data.max_gen;           {max octree subdivision generations}
  d.i := data.i;                       {user array dimensions}
  d.j := data.j;
  d.k := data.k;
  d.n_iso := data.n_iso;               {number of iso-surface to draw}
  d.val_offset := data.val_offset;     {array entry byte offset for scalar value}
  d.first_array_pp := data.array_adr;  {address of first array entry pointer}
  for i := 1 to d.n_iso do begin       {once for each iso-surface}
    d.iso[i] := data.iso[i];           {copy descriptor for this iso-surface}
    end;
  d.i_ofs := sizeof(d.first_array_pp^); {byte offset for next I index pointer}
  d.j_ofs := d.i_ofs * d.i;            {byte offset for next J index pointer}
  d.k_ofs := d.j_ofs * d.j;            {byte offset for next K index pointer}
{
*   Init the octree data structures.
}
  util_mem_grab (                      {allocate an initial block of nodes}
    sizeof(d.ar_p^), ray_mem_p^, false, d.ar_p);
  d.top_node_p := addr(d.ar_p^[1]);    {set pointer to top level node}
  d.n_ar := 1;                         {init to one node allocated in current array}
  d.free_p := nil;                     {init to no nodes on free chain}
  d.top_node_p^.n_obj :=               {set number of cells in array}
    d.i * d.j * d.k;
  d.top_node_p^.next_p := nil;         {init to no data nodes chained on}
  sys_mem_alloc (                      {allocate memory for local array data}
    sizeof(d.first_local_p^) * d.i * d.j * d.k, {number of bytes to allocate}
    d.first_local_p);                  {returned address of start of new memory}

  next_p := addr(d.top_node_p^.obj_p[1]); {init adr of next obj pointer to fill in}
  last_left := obj_per_leaf_node;      {init num object pointers left this node}
{
*   The octree is initialized with zero array entries.  Now loop thru each array
*   entry.  The array entry will be added to the top node, accumulated in the
*   min/max bounds for the octree, and its local data will be initialized.
}
  min_x := 1.0E30;                     {init min/max accumulators}
  max_x := -1.0E30;
  min_y := 1.0E30;
  max_y := -1.0E30;
  min_z := 1.0E30;
  max_z := -1.0E30;
  min_v := 1.0E30;
  max_v := -1.0E30;
  array_pp := d.first_array_pp;        {init adr of current array entry pointer}
  local_p := d.first_local_p;          {init pointer to current local data entry}
  node_p := d.top_node_p;              {init pointer to current octree node block}

  for k := 1 to d.k do begin           {loop once for each array element}
    for j := 1 to d.j do begin
      for i := 1 to d.i do begin
        coor_p := array_pp^;           {make pointer to coordinate for this entry}
        val_p :=                       {make pointer to data value for this entry}
          univ_ptr(integer32(coor_p) + d.val_offset);
        if coor_p^.x < min_x then min_x := coor_p^.x;
        if coor_p^.x > max_x then max_x := coor_p^.x;
        if coor_p^.y < min_y then min_y := coor_p^.y;
        if coor_p^.y > max_y then max_y := coor_p^.y;
        if coor_p^.z < min_z then min_z := coor_p^.z;
        if coor_p^.z > max_z then max_z := coor_p^.z;
        if val_p^ < min_v then min_v := val_p^;
        if val_p^ > max_v then max_v := val_p^;
        local_p^.flags := 0;           {init flags for this array entry}
        if i = 1 then
          local_p^.flags := local_p^.flags ! iflag_mini_k;
        if j = 1 then
          local_p^.flags := local_p^.flags ! iflag_minj_k;
        if k = 1 then
          local_p^.flags := local_p^.flags ! iflag_mink_k;
        if i = d.i then
          local_p^.flags := local_p^.flags ! iflag_maxi_k;
        if j = d.j then
          local_p^.flags := local_p^.flags ! iflag_maxj_k;
        if k = d.k then
          local_p^.flags := local_p^.flags ! iflag_maxk_k;
{
*   Create the min/max influence bounds box for this array element.
}
        minb_x := coor_p^.x;           {init min/max influence bounds accumulators}
        maxb_x := coor_p^.x;
        minb_y := coor_p^.y;
        maxb_y := coor_p^.y;
        minb_z := coor_p^.z;
        maxb_z := coor_p^.z;
        for kk := 0 to 7 do begin      {once for each array pnt to include in min/max}
          coor_pp := array_pp;         {init addr of this bounds array point index}
          if (kk & 1) = 0              {check I direction}
            then begin                 {this point is in negative I direction}
              if (local_p^.flags & iflag_mini_k) <> 0 then next;
              coor_pp := univ_ptr(
                integer32(coor_pp) - d.i_ofs);
              end
            else begin                 {this point is in positive I direction}
              if (local_p^.flags & iflag_maxi_k) <> 0 then next;
              coor_pp := univ_ptr(
                integer32(coor_pp) + d.i_ofs);
              end
            ;
          if (kk & 2) = 0              {check J direction}
            then begin                 {this point is in negative J direction}
              if (local_p^.flags & iflag_minj_k) <> 0 then next;
              coor_pp := univ_ptr(
                integer32(coor_pp) - d.j_ofs);
              end
            else begin                 {this point is in positive J direction}
              if (local_p^.flags & iflag_maxj_k) <> 0 then next;
              coor_pp := univ_ptr(
                integer32(coor_pp) + d.j_ofs);
              end
            ;
          if (kk & 4) = 0              {check K direction}
            then begin                 {this point is in negative K direction}
              if (local_p^.flags & iflag_mink_k) <> 0 then next;
              coor_pp := univ_ptr(
                integer32(coor_pp) - d.k_ofs);
              end
            else begin                 {this point is in positive K direction}
              if (local_p^.flags & iflag_maxk_k) <> 0 then next;
              coor_pp := univ_ptr(
                integer32(coor_pp) + d.k_ofs);
              end
            ;
          coor2_p := coor_pp^;         {get pointer to coor for this bounds limit}
          if coor2_p^.x < minb_x
            then minb_x := coor2_p^.x;
          if coor2_p^.x > maxb_x
            then maxb_x := coor2_p^.x;
          if coor2_p^.y < minb_y
            then minb_y := coor2_p^.y;
          if coor2_p^.y > maxb_y
            then maxb_y := coor2_p^.y;
          if coor2_p^.z < minb_z
            then minb_z := coor2_p^.z;
          if coor2_p^.z > maxb_z
            then maxb_z := coor2_p^.z;
          end;                         {back for next corner point of bounds box}
{
*   The raw influence bounds box is in MINB_X, MAXB_X, MINB_Y, MAXB_Y, MINB_Z,
*   and MAXB_Z.  Now adjust it to make sure that the center data point is not
*   directly at a side of the box.  In particular, the closer faces in each
*   dimension are moved out so that they are least 1/10 as far from the center
*   data point at the far face.
}
        if (maxb_x - coor_p^.x)*10.0 < (coor_p^.x - minb_x)
          then maxb_x := 1.1*coor_p^.x - 0.1*minb_x;
        if (coor_p^.x - minb_x)*10.0 < (maxb_x - coor_p^.x)
          then minb_x := 1.1*coor_p^.x - 0.1*maxb_x;
        if (maxb_y - coor_p^.y)*10.0 < (coor_p^.y - minb_y)
          then maxb_y := 1.1*coor_p^.y - 0.1*minb_y;
        if (coor_p^.y - minb_y)*10.0 < (maxb_y - coor_p^.y)
          then minb_y := 1.1*coor_p^.y - 0.1*maxb_y;
        if (maxb_z - coor_p^.z)*10.0 < (coor_p^.z - minb_z)
          then maxb_z := 1.1*coor_p^.z - 0.1*minb_z;
        if (coor_p^.z - minb_z)*10.0 < (maxb_z - coor_p^.z)
          then minb_z := 1.1*coor_p^.z - 0.1*maxb_z;

        local_p^.min_x := minb_x;      {set influence bounds box for this point}
        local_p^.max_x := maxb_x;
        local_p^.min_y := minb_y;
        local_p^.max_y := maxb_y;
        local_p^.min_z := minb_z;
        local_p^.max_z := maxb_z;

        if last_left <= 0 then begin   {no more room in current octree node ?}
          if d.n_ar >= node_array_size then begin {no room in curr mem block ?}
            sys_mem_alloc (sizeof(d.ar_p^), d.ar_p); {allocate new mem block for octree nodes}
            d.n_ar := 0;               {init number of nodes used in new block}
            end;                       {done handling nodes mem block full}
          d.n_ar := d.n_ar + 1;        {one more node in current nodes block}
          node_p^.next_p := addr(d.ar_p^[d.n_ar]); {make address of new chain node}
          node_p := node_p^.next_p;    {point to new current node block}
          node_p^.next_p := nil;       {indicate this is last node in chain}
          last_left := obj_per_data_node; {reset num pointers left in curr node}
          next_p := addr(node_p^.ch_p[1]); {pnt to next unused pointer in curr node}
          end;                         {done handling curr node full}
        next_p^ := array_pp;           {add this array box to octree}
        next_p :=                      {point to next empty pointer in curr node}
          univ_ptr(integer32(next_p) + sizeof(next_p^));
        last_left := last_left - 1;    {one less empty pointer in current node}
        array_pp :=                    {make adr of next array entry pointer}
          univ_ptr(integer32(array_pp) + sizeof(array_pp^));
        local_p :=                     {point to next local data entry}
          univ_ptr(integer32(local_p) + sizeof(local_p^));
        end;                           {back for next I index entry}
      end;                             {back for next J index entry}
    end;                               {back for next K index entry}

  d.origin.x := min_x;                 {set min coordinates corner of octree}
  d.origin.y := min_y;
  d.origin.z := min_z;
  d.size.x := max_x - min_x;           {set size of octree}
  d.size.y := max_y - min_y;
  d.size.z := max_z - min_z;
  d.recip_size.x := 1.0/d.size.x;      {make reciprocal sizes}
  d.recip_size.y := 1.0/d.size.y;
  d.recip_size.z := 1.0/d.size.z;

  d.top_node_p^.min_val := min_v;      {set min/max data value range for top voxel}
  d.top_node_p^.max_val := max_v;
  end;                                 {done with D abbreviation}
  end;
{
****************************************************************************
*
*   Local subroutine TYPE1_3DFIELD_VERSION (VERSION)
*
*   Return version information obout this class of objects.
}
procedure type1_3dfield_version (      {return version information about object}
  out     version: ray_object_version_t); {returned version information}

begin
  version.year := 1989;
  version.month := 7;
  version.day := 19;
  version.hour := 17;
  version.minute := 44;
  version.second := 0;
  version.version := 0;
  version.name := string_v('3DFIELD');
  version.aggregate := false;
  end;
{
***************************************************************************************
*
*   Local function TYPE1_3DFIELD_INTERSECT_CHECK (RAY,OBJECT,PARMS,HIT_INFO,SHADER)
*
*   Check ray and object for an intersection.  If so, return TRUE, and save
*   any partial results in HIT_INFO.  These partial results may be used later
*   to get detailed information about the intersection geometry.
}
function type1_3dfield_intersect_check ( {check for ray/object intersection}
  in out  ray: type1_ray_t;            {ray descriptor}
  in      object: ray_object_t;        {object to intersect ray with}
  in      parms: type1_object_parms_t; {run-time parameters passed from above}
  out     hit_info: ray_hit_info_t;    {all returned information}
  out     shader: ray_shader_t)        {pointer to shader to resolve color}
  :boolean;                            {TRUE if ray does hit object}

const
  mic = 4.76837E-7;                    {min representable coor in 0.0 to 1.0 space}
  mac = 1.0 - mic;                     {max representable coor in 0.0 to 1.0 space}

type
  icoor_t = record case integer of     {integer XYZ coordinate with array overlay}
    1:(                                {individually named fields}
      x, y, z: integer32);
    2:(                                {overlay array of algorithmic indexing}
      coor: array[1..3] of integer32);
    end;

  vstack_entry_t = record              {data about current voxel at one level}
    icoor: icoor_t;                    {voxel coordinate with unused bits set to zero}
    p: node_p_t;                       {pointer to voxel data}
    parent_p: node_pp_t;               {adr of pointer to the node at this level}
    end;

  pnt_list_entry_t = record
    coor_pp: array_coor_pp_t;          {pointer to index array entry for this point}
    ray_dist: real;                    {ray distance to perpendicular plane}
    end;

var
  r: real;                             {scratch floating point numbers}
  data_p: object_data_p_t;             {pointer to object's specific data area}
  p: vect3_t;                          {start of ray segment in octree}
  orayp: vect_3d_t;                    {ray point in octree 0,0,0 to 1,1,1 space}
  v: vect3_t;                          {original ray vector in octree space}
  vsq: vect3_t;                        {V with each component squared}
  maj, min1, min2: integer32;          {1-3 subscripts for major and minor axies}
  slope: vect3_t;                      {slope of each axis with respect to major axis}
  dmaj: real;                          {major axis delta}
  dmin: real;                          {minor axis delta}
  p1, p2, p3: vect3_t;                 {scratch points}
  level: integer32;                    {nesting level of current node, top = 0}
  coor_mask: integer32;                {masks in relevant adr bits for this level}
  icoor: icoor_t;                      {32 bit integer curr octree stepping coor}
  icoor2: icoor_t;                     {scratch integer coordinate}
  ray_iend: icoor_t;                   {integer octree coor of segment end point}
  max_iter_level: integer32;           {max level for iterating new coordinate}
  vox_icoor: icoor_t;                  {voxel coordinate with unused bits set to zero}
  coor_step:                           {delta ICOOR when major axis steps one voxel}
    array[0..max_coor_level] of icoor_t;
  vstack:                              {data about each nested voxel level}
    array[0..max_coor_level] of vstack_entry_t;
  vp: node_p_t;                        {pointer to current voxel}
  i, j, k: integer32;                  {loop counters and scratch integers}
  box_coor: array[0..1] of vect_3d_t;  {possible box corner points}
  shader_parms_p: shader_parms_p_t;    {pointer to data allocated from MEM array}

  min_v, max_v: real;                  {data value range for new voxel}
  local_p: local_data_p_t;             {pointer to local data for curr array point}
  array_coor_p: array_coor_p_t;        {pointer to array coordinate}
  array_coor_pp: array_coor_pp_t;      {address of pointer to array coordinate}
  array_data_p: array_data_p_t;        {pointer to array data value}
  min_x, max_x: real;                  {scratch min/max bounds values}
  min_y, max_y: real;
  min_z, max_z: real;
  coor_pp: array_coor_pp_t;            {scratch pointer into array index pointer list}
  coor_p: array_coor_p_t;              {scratch pointer to an array entry coordinate}
  vox_size: vect_3d_t;                 {ray space voxel size}
  ray_v: real;                         {data value at current ray point}
  ray_coor: vect_3d_t;                 {coordinate of current ray point}
  ray_dist: real;                      {ray distance to current ray point}
  old_ray_v: real;                     {data value at previous ray point}
  old_ray_coor: vect_3d_t;             {coordinate of previous ray point}
  old_ray_dist: real;                  {ray distance to previous ray point}
  val1: real;                          {data value at start of curr line segment}
  val2: real;                          {data value at end of curr line segment}
  val3: real;                          {data value at new line segment point}
  dist1, dist2, dist3: real;           {ray distances at iteration ray segment}
  last_dist: real;                     {ray distance at last best intersect guess}
  iso_val: real;                       {data value of current iso-surface}
  hit_iso: integer32;                  {index of iso-value that ray actually hit}
  hit_coor: vect_3d_t;                 {ray space coordinate of last hit point}
  vox_minc: vect3_t;                   {current voxel minimum coordinate}
  vox_maxc: vect3_t;                   {current voxel maximum coordinate}
  vox_grow: vect_3d_t;                 {amount to grow each face of voxel}

  newp: node_p_t;                      {pointer to new subdivided voxel}
  obj_pp: ^array_coor_pp_t;            {adr of next obj pnt in scanning voxel list}
  vlnp: node_p_t;                      {pointer to curr node in scanning voxel list}
  nleft: integer32;                    {obj pointers left this node in scanning list}
  new_left: integer32;                 {num obj pointers left in node being built}
  n_new: integer32;                    {total number of objects in voxel being built}
  new_pp: ^array_coor_pp_t;            {adr of next obj pointer in node being built}
  nnewp: node_p_t;                     {pointer to node being built}
  parent_p: node_pp_t;                 {address of pointer to current voxel}
  old_low: boolean;                    {TRUE if old ray point below threshold value}
  ray_ends: boolean;                   {TRUE if ray ends within octree}
  hit: boolean;                        {the ray hit an object}
  old_ray_valid: boolean;              {TRUE if field value exists at old ray point}
  ray_valid: boolean;                  {TRUE if field value exists at ray point}
  valid: boolean;                      {TRUE if field value exists}

label
  got_p, new_coor, got_start_level, new_voxel, leaf_node, subdivide_voxel,
  keep_voxel, trace_voxel, next_iso, next_coor, no_hit;
{
*************************************************************************************
*
*   Local function EVAL_FIELD(COOR,VALID)
*
*   Evaluate the 3D field data value at the coordinate COOR, and return the
*   result.  It is assumed that VP points to the current voxel.   VALID is
*   returned TRUE if a field value existed at COOR.
}
function eval_field (
  in      coor: vect_3d_t;             {ray space coordinate to evaluate field at}
  out     valid: boolean)              {true if data value existed here}
  :real;

var
  obj_pp: ^array_coor_pp_t;            {adr of next obj pnt in scanning voxel list}
  vlnp: node_p_t;                      {pointer to curr node in scanning voxel list}
  nleft: integer32;                    {obj pointers left this node in scanning list}
  coor_pp: array_coor_pp_t;            {pointer into index array}
  local_p: local_data_p_t;             {pointer to local data for curr array point}
  coor_p: array_coor_p_t;              {scratch pointer to curr array entry coor}
  val_p: array_data_p_t;               {pointer to data value for this array entry}

  acc: real;                           {data value accumulator}
  wat: real;                           {accumulator weighting factor}
  w: real;                             {weighting for current array point}
  i: integer32;                        {loop counter}
  lx, hx, ly, hy, lz, hz: real;        {data point dist from bounds box edges}

label
  next_point;

begin
  with                                 {set up abbreviations}
    data_p^:d
    do begin
{
*   Abbreviations:
*   D  -  Specific data block for OCTREE object
}
  acc := 0.0;                          {init data value accumulator}
  wat := 0.0;                          {init accumulator weighting factor}
  obj_pp := addr(vp^.obj_p[1]);        {init address of next object pointer}
  vlnp := vp;                          {init current node pointer}
  nleft := obj_per_leaf_node;          {init number of obj pointers left this node}
  for i := 1 to vp^.n_obj do begin     {once for each object in this voxel}
    if nleft <= 0 then begin           {no more object pointers in this node ?}
      vlnp := vlnp^.next_p;            {make pointer to next node in chain}
      obj_pp := addr(vlnp^.ch_p[1]);   {make adr of first object pointer in new node}
      nleft := obj_per_data_node;      {init number of object pointers left this node}
      end;
    coor_pp := obj_pp^;                {pointer to index array entry}
    coor_p := coor_pp^;                {pointer to array coordinate data}
    local_p := univ_ptr(               {make pointer to local data for this array ele}
      ((integer32(coor_pp) - integer32(d.first_array_pp))*local_size_mult)
      + integer32(d.first_local_p));
{
*   Check the user coordinate against the bounds box for this array element.
}
    lx := coor.x - local_p^.min_x;
    if lx <= 0.0 then goto next_point;
    hx := local_p^.max_x - coor.x;
    if hx <= 0.0 then goto next_point;
    ly := coor.y - local_p^.min_y;
    if ly <= 0.0 then goto next_point;
    hy := local_p^.max_y - coor.y;
    if hy <= 0.0 then goto next_point;
    lz := coor.z - local_p^.min_z;
    if lz <= 0.0 then goto next_point;
    hz := local_p^.max_z - coor.z;
    if hz <= 0.0 then goto next_point;
{
*   The current array element definately contributes to the data value at the
*   user coordinate.
}
    w :=                               {make weighting factor for this array element}
               lx / (coor_p^.x - local_p^.min_x + 1.0E-12);
    w := min(w, ly / (coor_p^.y - local_p^.min_y + 1.0E-12));
    w := min(w, lz / (coor_p^.z - local_p^.min_z + 1.0E-12));
    w := min(w, hx / (local_p^.max_x - coor_p^.x + 1.0E-12));
    w := min(w, hy / (local_p^.max_y - coor_p^.y + 1.0E-12));
    w := min(w, hz / (local_p^.max_z - coor_p^.z + 1.0E-12));
    val_p := univ_ptr(                 {make pointer to array element data value}
      integer32(coor_p) + d.val_offset);
    acc := acc + w*val_p^;             {accumulate contribution from this array ele}
    wat := wat + w;                    {accumulate contribution weight}

next_point:                            {jump here to go on to next array point}
    obj_pp :=                          {make adr of next obj pointer in this node}
      univ_ptr(integer32(obj_pp)+sizeof(obj_pp^));
    nleft := nleft-1;                  {one less object pointer left in this node}
    end;                               {back to process next array element in list}

  if wat < 1.0E-20 then begin          {no contributions here at all ?}
    valid := false;
    return;
    end;

  valid := true;
  eval_field := acc / wat;             {pass back weighted average of contributions}
  end;                                 {done with D abbrevation}
  end;
{
*************************************************************************************
*
*   Start of main routine.
}
begin
  data_p := object_data_p_t(object.data_p); {make pointer to object's data area}
  with                                 {set up abbreviations}
    data_p^:d
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
  if ray_ends then begin               {need to calculate integer ending coor ?}
    p1.x := orayp.x + v.x*ray.max_dist; {make ray end point in octree space}
    p1.y := orayp.y + v.y*ray.max_dist;
    p1.z := orayp.z + v.z*ray.max_dist;
    if  (p1.x >= mic) and (p1.x <= mac) and {ray end in or out octree ?}
        (p1.y >= mic) and (p1.y <= mac) and
        (p1.z >= mic) and (p1.z <= mac)
      then begin                       {ray ends inside the octree}
        ray_iend.x := trunc(p1.x*int_scale); {integer octree ray end coordinate}
        ray_iend.y := trunc(p1.y*int_scale);
        ray_iend.z := trunc(p1.z*int_scale);
        end
      else begin                       {ray ends outside the octree}
        ray_ends := false;             {as if ray doesn't end at all}
        end
      ;
    end;                               {done handling ray ends}
  vsq.x := sqr(v.x);                   {square each component of unscaled vector}
  vsq.y := sqr(v.y);
  vsq.z := sqr(v.z);
{
*   The ray point and ray vector have been transformed into a space in which the
*   octree occupies (0,0,0) to (1,1,1).  The transformed ray start point is in P,
*   and the transformed ray vector is in V.  This transformed space is only used
*   to decide which voxels the ray hits.  The original ray is passed directly to
*   the subordinate object intersection routines.
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
  if (p.x >= mic) and (p.x <= mac)     {is ray segment start inside octree ?}
      and (p.y >= mic) and (p.y <= mac)
      and (p.z >= mic) and (p.z <= mac)
    then goto got_p;                   {point P is staring point}
{
*   The ray segment start point is outside the octree.  Make points P1 and P2 which
*   are the intersection points between the ray and the front and back planes
*   perpendicular to the major axis.  P1 and P2 are clipped to the ray starting point.
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
*   P is the first point along the ray inside the octree where we are allowed to
*   look for hits.  P is in the octree (0,0,0) to (1,1,1) space.
*   Now do remaining initialization before entering main voxel stepping loop.
}
got_p:                                 {jump here if P started out inside octree}
  hit := false;                        {init to ray not hit anything in octree}
  max_iter_level := d.max_gen + iter_below_minvox; {set max coordinate level to use}
  if max_iter_level > max_coor_level   {clip to max coordinate level allocated}
    then max_iter_level := max_coor_level;
  r := pick_masks[max_iter_level-1]*box_width_scale; {octree space voxel grow amount}
  vox_grow.x := r * d.size.x;          {amount to add to voxel bounds}
  vox_grow.y := r * d.size.y;
  vox_grow.z := r * d.size.z;
  if v.coor[maj] >= 0                  {check sign of major axis}
    then begin                         {major axis is positive}
      icoor2.x := round(slope.x*int_scale); {make step delta for top voxel}
      icoor2.y := round(slope.y*int_scale);
      icoor2.z := round(slope.z*int_scale);
      end
    else begin                         {major axis is negative}
      icoor2.x := -round(slope.x*int_scale); {make step delta for top voxel}
      icoor2.y := -round(slope.y*int_scale);
      icoor2.z := -round(slope.z*int_scale);
      end
    ;
  coor_step[0] := icoor2;              {save step size for top voxel}
  for i := 1 to max_iter_level do begin {once for each used coordinate level}
    icoor2.x := icoor2.x div 2;        {make step size for voxel at this level}
    icoor2.y := icoor2.y div 2;
    icoor2.z := icoor2.z div 2;
    coor_step[i] := icoor2;            {set step sizes for this level}
    end;                               {back to set steps for next smaller voxel}
  icoor.x := trunc(p.x*int_scale);     {init current ray point integer coordinate}
  icoor.y := trunc(p.y*int_scale);
  icoor.z := trunc(p.z*int_scale);
  vstack[0].icoor.x := 0;              {set coordinate of top level voxel}
  vstack[0].icoor.y := 0;
  vstack[0].icoor.z := 0;
  vstack[0].p := d.top_node_p;         {set pointer to top level voxel}
  vstack[0].parent_p := addr(d.top_node_p); {init parent pointer of top level voxel}
  level := 0;                          {init to we are at top level voxel}
  coor_mask := coor_masks[level];      {init integer coordinate mask for this voxel}
{
*   Voxel loop.  Come back here for each new voxel to check.  ICOOR is set to
*   an integer coordinate inside the next voxel.  LEVEL is set to the level of
*   the previous voxel, and is also the index for the current voxel into VSTACK,
*   COOR_MASKS, PICK_MASKS, and COOR_STEP.
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
  while vp^.subdiv_flag = node_subdiv_k do begin {keep looping until find leaf node}
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
  if vp^.n_obj <= 9 then goto trace_voxel; {not too many objects to force subdivide ?}
{
*   This voxel needs to be subdivided.
}
subdivide_voxel:
  if d.free_p = nil                    {check for available nodes on free list}
    then begin                         {no nodes currently on free list}
      if d.n_ar >= node_array_size then begin {this node array filled up ?}
        util_mem_grab (                {allocate a new node array}
          sizeof(d.ar_p^), ray_mem_p^, false, d.ar_p);
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
  newp^.subdiv_flag := node_subdiv_k;  {indicate this is a subdivided node}
{
*   NEWP points to what will be the new node for this voxel.  It has already been
*   flagged as a subdivided node.  The 8 child pointers need to be filled in.
*   The box checks need to be performed before this can be done.  Since the boxes
*   share common coordinate values, these will be precomputed into array BOX_COOR.
*   The box descriptor BOX will be all set up except for the box corner point.
*   This will be put into the BOX_COOR array separately for each box.
}
  r :=                                 {make half width of voxel in octree space}
    pick_masks[level] * box_radius_scale;
  vox_size.x := r * d.size.x;          {make ray space size of a child voxel}
  vox_size.y := r * d.size.y;
  vox_size.z := r * d.size.z;

  box_coor[0].x := vox_icoor.x*box_radius_scale*d.size.x + d.origin.x;
  box_coor[1].x := box_coor[0].x + vox_size.x;
  box_coor[0].y := vox_icoor.y*box_radius_scale*d.size.y + d.origin.y;
  box_coor[1].y := box_coor[0].y + vox_size.y;
  box_coor[0].z := vox_icoor.z*box_radius_scale*d.size.z + d.origin.z;
  box_coor[1].z := box_coor[0].z + vox_size.z;

  for i := 0 to 7 do begin             {once for each subordinate voxel to create}
    min_x := box_coor[i&1].x;          {get min voxel coordinate values}
    min_y := box_coor[rshft(i, 1)&1].y;
    min_z := box_coor[rshft(i, 2)&1].z;
    max_x := min_x + vox_size.x + vox_grow.x; {make max voxel coordinate values}
    max_y := min_y + vox_size.y + vox_grow.y;
    max_z := min_z + vox_size.z + vox_grow.z;
    min_x := min_x - vox_grow.x;       {make final min voxel coordinate values}
    min_y := min_y - vox_grow.y;
    min_z := min_z - vox_grow.z;
    vlnp := vp;                        {init pointer to current node in voxel list}
    obj_pp := addr(vp^.obj_p[1]);      {get pointer to first object pointer}
    nleft := obj_per_leaf_node;        {init number of pointers left in curr node}
    if d.free_p = nil                  {check for available nodes on free list}
      then begin                       {no nodes currently on free list}
        if d.n_ar >= node_array_size then begin {this node array filled up ?}
          util_mem_grab (              {allocate a new node array}
            sizeof(d.ar_p^), ray_mem_p^, false, d.ar_p);
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
    min_v := 1.0E30;                   {init max/min range for new child voxel}
    max_v := -1.0E30;
    new_pp := addr(nnewp^.obj_p[1]);   {address of next object pointer to fill in}
    new_left := obj_per_leaf_node;     {obj pointers left to fill in this node}

    for j := 1 to vp^.n_obj do begin   {once for each object in current voxel}
      if nleft <= 0 then begin         {no more object pointers left this voxel ?}
        vlnp := vlnp^.next_p;          {point to next node in voxel data chain}
        obj_pp := addr(vlnp^.ch_p[1]); {get address of first object pointer this node}
        nleft := obj_per_data_node;    {reset number of object pointers available}
        end;                           {done switching to next data node}
      array_coor_pp := obj_pp^;        {get address of array coordinate index pointer}
      array_coor_p := array_coor_pp^;  {point to XYZ coordinate of this array point}
      array_data_p := univ_ptr(        {point to data value for this array point}
        integer32(array_coor_p) + d.val_offset);
      local_p := univ_ptr(             {point to local data for this array point}
        (integer32(array_coor_pp) - integer32(d.first_array_pp)) * local_size_mult
        + integer32(d.first_local_p));
      if
          (max(local_p^.min_x, min_x) < min(local_p^.max_x, max_x)) and
          (max(local_p^.min_y, min_y) < min(local_p^.max_y, max_y)) and
          (max(local_p^.min_z, min_z) < min(local_p^.max_z, max_z))
          then begin                   {array point needs to be added to voxel ?}
        if new_left <= 0 then begin    {no more object pointers left in this node ?}
          if d.free_p = nil            {check for available nodes on free list}
            then begin                 {no nodes currently on free list}
              if d.n_ar >= node_array_size then begin {this node array filled up ?}
                util_mem_grab (        {allocate a new node array}
                  sizeof(d.ar_p^), ray_mem_p^, false, d.ar_p);
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
        if array_data_p^ < min_v       {update min/max value range for this voxel}
          then min_v := array_data_p^;
        if array_data_p^ > max_v
          then max_v := array_data_p^;
        end;                           {done adding this array point to voxel}

      obj_pp :=                        {make address of next object pointer this node}
        univ_ptr(integer32(obj_pp)+sizeof(obj_pp^));
      nleft := nleft-1;                {one less obj left to read from this node}
      end;                             {back and check next object in old parent voxel}

    for j := 1 to d.n_iso do begin     {check new min/max against iso vals}
      if (d.iso[j].val >= min_v) and (d.iso[j].val <= max_v) {iso within range ?}
        then goto keep_voxel;          {we need to keep this voxel around}
      end;
{
*   The new child voxel either didn't contain any data points, or its min/max
*   range doesn't cover any of the iso-surface.  Deallocate its memory and
*   flag it as a non-existant voxel in its parent.
}
    nnewp := newp^.node_p[i];          {get pointer to first node in new child voxel}
    while nnewp <> nil do begin        {keep looping until end of chain}
      vlnp := nnewp^.next_p;           {save pointer to next node in data chain}
      nnewp^.free_p := d.free_p;       {put node at NNEWP onto free chain}
      d.free_p := nnewp;
      nnewp := vlnp;                   {point NNEWP to next node in voxel}
      end;                             {back do deallocate this new node}
    newp^.node_p[i] := nil;            {indicate this child voxel is empty}
    next;                              {go on to the next child voxel}

keep_voxel:                            {we need to keep this new child voxel}
    newp^.node_p[i]^.n_obj := n_new;   {set number of objects in child voxel}
    newp^.node_p[i]^.min_val := min_v; {set new voxel's min/max value range}
    newp^.node_p[i]^.max_val := max_v;
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
*   VP is pointing at a leaf node voxel that the ray is supposed to be traced
*   thru.
}
trace_voxel:                           {jump here to trace ray thru voxel at VP}
  vox_minc.x :=                        {make min coordinate of this voxel}
    vox_icoor.x*box_radius_scale*d.size.x + d.origin.x;
  vox_minc.y :=
    vox_icoor.y*box_radius_scale*d.size.y + d.origin.y;
  vox_minc.z :=
    vox_icoor.z*box_radius_scale*d.size.z + d.origin.z;
  r :=                                 {make width of this voxel}
    pick_masks[level] * box_width_scale;
  vox_maxc.x := vox_minc.x + r*d.size.x + vox_grow.x; {max voxel coordinate}
  vox_maxc.y := vox_minc.y + r*d.size.y + vox_grow.y;
  vox_maxc.z := vox_minc.z + r*d.size.z + vox_grow.z;
  vox_minc.x := vox_minc.x - vox_grow.x; {final min voxel coordinate}
  vox_minc.y := vox_minc.y - vox_grow.y;
  vox_minc.z := vox_minc.z - vox_grow.z;

  if ray.vect.coor[maj] > 0.0          {check sign of major axis}
    then begin                         {ray is heading towards positive major axis}
      old_ray_dist :=                  {ray distance to MAJ=near voxel face}
        (vox_minc.coor[maj] - ray.point.coor[maj]) / ray.vect.coor[maj];
      ray_dist :=                      {ray distance to MAJ=far voxel face}
        (vox_maxc.coor[maj] - ray.point.coor[maj]) / ray.vect.coor[maj];
      end
    else begin                         {ray is heading towards negative major axis}
      old_ray_dist :=                  {ray distance to MAJ=near voxel face}
        (vox_maxc.coor[maj] - ray.point.coor[maj]) / ray.vect.coor[maj];
      ray_dist :=                      {ray distance to MAJ=far voxel face}
        (vox_minc.coor[maj] - ray.point.coor[maj]) / ray.vect.coor[maj];
      end
    ;
  if old_ray_dist < ray.min_dist       {clip to min valid distance of ray segment}
    then old_ray_dist := ray.min_dist;
  if ray_dist > ray.max_dist           {clip to max valid distance of ray segment}
    then ray_dist := ray.max_dist;
  if ray_dist <= old_ray_dist          {nothing left where check for intersects}
    then goto next_coor;
  old_ray_coor.x :=                    {make coordinate of this ray point}
    ray.point.x + (old_ray_dist * ray.vect.x);
  old_ray_coor.y :=
    ray.point.y + (old_ray_dist * ray.vect.y);
  old_ray_coor.z :=
    ray.point.z + (old_ray_dist * ray.vect.z);
  old_ray_v := eval_field(old_ray_coor, old_ray_valid); {save field value here}
  if not old_ray_valid then goto next_coor;

  ray_coor.x :=                        {make coordinate of this ray point}
    ray.point.x + (ray_dist * ray.vect.x);
  ray_coor.y :=
    ray.point.y + (ray_dist * ray.vect.y);
  ray_coor.z :=
    ray.point.z + (ray_dist * ray.vect.z);
  ray_v := eval_field(ray_coor, ray_valid); {save field value here}
  if not ray_valid then goto next_coor;

  for j := 1 to d.n_iso do begin       {once for each iso-value}
    iso_val := d.iso[j].val;           {get this iso value}
    old_low := old_ray_v < iso_val;    {set flag for previous value was below iso}
    if old_low = (ray_v < iso_val)     {not crossed this iso-surf in curr interval ?}
      then next;                       {on to next iso-value}
{
*   We now have a ray segment that crosses the iso-value ISO_VAL.  The segment
*   extends from ray distance OLD_RAY_DIST to RAY_DIST.  The endpoint coordinates
*   are OLD_RAY_COOR and RAY_COOR.  The field values at the end points are
*   OLD_RAY_V and RAY_V.
*
*   The exact ray/iso intersect point will be found by succesive approximation.
*   The ray segment will be stored as point 1 to point 2 in ascending order of
*   the field value.  At each iteration, the segment slope is used to compute
*   a best guess point for the intersection.  The iteration is complete when
*   the latest best guess is within the error tolerance of the previous best
*   guess point.  Otherwise, one of the segment end points is moved to the middle,
*   and the result used as the segment for the next iteration.
}
    if old_low                         {check field value direction along ray}
      then begin                       {field value is ascending along ray}
        dist1 := old_ray_dist;
        val1 := old_ray_v;
        dist2 := ray_dist;
        val2 := ray_v;
        end
      else begin                       {field value is decending along ray}
        dist1 := ray_dist;
        val1 := ray_v;
        dist2 := old_ray_dist;
        val2 := old_ray_v;
        end
      ;
    r := (iso_val - val1) / (val2 - val1); {point 2 weighting factor for guess}
    dist3 := (1.0 - r)*dist1 + r*dist2; {make ray dist of first guess}
    last_dist := dist3;                {init ray distance to previous guess}

    for k := 1 to max_iso_iterate do begin {iterate for better answer}
      dist3 := 0.5*(dist1 + dist2);    {make ray dist at segment midpoint}
      p3.x :=                          {make coordinate at segment midpoint}
        ray.point.x + (dist3 * ray.vect.x);
      p3.y :=
        ray.point.y + (dist3 * ray.vect.y);
      p3.z :=
        ray.point.z + (dist3 * ray.vect.z);
      val3 := eval_field(p3, valid);   {find data value at new midpoint}
      if not valid then goto next_iso;
      if val3 < iso_val                {which half of segment has iso-val}
        then begin                     {iso-val is on point 2 side of segment}
          dist1 := dist3;
          val1 := val3;
          end
        else begin                     {iso-val is on point 1 side of segment}
          dist2 := dist3;
          val2 := val3;
          end
        ;
      r := (iso_val - val1) / (val2 - val1); {point 2 weighting factor for guess}
      dist3 := (1.0 - r)*dist1 + r*dist2; {make ray dist of new guess}
      if abs(dist3-last_dist) < iterate_dist_err {within ray dist err tolerance ?}
        then exit;                     {no need to iterate further}
      last_dist := dist3;              {update ray distance to previous guess}
      end;                             {back for another iteration}
{
*   DIST3 is the ray distance to the intersect point with iso-surface J.
}
    p3.x :=                            {make coordinate of intersect point}
      ray.point.x + (dist3 * ray.vect.x);
    p3.y :=
      ray.point.y + (dist3 * ray.vect.y);
    p3.z :=
      ray.point.z + (dist3 * ray.vect.z);
    if (p3.coor[min1] < vox_minc.coor[min1])
      then goto next_iso;
    if (p3.coor[min1] > vox_maxc.coor[min1])
      then goto next_iso;
    if (p3.coor[min2] < vox_minc.coor[min2])
      then goto next_iso;
    if (p3.coor[min2] > vox_maxc.coor[min2])
      then goto next_iso;
{
*   P3 is the best valid hit point so far.  The ray hit iso-surface J.
*   DIST3 is the ray distance to the hit point.
}
    hit := true;                       {indicate that the ray hit something}
    hit_iso := j;                      {save number of iso-surface that got hit}
    hit_coor := p3;                    {save coordinate of hit point}
    ray.max_dist := dist3;             {update max allowable ray dist to new hit}
next_iso:
    end;                               {back and test next iso-surf in this interval}
{
*   Check for have ray hit to pass back.  HIT_ISO is the iso-surface number that
*   got hit and RAY.MAX_DIST is the ray distance to the hit point.
*   VP is pointing to the voxel where the hit occurred.
*
*   Create and fill in all the data structures and pass back a TRUE status indicating
*   a hit.
}
  if hit then begin                    {ray hit something in this voxel ?}
    shader_parms_p := addr(mem[next_mem]); {get pointer to hit geom block}
    next_mem := ((sizeof(shader_parms_p^)+3) & 16#0FFFFFFFC)
      + next_mem;                      {allocate 4 byte chunks for HIT_GEOM block}
    if next_mem > mem_block_size then begin {not enough room for HIT_GEOM block ?}
      writeln ('Insufficient space in array MEM (RAY_TYPE1_2.INS.PAS).');
      sys_bomb;                        {save traceback info and abort}
      end;

    hit_info.object_p := addr(object); {return handle to object that got hit}
    hit_info.distance := ray.max_dist; {return ray distance to hit point}
    hit_info.shader_parms_p :=         {return pointer to shader parms and save area}
      univ_ptr(shader_parms_p);
    hit_info.enter := true;            {indicate ray is entering object}

    if d.shader = nil                  {check where shader addr is coming from}
      then shader := parms.shader      {inherit thru call arguments}
      else shader := d.shader;         {from object data block}

    if d.liparm_p = nil                {check where liparm pointer is coming from}
      then shader_parms_p^.base.liparm_p := parms.liparm_p
      else shader_parms_p^.base.liparm_p := d.liparm_p;
    if d.iso[hit_iso].visprop_p = nil  {where is visprop pointer coming from ?}
      then begin
        if d.visprop_p = nil
          then shader_parms_p^.base.visprop_p := parms.visprop_p
          else shader_parms_p^.base.visprop_p := d.visprop_p;
        end
      else begin
        shader_parms_p^.base.visprop_p := d.iso[hit_iso].visprop_p;
        end
      ;
    shader_parms_p^.iso := hit_iso;
    shader_parms_p^.voxel_p := vp;
    shader_parms_p^.hit_coor := hit_coor;
    shader_parms_p^.ray_vect := ray.vect;
    type1_3dfield_intersect_check := true; {indicate ray did hit something}
    return;
    end;                               {done passing back final ray hit}
{
*   We are done with the current voxel pointed to by VP.  Update point ICOOR to a
*   coordinate inside the next voxel, and then jump back to NEW_COOR.  COOR_MASK
*   is set to the coordinate mask for the level of the current voxel.  RAY_ENDS
*   is set to TRUE if the ray end point is within the octree.  In this case,
*   RAY_IEND is the integer octree ray ending coordinate.
}
next_coor:                             {jump here to step to the next voxel}
  if ray_ends then begin               {ray may end in this current voxel ?}
    if  (vox_icoor.x = ray_iend.x & coor_mask) and {ray ends in the current voxel ?}
        (vox_icoor.y = ray_iend.y & coor_mask) and
        (vox_icoor.z = ray_iend.z & coor_mask)
      then goto no_hit;                {no need to go on to other voxels}
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
    goto no_hit;                       {no more voxels left to do}
    end;
  goto new_coor;                       {ICOOR is new coordinate to find voxel at}
{
*   Jump here if the ray never intersected the outside box of the octree in the first
*   place.
}
no_hit:
  type1_3dfield_intersect_check := false; {indicate no hit}
  end;                                 {done with abbreviations}
  end;
{
****************************************************************************
*
*   Local subroutine TYPE1_3DFIELD_INTERSECT_GEOM (HIT_INFO,FLAGS,GEOM_INFO)
*
*   Return specific geometric information about a ray/object intersection
*   in GEOM_INFO.  In HIT_INFO are any useful results left by the ray/object
*   intersection check calculation.  FLAGS identifies what specific geometric
*   information is being requested.
}
procedure type1_3dfield_intersect_geom ( {return detailed geometry of intersection}
  in      hit_info: ray_hit_info_t;    {data saved by INTERSECT_CHECK}
  in      flags: ray_geom_flags_t;     {bit mask list of what is being requested}
  out     geom_info: ray_geom_info_t); {filled in geometric info}

var
  shader_parms_p: shader_parms_p_t;    {pointer to data in MEM array}
  data_p: object_data_p_t;             {pointer to specific data about this object}
  obj_pp: ^array_coor_pp_t;            {adr of next obj pnt in scanning voxel list}
  vlnp: node_p_t;                      {pointer to curr node in scanning voxel list}
  nleft: integer32;                    {obj pointers left this node in scanning list}
  val_p: array_data_p_t;               {pointer to scalar value for current entry}
  acc: vect_3d_t;                      {weighted average normal vector accumulator}
  w: real;                             {weighting for current array point}
  i, j: integer32;                     {scratch integers and loop counters}
  array_pp: array_coor_pp_t;           {pointer to index table entry for this ar pnt}
  local_p: local_data_p_t;             {pointer to local data for curr array point}
  coor_p: array_coor_p_t;              {scratch pointer to curr array entry coor}
  lx, hx, ly, hy, lz, hz: real;        {data point dist from bounds box edges}

  grad_acc: vect_3d_t;                 {gradient vector accumulator}
  grad: vect_3d_t;                     {current gradient vector}
  nei_coor_p: array_coor_p_t;          {pointer to neighbor element coordinate}
  nei_val_p: array_data_p_t;           {pointer to neighbor element data value}
  nei_coor_pp: array_coor_pp_t;        {pointer to neightor element index table entry}

label
  next_voxel;

begin
  shader_parms_p := univ_ptr(          {get pointer to data block in MEM array}
    hit_info.shader_parms_p);
  data_p := univ_ptr(                  {get pointer to specific data for this object}
    hit_info.object_p^.data_p);
  with                                 {set up abbreviations}
    shader_parms_p^: s,
    data_p^: d
      do begin
{
*   Abbreviations:
*     S - Shader parms data block in MEM array.
*     D - Specific data block for this object.
}
  geom_info.flags := [];               {init to nothing was returned}
{
*   Return intersection point coordinate.
}
  if ray_geom_point in flags then begin
    geom_info.point := s.hit_coor;     {pass back intersect coordinate}
    geom_info.flags :=                 {indicate intersect coordinates passed back}
      geom_info.flags + [ray_geom_point];
    end;
{
*   Return unit normal vector at intersect point.
}
  if ray_geom_unorm in flags then begin
    acc.x := 0.0;                      {init normal vector accumulator}
    acc.y := 0.0;
    acc.z := 0.0;
    obj_pp := addr(s.voxel_p^.obj_p[1]); {init address of next object pointer}
    vlnp := s.voxel_p;                 {init current node pointer}
    nleft := obj_per_leaf_node;        {init number of obj pointers left this node}

    for i := 0 to s.voxel_p^.n_obj-1 do begin {once for each object in this voxel}
      if nleft <= 0 then begin         {no more object pointers in this node ?}
        vlnp := vlnp^.next_p;          {make pointer to next node in chain}
        obj_pp := addr(vlnp^.ch_p[1]); {make adr of first object pointer in new node}
        nleft := obj_per_data_node;    {init number of object pointers left this node}
        end;
      array_pp := obj_pp^;             {make pointer into index table for this ele}
      local_p := univ_ptr(             {make pointer to local data for this array ele}
        ((integer32(array_pp) - integer32(d.first_array_pp)) * local_size_mult)
        + integer32(d.first_local_p));
{
*   Check the user coordinate against the bounds box for this array element.
}
      lx := s.hit_coor.x - local_p^.min_x; {check against X limits}
      if lx <= 0.0 then goto next_voxel;
      hx := local_p^.max_x - s.hit_coor.x;
      if hx <= 0.0 then goto next_voxel;

      ly := s.hit_coor.y - local_p^.min_y; {check against Y limits}
      if ly <= 0.0 then goto next_voxel;
      hy := local_p^.max_y - s.hit_coor.y;
      if hy <= 0.0 then goto next_voxel;

      lz := s.hit_coor.z - local_p^.min_z; {check against Z limits}
      if lz <= 0.0 then goto next_voxel;
      hz := local_p^.max_z - s.hit_coor.z;
      if hz <= 0.0 then goto next_voxel;
{
*   The current array element definately contributes to the data value at the
*   user coordinate.
}
      coor_p := array_pp^;             {get pointer to coor of this array element}

      if (local_p^.flags & iflag_unorm_set_k) = 0 then begin {unorm not already here ?}
        val_p := univ_ptr(             {make pointer to this element's data value}
          integer32(coor_p) + d.val_offset);
        grad_acc.x := 0.0;             {init gradient vector accumulator}
        grad_acc.y := 0.0;
        grad_acc.z := 0.0;
        for j := 1 to 6 do begin       {once for each neighbor array element}
          case j of                    {different code for each neighbor}
1:          begin
              if (local_p^.flags & iflag_mini_k) <> 0 {this neighbor doesn't exist ?}
                then next;
              nei_coor_pp := univ_ptr( {make pointer to neighbor's index entry}
                integer32(array_pp) - d.i_ofs);
              end;
2:          begin
              if (local_p^.flags & iflag_maxi_k) <> 0 {this neighbor doesn't exist ?}
                then next;
              nei_coor_pp := univ_ptr( {make pointer to neighbor's index entry}
                integer32(array_pp) + d.i_ofs);
              end;
3:          begin
              if (local_p^.flags & iflag_minj_k) <> 0 {this neighbor doesn't exist ?}
                then next;
              nei_coor_pp := univ_ptr( {make pointer to neighbor's index entry}
                integer32(array_pp) - d.j_ofs);
              end;
4:          begin
              if (local_p^.flags & iflag_maxj_k) <> 0 {this neighbor doesn't exist ?}
                then next;
              nei_coor_pp := univ_ptr( {make pointer to neighbor's index entry}
                integer32(array_pp) + d.j_ofs);
              end;
5:          begin
              if (local_p^.flags & iflag_mink_k) <> 0 {this neighbor doesn't exist ?}
                then next;
              nei_coor_pp := univ_ptr( {make pointer to neighbor's index entry}
                integer32(array_pp) - d.k_ofs);
              end;
6:          begin
              if (local_p^.flags & iflag_maxk_k) <> 0 {this neighbor doesn't exist ?}
                then next;
              nei_coor_pp := univ_ptr( {make pointer to neighbor's index entry}
                integer32(array_pp) + d.k_ofs);
              end;
            end;                       {end of specific code for each neighbor dir}
          nei_coor_p := nei_coor_pp^;  {get pointer to neighbor coordinate data}
          nei_val_p := univ_ptr(       {make pointer to neighbor data value}
            integer32(nei_coor_p) + d.val_offset);
          grad.x := nei_coor_p^.x - coor_p^.x; {raw displacement vector to neighbor}
          grad.y := nei_coor_p^.y - coor_p^.y;
          grad.z := nei_coor_p^.z - coor_p^.z;
          w := sqr(grad.x) + sqr(grad.y) + sqr(grad.z); {square of dist to neighbor}
          if w < 1.0E-10 then next;    {too close, skip this neighbor ?}
          w := (nei_val_p^ - val_p^)/w; {scale factor to make gradient vector}
          grad_acc.x := grad_acc.x + w*grad.x; {accumulate this gradient contribution}
          grad_acc.y := grad_acc.y + w*grad.y;
          grad_acc.z := grad_acc.z + w*grad.z;
          end;                         {back and process next neighbor array point}
        w :=                           {scale factor for unitizing average gradient}
          1.0 / sqrt(sqr(grad_acc.x) + sqr(grad_acc.y) + sqr(grad_acc.z));
        local_p^.unorm.x := grad_acc.x * w;
        local_p^.unorm.y := grad_acc.y * w;
        local_p^.unorm.z := grad_acc.z * w;
        local_p^.flags := local_p^.flags ! iflag_unorm_set_k; {unorm is now set}
        end;                           {done computing unit normal at this element}

      w :=                             {make weighting factor for this array element}
                 lx / (coor_p^.x - local_p^.min_x + 1.0E-12);
      w := min(w, ly / (coor_p^.y - local_p^.min_y + 1.0E-12));
      w := min(w, lz / (coor_p^.z - local_p^.min_z + 1.0E-12));
      w := min(w, hx / (local_p^.max_x - coor_p^.x + 1.0E-12));
      w := min(w, hy / (local_p^.max_y - coor_p^.y + 1.0E-12));
      w := min(w, hz / (local_p^.max_z - coor_p^.z + 1.0E-12));
      acc.x := acc.x + w*local_p^.unorm.x; {accumulate weighted average unit normal}
      acc.y := acc.y + w*local_p^.unorm.y;
      acc.z := acc.z + w*local_p^.unorm.z;

next_voxel:                            {jump here to abort out of current voxel}
      obj_pp :=                        {make adr of next obj pointer in this node}
        univ_ptr(integer32(obj_pp)+sizeof(obj_pp^));
      nleft := nleft-1;                {one less object pointer left in this node}
      end;                             {back and make local copy of next pointer}

    if ((shader_parms_p^.ray_vect.x * acc.x) +
        (shader_parms_p^.ray_vect.y * acc.y) +
        (shader_parms_p^.ray_vect.z * acc.z)) < 0.0
      then begin                       {ray and norm facing, don't flip norm}
        w := 1.0 / sqrt(sqr(acc.x) + sqr(acc.y) + sqr(acc.z)); {unity scale factor}
        end
      else begin                       {norm facing away from ray, flip norm}
        w := -1.0 / sqrt(sqr(acc.x) + sqr(acc.y) + sqr(acc.z)); {unity scale factor}
        end
      ;
    geom_info.unorm.x := acc.x * w;    {pass back unitized vector}
    geom_info.unorm.y := acc.y * w;
    geom_info.unorm.z := acc.z * w;
    geom_info.flags :=                 {indicate unit normal passed back}
      geom_info.flags + [ray_geom_unorm];
    end;                               {done passing back unit normal vector}
  end;                                 {done with S and D abbreviations}
  end;
{
***************************************************************************************
*
*   Local subroutine TYPE1_3DFIELD_INTERSECT_BOX (BOX,OBJECT,HERE,ENCLOSED)
*
*   Find the intersection status between this object and a paralellpiped.
*   HERE is returned as TRUE if the intersect check routine for this object could
*   ever return TRUE for ray within the box volume.  ENCLOSED is returned as true
*   if the object completely encloses the box.
}
procedure type1_3dfield_intersect_box ( {find object/box intersection status}
  in      box:    ray_box_t;           {descriptor for a paralellpiped volume}
  in      object: ray_object_t;        {object to intersect with the box}
  out     here:   boolean;             {TRUE if ISECT_CHECK could be true in box}
  out     enclosed: boolean);          {TRUE if solid obj, and completely encloses box}

begin
  here := true;
  enclosed := false;
  end;
