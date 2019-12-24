{   Internal data structures used by the OCTREE object, that must also be
*   visible to the OCTREE_DATA object.
}
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

  node_pp_t = ^node_p_t;
  node_p_t = ^node_t;
  node_t = record case integer of      {template for one octree node}
    1:(                                {the node is a leaf node}
      n_obj: sys_int_machine_t;        {total number of objects at this voxel}
      hits: sys_int_machine_t;         {num of rays thru this voxel that hit something}
      misses: sys_int_machine_t;       {num of rays thru this voxel that hit nothing}
      obj_p:                           {pointers to objects at this voxel}
        array[1..obj_per_leaf_node] of ray_object_p_t;
      next_p: node_p_t);               {pointer to first chained data node}
    2:(                                {the node has been subdivided}
      unused1: sys_int_machine_t;      {set to -1, overlay onto N_OBJ below}
      node_p:                          {pntrs to child nodes, index msb is Z, lsb is X}
        array[0..7] of node_p_t);      {NIL means child node is empty}
    3:(                                {the node is a chained data node}
      ch_p:                            {more pointers to objects at this voxel}
        array[1..obj_per_data_node] of ray_object_p_t;
      unused2: node_p_t);              {use NEXT_P field above}
    4:(                                {unused node, on the free nodes chain}
      free_p: node_p_t);               {pointer to next node on free chain}
    end;

  node_array_p_t = ^node_array_t;
  node_array_t =                       {approximately 1Mb of node descriptors}
    array[1..node_array_size] of node_t;

  oct_data_p_t = ^oct_data_t;
  oct_data_t = record                  {data record pointed to by object block}
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
