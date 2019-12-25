{   Private insert file for TYPE1 routines.
}
%include 'ray2.ins.pas';
%include 'ray_type1.ins.pas';

const
  mem_block_size = 100000;             {number of bytes in mem block}
  max_mem_index = mem_block_size - 1;  {max index into mem block}

type
  stats_tri_t = record                 {triangle object statistics}
    isect_ray: sys_int_machine_t;      {number of ray/triangle intersect checks}
    hit_ray: sys_int_machine_t;        {ray/tri intersections found}
    isect_box: sys_int_machine_t;      {number of box/triangle intersect checks}
    hit_box: sys_int_machine_t;        {box/tri intersections found}
    end;

  stats_oct_t = record                 {octree object statistics}
    n_parent: sys_int_machine_t;       {number of parent octree nodes}
    n_leaf: sys_int_machine_t;         {number of leaf node octree voxels}
    mem: sys_int_adr_t;                {total memory allocated for octree overhead}
    end;
{
*********************************************************************************
*
*   Common block for all TYPE1 routines.
}
var (ray_type1_2)
  mem: array[0..max_mem_index] of char; {scratch area for DAG path and hit blocks}
  next_mem: sys_int_adr_t              {MEM index of start of free area}
    := 0;                              {init to all of MEM is free area}
  stats_tri: stats_tri_t;              {triangle primitive statistics}
  stats_oct: stats_oct_t;              {octree object statistics}
