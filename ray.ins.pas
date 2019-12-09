{   Public include file for the RAY library.  This ray tracer beyond just the
*   kernel.
}
type
  ray_view1_t = record                 {format 1 view definition}
    eye: vect_3d_t;                    {eye point}
    img_cent: vect_3d_t;               {a point that will appear at image center}
    up: vect_3d_t;                     {up direction for camera orientation}
    focal_len: real;                   {focal length of lens (35mm film)}
    x_size: real;                      {image width relative to focal length result}
    y_size: real;                      {image height relative to focal length result}
    x_pix: sys_int_machine_t;          {number of pixels accross the image}
    y_pix: sys_int_machine_t;          {number of pixels up the image}
    end;

  ray_comlin_t = record                {template for command line arguments}
    fnam_in: string_treename_t;        {raw input file name from command line}
    size_x: sys_int_machine_t;         {horizontal image size in pixels}
    size_y: sys_int_machine_t;         {vertical image size in pixels}
    size_set: boolean;                 {TRUE if size explicitly set on command line}
    aa: boolean;                       {TRUE if anti-aliasing ON}
    aa_set: boolean;                   {TRUE if AA on/off explicitly set}
    aspect: real;                      {width/height of properly display image}
    aspect_set: boolean;               {TRUE if aspect explicitly set}
    end;

var (ray)
  ray_mem_p: util_mem_context_p_t;     {points to master memory context for ray lib}
{
*   Entry point definitions except for RAY_TRACE.  This entry point is defined
*   in RAY_KERNEL.INS.PAS.
}
procedure ray_close;                   {close use of ray tracer, release resources}
  val_param; extern;

procedure ray_get_eye_point (          {return eye point from saved view parameters}
  out     point: vect_3d_t);           {returned 3D eye point}
  val_param; extern;

procedure ray_init (                   {init ray tracer, must be first call}
  in out  parent_mem: util_mem_context_t); {all new ray mem will be below this}
  val_param; extern;

procedure ray_init_default;            {init ray tracer, use defaults}
  val_param; extern;

procedure ray_make_ray (               {make ray unit vector given pixel coordinates}
  in      pix_x: real;                 {X pixel coordinate (0 is left image edge)}
  in      pix_y: real;                 {Y pixel coordinage (0 is top image edge)}
  out     ray: vect_3d_t);             {ray unit vector from eye point to pixel}
  val_param; extern;

procedure ray_readin_com_line (        {read command line and pass back result}
  out     comlin: ray_comlin_t);       {data about command line}
  val_param; extern;

procedure ray_view1 (                  {set view xform with format 1 view descriptor}
  in      view: ray_view1_t);          {the view definition}
  val_param; extern;
