{   Private insert file for all the RAY_ routines except RAY_TRACE.
*   By definition, RAY_KERNEL.INS.PAS defines all the ray tracer data
*   structures needed by RAY_TRACE.  RAY_TRACE therefore only
*   inserts that file.
}
%include 'sys.ins.pas';
%include 'util.ins.pas';
%include 'string.ins.pas';
%include 'file.ins.pas';
%include 'vect.ins.pas';
%include 'ray_kernel.ins.pas';
%include 'ray.ins.pas';

var (ray_2)
  img_orig_vect: vect_3d_t;            {eye to image 0,0 vector}
  x_pix_vect: vect_3d_t;               {vector for one pixel to the right}
  y_pix_vect: vect_3d_t;               {vector for one pixel down}
  eye_point: vect_3d_t;                {3D coordinates of eye point}

