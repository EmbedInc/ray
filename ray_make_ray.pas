{   Subroutine RAY_MAKE_RAY (PIX_X, PIX_Y, RAY)
*
*   Make the unit vector for an eye ray given the floating point
*   pixel coordinates PIX_X and PIX_Y.  The top left corner of the
*   top left pixel is at 0,0.  Eack pixel is 1.0 wide, and 1.0
*   tall.
*
*   The scene geometry comes from the common block.  Relevant stored values
*   are:
*
*     IMG_ORIG_VECT  -  Vector from eye point to image plane 0,0.
*
*     X_PIX_VECT, Y_PIX_VECT  -  Image plane single pixel displacement vectors.
}
module ray_make_ray;
define ray_make_ray;
%include 'ray2.ins.pas';

procedure ray_make_ray (               {make ray unit vector given pixel coordinates}
  in      pix_x: real;                 {X pixel coordinate (0 is left image edge)}
  in      pix_y: real;                 {Y pixel coordinage (0 is top image edge)}
  out     ray: vect_3d_t);             {ray unit vector from eye point to pixel}
  val_param;

var
  m: real;                             {used to unitize vector}

begin
  ray.x := img_orig_vect.x + (x_pix_vect.x * pix_x) + (y_pix_vect.x * pix_y);
  ray.y := img_orig_vect.y + (x_pix_vect.y * pix_x) + (y_pix_vect.y * pix_y);
  ray.z := img_orig_vect.z + (x_pix_vect.z * pix_x) + (y_pix_vect.z * pix_y);

  m := 1.0 / sqrt( sqr(ray.x) + sqr(ray.y) + sqr(ray.z)); {unitize scale factor}

  ray.x := ray.x * m;                  {unitize vector from eye to image plane point}
  ray.y := ray.y * m;
  ray.z := ray.z * m;
  end;
