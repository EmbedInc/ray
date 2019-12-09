{   Subroutine RAY_VEIW1 (VIEW)
*
*   Set the view transformation for use by other RAY library routines.
}
module ray_view1;
define ray_view1;
%include 'ray2.ins.pas';

procedure ray_view1 (                  {set view xform with format 1 view descriptor}
  in      view: ray_view1_t);          {the view definition}
  val_param;

var
  eye_dist: real;                      {dimensionless eye distance perspective number}
  cent_vect: vect_3d_t;                {eye point to image center point vector}
  m: real;                             {scratch vector magnitude}

begin
  eye_dist := view.focal_len/15.0;     {make eye distance perspective number}
  cent_vect := vect_sub(view.img_cent, view.eye); {in direction of image center}
  vect_adjm (cent_vect, eye_dist);     {eye to image center vector}
  x_pix_vect := vect_cross(cent_vect, view.up); {direction of image X}
  m := vect_mag(x_pix_vect);           {get length of cross product result}
  if m < 1.0e-6 then begin             {gaze and up in about the same direction ?}
    x_pix_vect := vect_vector(1.0, 0.0, 0.0); {make up an X vector}
    m := 1.0;                          {make up magnitude to go with it}
    end;
  x_pix_vect :=                        {image center to right edge vector}
    vect_mult(x_pix_vect, view.x_size/m);
  y_pix_vect := vect_cross(cent_vect, x_pix_vect); {in direction of image -Y}
  vect_adjm (y_pix_vect, view.y_size); {image center to bottom edge vector}
  img_orig_vect := vect_sub(cent_vect, {eye to image top left corner vector}
    vect_add(x_pix_vect, y_pix_vect));
  x_pix_vect :=                        {final vector for one pixel to the right}
    vect_mult(x_pix_vect, 1.0/(view.x_pix*0.5));
  y_pix_vect :=                        {final vector for one pixel down the image}
    vect_mult(y_pix_vect, 1.0/(view.y_pix*0.5));
  eye_point := view.eye;               {save eye point in common block}
  end;
