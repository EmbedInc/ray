{   Subroutine RAY_GET_EYE_POINT (POINT)
*
*   Return the eye point saved as part of the view geometry.
}
module ray_get_eye_point;
define ray_get_eye_point;
%include 'ray2.ins.pas';

procedure ray_get_eye_point (          {return eye point from saved view parameters}
  out     point: vect_3d_t);           {returned 3D eye point}
  val_param;

begin
  point := eye_point;                  {return eye point from common block}
  end;
