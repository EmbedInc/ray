{   Subroutine TYPE1_IMAGE_ALIASED (IMG_HANDLE,RAY_CONTEXT)
*
*   Find the image value at each pixel, and write out the resulting image.
*   IMG_HANDLE is the handle to a previously opened image file.  The image
*   dimensions and the like are defined in IMG_HANDLE.  RAY_CONTEXT is the
*   static ray descriptor.  It must be compeltely filled in.  The view geometry
*   is also assumed to be already set up such that RAY_MAKE_RAY can be used
*   to convert pixel coordinates to eye ray unit vectors.
*
*   The resulting image will be aliased.  One sample will be taken at the center
*   of each pixel.
}
module type1_image_aliased;
define type1_image_aliased;
%include 'ray_type1_2.ins.pas';

procedure type1_image_aliased (        {make and write aliased image}
  in out  img_handle: img_conn_t;      {handle to previously opened image out file}
  in      ray_context: ray_context_t); {static part of ray information}

const
  scan_line_size = 4096;               {max number of pixels in a scan line}
  scan_line_max = scan_line_size - 1;  {max SCAN_LINE index}
  notify_n = 32;                       {num times to try to show scan line progress}
  notify_lines_min = 4;                {never show progress more often than this}

var
  scan_line:                           {one scan line buffer}
    array[0..scan_line_max] of img_pixel1_t;
  ray: type1_ray_t;                    {top level eye ray descriptor}
  ix, iy: sys_int_machine_t;           {integer pixel cooridinates}
  x, y: real;                          {floating point pixel coordinates}
  color: type1_color_t;                {resolved ray color}
  notify: sys_int_machine_t;           {notify user every this many scan lines}
  not_cnt: sys_int_machine_t;          {current notify count}
  stat: sys_err_t;                     {error status code}

begin
  if img_handle.x_size > scan_line_size then begin {image too big for us ?}
    writeln ('Image too wide for scan line buffer (TYPE1_IMAGE_ALIASED).');
    sys_bomb;
    end;

  notify := img_handle.y_size div notify_n; {init how often to notify of progress}
  notify := max(notify, notify_lines_min); {clip to most frequent notify allowed}
  not_cnt := 1;                        {init to notify on first scan line}
{
*   Fill in the constant parts of the ray descriptor.
}
  ray.base.context_p := addr(ray_context); {pointer to static ray descriptor}
  ray_get_eye_point (ray.point);       {ray origin point}
  ray.generation := 1;                 {ray generation number (eye ray = 1)}
  ray.energy := 1.0;                   {contribution into top level eye ray}
  ray.min_dist := 0.0;                 {minimum distance from eye point}

  for iy := 0 to img_handle.y_size-1 do begin {down the scan lines in the image}
    not_cnt := not_cnt - 1;            {one less scan line before a notify}
    if not_cnt <= 0 then begin         {time for notify ?}
      writeln ('Starting scan line', iy:5);
      not_cnt := notify;               {reset current notify count}
      end;
    y := iy + 0.5;                     {make floating point coordinate of line center}
    for ix := 0 to img_handle.x_size-1 do begin {accross the pixels in scan line}
      x := ix + 0.5;                   {make floating point coordinate of pixel center}
      ray_make_ray (x, y, ray.vect);   {make ray vector at this pixel coodinate}
      ray.max_dist := 1.0e20;          {maximum allowable distance from eye point}
      ray_trace (ray, color);          {get color of ray thru this pixel}
      color.red := max(0.0, min(0.9999, color.red)); {clip color values}
      color.grn := max(0.0, min(0.9999, color.grn));
      color.blu := max(0.0, min(0.9999, color.blu));
      color.alpha := max(0.0, min(0.9999, color.alpha));
      scan_line[ix].alpha := trunc(color.alpha*256.0); {make 8 bit color values}
      scan_line[ix].red := trunc(color.red*256.0);
      scan_line[ix].grn := trunc(color.grn*256.0);
      scan_line[ix].blu := trunc(color.blu*256.0);
      end;                             {back and get color at next pixel}
    img_write_scan1 (img_handle, scan_line, stat); {write this scan line to image file}
    sys_error_abort (stat, 'img', 'write_scan_line', nil, 0);
    end;                               {back and do next scan line down the image}

  img_close (img_handle, stat);        {close the image output file}
  sys_error_abort (stat, 'img', 'close', nil, 0);
  end;
