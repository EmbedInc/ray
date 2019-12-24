{   Subroutine TYPE1_SHADER_PHONG (RAY, HIT_INFO, COLOR)
*
*   Return the ray color for a given intersection in COLOR.  RAY is the ray
*   descriptor.  HIT_INFO is all the intermediate data returned by the object
*   intersect check routine.
}
module type1_shader_phong;
define type1_shader_phong;
%include 'ray_type1_2.ins.pas';

procedure type1_shader_phong (         {shader using Phong lighting model}
  in var  ray: type1_ray_t;            {handle to the ray}
  in var  hit_info: ray_hit_info_t;    {info about specific intersection}
  out     color: type1_color_t);       {returned ray color}

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  geom_info: ray_geom_info_t;          {returned geometric info about hit}
  hit_geom_p:                          {pointer to object private hit geom block}
    type1_hit_geom_p_t;
  visprop_p: type1_visprop_p_t;        {local copy of pointer to visprop block}
  liparm_p: type1_liparm_p_t;          {local copy of pointer to liparm block}
  ray2: type1_ray_t;                   {ray descriptor for recursive rays}
  i: sys_int_machine_t;                {scratch integer and loop counter}
  dot: real;                           {result of dot product}
  alpha: real;                         {opacity at hit point weighted by ray energy}
  light_red, light_grn, light_blu: real; {color fractions of current light source}
  above_hitp: vect_3d_t;               {point just above actual hit point}
  m: real;                             {scratch multiplication factor}
  r: real;                             {scratch real number}
  refl_x, refl_y, refl_z: real;        {scratch light reflection vector}
  ray_p: ^type1_ray_t;                 {kluge to allow writing to RAY}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

label
  done_spec, leave;
{
********************************************************************************
*
*   Local subroutine LIGHT_RAY
*
*   Determine the coupling factor between the hit point and a light source.  A
*   ray will be launched to a light source to determine the coupling factor.
*   The following fields are assumed to be set in RAY2, which will be the
*   descriptor for the recusive ray:
*
*     BASE  -  Mandatory basic ray info.
*
*     VECT  -  Unit vector to the light source.
*
*     GENERATION  -  Recursive generation counter.
*
*     MAX_DIST  -  Distance to the light source.
*
*   RAY2.ENERGY will be the returned coupling factor for this light source into
*   the final top generation ray color.  It will be 0.0 when the light source is
*   completely occluded, and ALPHA when it is completely visible from the hit
*   point.
}
procedure light_ray;

var
  hit_info: ray_hit_info_t;            {hit info block for recursive rays}
  shader: ray_shader_t;                {unused shader handle}
  hit_geom_p:                          {pointer to object private hit geom block}
    type1_hit_geom_p_t;
  geom_info: ray_geom_info_t;          {returned geometric info about hit}
  m: real;                             {scratch multiplier factor}

label
  hit_loop;

begin
  ray2.point := above_hitp;            {start ray just above surface from hit point}
  ray2.energy := alpha;                {importance of this ray}
  ray2.min_dist := 0.0;                {start looking for objects immediately}

hit_loop:                              {back here after each new hit}
  if ray2.energy < 0.001 then begin    {too little left to make a difference ?}
    ray2.energy := 0.0;
    return;
    end;
  if not                               {hit nothing, so got to the light source ?}
      ray2.base.context_p^.top_level_obj_p^.routines_p^.intersect_check^ ( {hit ?}
        ray2,                          {the ray descriptor}
        ray2.base.context_p^.top_level_obj_p^, {object to intersect ray with}
        ray2.base.context_p^.object_parms_p^, {run time parameters for top level object}
        hit_info,                      {specific data returned about this hit}
        shader)                        {unused, we just want to know if it hit}
    then return;                       {RAY2.ENERGY is all set}
{
*   This light ray hit something on the way to the light source.
}
  hit_info.object_p^.routines_p^.intersect_geom^ ( {make valid VISPROP, get SHNORM}
    hit_info,                          {info about this intersection}
    [ray_geom_unorm],                  {we only need to know shading normal vector}
    geom_info);                        {returned data}
  hit_geom_p :=                        {get pointer to shader parameter pointers}
    type1_hit_geom_p_t(hit_info.shader_parms_p);
  with hit_geom_p^.visprop_p^: visprop do begin {VISPROP is visual prop of hit obj}
  if not visprop.opac_on then begin    {object is opaque ?}
    ray2.energy := 0.0;
    return;
    end;
{
*   The object the ray hit is partially transparent.
}
  if visprop.opac_front = visprop.opac_side
    then begin                         {transparency is independent of angle}
      ray2.energy :=                   {weight remaining after passing thru this obj}
        ray2.energy * (1.0 - visprop.opac_front);
      end
    else begin                         {transparency is a function of hit angle}
      m := abs(                        {cosine between normal and ray vectors}
        (ray2.vect.x * geom_info.unorm.x) +
        (ray2.vect.y * geom_info.unorm.y) +
        (ray2.vect.z * geom_info.unorm.z));
      m :=                             {blended opacity between front and side values}
        (m * visprop.opac_front) +
        ((1.0 - m) * visprop.opac_side);
      ray2.energy :=                   {weight remaining after passing thru this obj}
        ray2.energy * (1.0 - m);
      end
    ;                                  {new RAY2.ENERGY all set}
  ray2.min_dist := hit_info.distance + 1.0E-4; {step past object we just hit}
  goto hit_loop;                       {continue tracing ray after this object}
  end;                                 {done with VISPROP abbreviation}
  end;
{
********************************************************************************
*
*   Start of main routine.
}
begin
  if                                   {don't take this ray seriously ?}
      (ray.generation > type1_max_generation_k) or
      (ray.energy < type1_min_energy_k)
      then begin
    color.red := 0.30 * ray.energy;    {pass back neutral gray}
    color.grn := 0.30 * ray.energy;
    color.blu := 0.30 * ray.energy;
    color.alpha := ray.energy;
    goto leave;
    end;

  hit_info.object_p^.routines_p^.intersect_geom^ ( {get geom info about this hit}
    hit_info,                          {info about this intersection}
    [ray_geom_unorm, ray_geom_point],  {list of requested info}
    geom_info);                        {returned data}

  hit_geom_p :=                        {get pointer to shader parameter pointers}
    type1_hit_geom_p_t(hit_info.shader_parms_p);
  visprop_p := hit_geom_p^.visprop_p;  {make local copy of visprop pointer}
  liparm_p := hit_geom_p^.liparm_p;    {make local copy of liparm pointer}

  with
      liparm_p^: liparm,               {light source descriptor block}
      visprop_p^: visprop,             {object visual properties block}
      geom_info.point: hitp,           {coordinate of hit point}
      geom_info.unorm: shnorm          {unit surface normal vector for shading}
      do begin
{
*   The following abbreviations have been set up:
*
*   LIPARM   -  LIPARM light source properties block.
*   VISPROP  -  VISPROP object visual properties block.
*   HITP     -  Coordinate of hit point.
*   SHNORM   -  Unit surface normal vector to use for shading
*
*   Initialize RAY2, the ray descriptor for any recursive or light source rays.
*   We will fill in the fields that won't change for all the various rays we
*   might launch.
}
  ray2.base := ray.base;               {copy over the mandatory fields}
  ray2.generation := ray.generation+1; {this is yet another recursive ray generation}
{
*   Init the returned color to the emissive color and whatever is shining thru
*   the object.  Our local value ALPHA will be set to the 1.0 to 0.0 weighted
*   opacity fraction for later use.  This will take into account the weighting
*   of the original ray.
}
  if visprop.opac_on
    then begin                         {transparency is enabled}
      if visprop.opac_front = visprop.opac_side
        then begin                     {transparency value is fixed}
          alpha := visprop.opac_front;
          end
        else begin                     {transparency is a function of angle}
          dot := abs(                  {cosine between normal and eye vectors}
            (ray.vect.x * shnorm.x) +
            (ray.vect.y * shnorm.y) +
            (ray.vect.z * shnorm.z));
          alpha :=                     {blend opacity between front and side values}
            (dot * visprop.opac_front) +
            ((1.0 - dot) * visprop.opac_side);
          end
        ;                              {ALPHA is unweighted opacity at hit point}
      ray2.point := ray.point;         {start point of recursive ray}
      ray2.vect := ray.vect;           {unit ray vector}
      ray2.energy := ray.energy * (1.0 - alpha); {weighting factor for this ray}
      ray2.min_dist := hit_info.distance + 1.0E-4; {start a little past hit point}
      ray2.max_dist := ray.max_dist;   {distance to farthest acceptable hit point}
      ray_trace (ray2, color);         {init color from transparent part}
      alpha := ray.energy * alpha;     {make final weighted opacity fraction}

      color.red := color.red + (visprop.emis_red * alpha); {emissive contribution}
      color.grn := color.grn + (visprop.emis_grn * alpha);
      color.blu := color.blu + (visprop.emis_blu * alpha);
      color.alpha := color.alpha + alpha; {all remaining contributions are opaque}
      end
    else begin                         {transparency is disabled}
      alpha := ray.energy;             {set total fraction for opaque part}
      color.red := visprop.emis_red * alpha;
      color.grn := visprop.emis_grn * alpha;
      color.blu := visprop.emis_blu * alpha;
      color.alpha := alpha;
      end
    ;
{
*******************
*
*   Sum all the color contributions that are a function of the light sources.
}
  above_hitp.x := hitp.x + (shnorm.x * 3.0E-4); {point to launch light rays from}
  above_hitp.y := hitp.y + (shnorm.y * 3.0E-4);
  above_hitp.z := hitp.z + (shnorm.z * 3.0E-4);

  for i := 1 to liparm.n_lights do begin {once for each light source}
    with liparm.light[i]: light do begin {LIGHT is descriptor for this light source}
    case light.ltype of                {different code for each type of light source}
{
*   Light source is turned off.
}
type1_ltype_off_k: begin
  next;                                {go on to next light source}
  end;
{
*   Ambient light source.
}
type1_ltype_ambient_k: begin
  color.red := color.red +
    (light.amb_red * visprop.diff_red * alpha);
  color.grn := color.grn +
    (light.amb_grn * visprop.diff_grn * alpha);
  color.blu := color.blu +
    (light.amb_blu * visprop.diff_blu * alpha);
  next;                                {go on to next light source}
  end;
{
*   Directional light source.
}
type1_ltype_directional_k: begin
  ray2.vect := light.dir_uvect;        {get unit vector to light source}
  ray2.max_dist := 1.0E30;             {set max allowed hit distance to infinity}
  light_ray;                           {follow ray towards light source}
  if ray2.energy < 0.001 then next;    {light source totally blocked ?}

  light_red := light.dir_red * ray2.energy; {get light source raw colors}
  light_grn := light.dir_grn * ray2.energy;
  light_blu := light.dir_blu * ray2.energy;
  end;
{
*   Point light source with no light fall off.
}
type1_ltype_point_constant_k: begin
  ray2.vect.x :=                       {make ununitized vector to light source}
    light.pcon_coor.x - hitp.x;
  ray2.vect.y :=
    light.pcon_coor.y - hitp.y;
  ray2.vect.z :=
    light.pcon_coor.z - hitp.z;
  ray2.max_dist := sqrt(               {find distance to this light source}
    sqr(ray2.vect.x) + sqr(ray2.vect.y) + sqr(ray2.vect.z));
  m := 1.0/ray2.max_dist;              {make scale factor to unitize vector}
  ray2.vect.x := ray2.vect.x*m;        {make unit vector towards light source}
  ray2.vect.y := ray2.vect.y*m;
  ray2.vect.z := ray2.vect.z*m;
  light_ray;                           {follow ray towards light source}
  if ray2.energy < 0.001 then next;    {light source totally blocked ?}

  light_red := light.pcon_red * ray2.energy; {get light source raw colors}
  light_grn := light.pcon_grn * ray2.energy;
  light_blu := light.pcon_blu * ray2.energy;
  end;
{
*   Point light source with 1/R**2 falloff.
}
type1_ltype_point_r2_k: begin
  ray2.vect.x :=                       {make ununitized vector to light source}
    light.pr2_coor.x - hitp.x;
  ray2.vect.y :=
    light.pr2_coor.y - hitp.y;
  ray2.vect.z :=
    light.pr2_coor.z - hitp.z;
  r :=                                 {square of distance to light source}
    sqr(ray2.vect.x) + sqr(ray2.vect.y) + sqr(ray2.vect.z);
  ray2.max_dist := sqrt(r);            {find distance to light source}
  m := 1.0/ray2.max_dist;              {make scale factor to unitize vector}
  ray2.vect.x := ray2.vect.x*m;        {make unit vector towards light source}
  ray2.vect.y := ray2.vect.y*m;
  ray2.vect.z := ray2.vect.z*m;
  light_ray;                           {follow ray towards light source}
  if ray2.energy < 0.001 then next;    {light source totally blocked ?}

  m := ray2.energy / r;                {scale factor for 1/R**2 falloff}
  light_red := light.pcon_red * m;     {get light source raw colors}
  light_grn := light.pcon_grn * m;
  light_blu := light.pcon_blu * m;
  end;
{
*   Unrecognized light source ID.
}
otherwise
      sys_msg_parm_int (msg_parm[1], ord(light.ltype));
      sys_message_bomb ('ray_type1', 'light_id_bad', msg_parm, 1);
      end;                             {done with all the light source types}
{
*******************
*
*   The current light source is meaningful, and has been reduced to the
*   following state:
*
*     RAY2.VECT  -  Unit vector to light source.
*
*     LIGHT_RED, LIGHT_GRN, LIGHT_BLU  -  Weighted occluded light source color
*       at hit point.  These are the color contributions to the top level ray
*       for the full light at the hit point.
*
*   Now handle the surface properties that are a function of the incoming light.
*   VISPROP is the visual properties block.
*
*   Diffuse reflections.
}
    if visprop.diff_on then begin      {diffuse color turned on ?}
      dot := abs(                      {coupling factor due to incident angle}
        (ray2.vect.x * shnorm.x) +
        (ray2.vect.y * shnorm.y) +
        (ray2.vect.z * shnorm.z));
      color.red := color.red +         {add in diffuse contribution for this light}
        visprop.diff_red * light_red * dot;
      color.grn := color.grn +
        visprop.diff_grn * light_grn * dot;
      color.blu := color.blu +
        visprop.diff_blu * light_blu * dot;
      end;                             {done handling diffuse reflections}
{
*   Specular reflections.
}
    if visprop.spec_on then begin      {specular reflections turned on ?}
      dot :=                           {dot product of light and shading normal}
        (ray2.vect.x * shnorm.x) +
        (ray2.vect.y * shnorm.y) +
        (ray2.vect.z * shnorm.z);
      if dot <= 0.0 then goto done_spec; {light coming from other side of object ?}
      dot :=                           {dot product used for making light refl vect}
        2.0*(ray2.vect.x*shnorm.x + ray2.vect.y*shnorm.y + ray2.vect.z*shnorm.z);
      refl_x :=                        {make unit light reflection vector}
        dot*shnorm.x - ray2.vect.x;
      refl_y :=
        dot*shnorm.y - ray2.vect.y;
      refl_z :=
        dot*shnorm.z - ray2.vect.z;
      dot :=                           {dot product of reflection and view vectors}
        -(refl_x*ray.vect.x + refl_y*ray.vect.y + refl_z*ray.vect.z);
      if dot <= 0.0 then goto done_spec; {negative dot product is like zero}
      dot := dot ** visprop.spec_exp;  {apply specular exponent}
      color.red := color.red +         {add in specular contribution for this light}
        visprop.spec_red * light_red * dot;
      color.grn := color.grn +
        visprop.spec_grn * light_grn * dot;
      color.blu := color.blu +
        visprop.spec_blu * light_blu * dot;
done_spec:                             {jump here to abort specular calculations}
      end;

    end;                               {done with LIGHT abbreviation}
    end;                               {back and do next light source}
{
*******************
*
*   Done looping thru all the light sources.
}
  ray_p := addr(ray);
  ray_p^.max_dist := hit_info.distance;
  end;                                 {done LIPARM, VISPROP, HITP, SHORM abbrevs}

leave:                                 {common exit point}
  if ray.generation <= 1 then begin    {this is a top level ray ?}
    next_mem := 0;                     {reset DAG path scratch memory to all free}
    end;
  end;
