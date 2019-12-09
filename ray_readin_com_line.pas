{   Subroutine RAY_READIN_COM_LINE (COMLIN)
*
*   Read in the command line and pass back the resulting information.
*   The command line has the following format:
*
*   <file name>
*
*     Mandatory first argument that is the name of the ray tracer input file.
*
*   -SIZE dx dy
*
*     Set final image output size in pixels.
*
*   -AA
*
*     Indicate that the output image is to be anti-aliased.
*
*   -NO_AA
*
*     Turn off anti-aliasing.
*
*   -ASPECT x_size y_size
*
*     Indicate aspect ratio of final output image.  The default is square pixels.
}
module ray_readin_com_line;
define ray_readin_com_line;
%include 'ray2.ins.pas';

procedure ray_readin_com_line (        {read command line and pass back result}
  out     comlin: ray_comlin_t);       {data about command line}
  val_param;

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  r1, r2: real;                        {scratch floating point numbers}
  opt: string_var16_t;                 {command line option name}
  parm: string_var80_t;                {command line option parameter}
  pick: sys_int_machine_t;             {number of option picked from list}
  msg_parm:                            {message parameter references}
    array[1..max_msg_parms] of sys_parm_msg_t;
  stat: sys_err_t;                     {error status code}

label
  next_opt, done_opts;

begin
  comlin.fnam_in.max := sizeof(comlin.fnam_in.str); {init var string}
  opt.max := sizeof(opt.str);
  parm.max := sizeof(parm.str);
{
*   Init to defaults before processing command line.
}
  comlin.size_x := 512;
  comlin.size_y := 410;
  comlin.size_set := false;
  comlin.aa := false;
  comlin.aa_set := false;
  comlin.aspect_set := false;

  string_cmline_init;                  {init command line parsing}
  string_cmline_token (comlin.fnam_in, stat); {get input file name from command line}
  string_cmline_req_check (stat);      {input file name is mandatory}
{
*   Process the command line options.  Come back here each new command line
*   option.
}
next_opt:
  string_cmline_token (opt, stat);     {read new command line option}
  if string_eos(stat) then goto done_opts; {nothing left on command line ?}
  sys_error_abort (stat, 'string', 'cmline_opt_err', nil, 0);
  string_upcase (opt);                 {make upper case for token matching}
  string_tkpick80 (                    {pick option name from list}
    opt,                               {option name}
    '-SIZE -AA -NO_AA -ASPECT',        {valid option names}
    pick);                             {number of picked option}
  case pick of                         {do routine for specific option}
{
*   Command line option:
*   -SIZE dx dy
}
1: begin
  string_cmline_token_int (comlin.size_x, stat);
  string_cmline_parm_check (stat, opt);
  string_cmline_token_int (comlin.size_y, stat);
  comlin.size_set := true;
  end;
{
*   Command line option:
*   -AA
}
2: begin
  comlin.aa := true;
  comlin.aa_set := true;
  end;
{
*   Command line option:
*   -NO_AA
}
3: begin
  comlin.aa := false;
  comlin.aa_set := true;
  end;
{
*   Command line option:
*   -ASPECT x_size y_size
}
4: begin
  string_cmline_token_fpm (r1, stat);
  string_cmline_parm_check (stat, opt);
  string_cmline_token_fpm (r2, stat);
  comlin.aspect := r1 / r2;
  comlin.aspect_set := true;
  end;
{
*   Unrecognized command line option.
}
otherwise
    sys_msg_parm_vstr (msg_parm[1], opt);
    sys_message_bomb ('string', 'cmline_opt_bad', msg_parm, 1);
    end;                               {done with command line option cases}

  string_cmline_parm_check (stat, opt);
  goto next_opt;
done_opts:                             {done with all the command line options}

  if not comlin.aspect_set then begin  {use default aspect ratio ?}
    comlin.aspect := comlin.size_x / comlin.size_y;
    end;
  end;
