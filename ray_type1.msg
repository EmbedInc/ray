/*   Error and other messages related to the TYPE1 convention routines
/*   of the ray tracer.
/*
msg light_id_bad
  lan english -def
    Unrecognized light source type ID of %p1i encountered.
msg mem_overflow
  lan english -def
    Ray tracer type 1 routines MEM array overflow.
    MEM is declared in RAY_TYPE1_2.INS.PAS.
/*
/*   Messages used by RAY1 program.
/*
msg ray1_at_line
  lan english -def
    Called from line %p1i of file "%p2s".
msg ray1_at_line_token
  lan english -def
    Error occurred on line %p1i of file "%p2s".  Last token read
    was "%p3s".
msg ray1_cmd_after_include
  lan english -def
    More commands not allowed on same line after INCLUDE command.
msg ray1_comment_comment
  lan english -def
    A command start character was found inside a comment.
msg ray1_integer_missing
  lan english -def
    An integer value was expected but not found.
msg ray1_mand_incr_missing
  lan english -def
    Missing mandatory ABSOLUTE/INCREMENTAL VISPROP option.
msg ray1_name_missing
  lan english -def
    A name was expected but not found.
msg ray1_nesting_too_deep
  lan english -def
    Input file nesting level too deep.
msg ray1_real_missing
  lan english -def
    A real value was expected but not found.
msg ray1_too_many_lights
  lan english -def
    Too many light sources defined.
msg ray1_unrecognized_command
  lan english -def
    Unrecognized command "%p1s".
msg tracing
  lan english -def
    Tracing image for output file "%p1s".
/*
/*   Messages related to TRI (triangle) primitive.
/*
msg tri_not_aggregate
  lan english -def
    Attempt to add child to TYPE1_TRI object.  This object is not an
    aggregate, and therefore can not perform ADD_CHILD operation.
