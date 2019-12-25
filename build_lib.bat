@echo off
rem
rem   BUILD_LIB [-dbg]
rem
rem   Build the RAY library.
rem
setlocal
call build_pasinit

call src_insall %srcdir% %libname%
call src_insall %srcdir% %libname%_type1

call src_pas %srcdir% %libname%_close %1
call src_pas %srcdir% %libname%_comblock %1
call src_pas %srcdir% %libname%_init %1
call src_pas %srcdir% %libname%_mem %1
call src_pas %srcdir% %libname%_trace %1

rem call src_pas %srcdir% type1_3dfield %1
call src_pas %srcdir% type1_comblock %1
rem call src_pas %srcdir% type1_list %1
call src_pas %srcdir% type1_octree %1
rem call src_pas %srcdir% type1_octree_data %1
call src_pas %srcdir% type1_shader_fixed %1
call src_pas %srcdir% type1_shader_phong %1
call src_pas %srcdir% type1_sphere %1
call src_pas %srcdir% type1_tri %1

call src_lib %srcdir% %libname%

copya %libname%_type1.ins.pas (cog)lib/%libname%_type1.ins.pas
copya %libname%_type1.h (cog)lib/%libname%_type1.h

call src_msg %srcdir% %libname%
call src_msg %srcdir% %libname%_type1
