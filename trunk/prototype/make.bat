rem ASF C++ Prototype Build Script: generated Sun Jul 9 01:09:24 AKDT 2006 by make_make.bat.sh
del /Q bin\*
del /Q lib\*
cl /nologo /EHsc /GR /MD /DWIN32=1 src/asf/plugin.cpp src/asf/plugin_loader.cpp src/asf/plugin_execute.cpp src/asf/image.cpp src/asf/clui.cpp src/asf/ddr.cpp src/asf/util.cpp src/asf/meta.cpp src/asf/meta_earth.cpp src/asf/meta_sar.cpp src/pup/pup.cpp src/osl/dll.cpp src/osl/dir.cpp -olib/asf_core.dll /O2 /Ox /GB -I. -I.. -Ibuild/.. -Isrc -DASF_BUILD_COREDLL=1 /LD /link
cl /nologo /EHsc /GR /MD /DWIN32=1 src/asf/clui_cli.cpp lib/asf_core.lib /o bin/clui.exe /O2 /Ox /GB -I. -I.. -Ibuild/.. -Isrc
cd plugins
cl /nologo /EHsc /GR /MD /DWIN32=1 array_push_back.cpp ../lib/asf_core.lib -o../lib/array_push_back.cpp.dll /O2 /Ox /GB -I. -I.. -I../build/.. -I../src /LD /link
cl /nologo /EHsc /GR /MD /DWIN32=1 assert_equal.cpp ../lib/asf_core.lib -o../lib/assert_equal.cpp.dll /O2 /Ox /GB -I. -I.. -I../build/.. -I../src /LD /link
cl /nologo /EHsc /GR /MD /DWIN32=1 echo.cpp ../lib/asf_core.lib -o../lib/echo.cpp.dll /O2 /Ox /GB -I. -I.. -I../build/.. -I../src /LD /link
cl /nologo /EHsc /GR /MD /DWIN32=1 image_blur.cpp ../lib/asf_core.lib -o../lib/image_blur.cpp.dll /O2 /Ox /GB -I. -I.. -I../build/.. -I../src /LD /link
cl /nologo /EHsc /GR /MD /DWIN32=1 image_checksum.cpp ../lib/asf_core.lib -o../lib/image_checksum.cpp.dll /O2 /Ox /GB -I. -I.. -I../build/.. -I../src /LD /link
cl /nologo /EHsc /GR /MD /DWIN32=1 image_clamp.cpp ../lib/asf_core.lib -o../lib/image_clamp.cpp.dll /O2 /Ox /GB -I. -I.. -I../build/.. -I../src /LD /link
cl /nologo /EHsc /GR /MD /DWIN32=1 image_input_jpeg.cpp ../lib/asf_core.lib -ljpeg -o../lib/image_input_jpeg.cpp.dll /O2 /Ox /GB -I. -I.. -I../build/.. -I../src /LD /link
cl /nologo /EHsc /GR /MD /DWIN32=1 image_input_las.cpp ../lib/asf_core.lib -o../lib/image_input_las.cpp.dll /O2 /Ox /GB -I. -I.. -I../build/.. -I../src /LD /link
cl /nologo /EHsc /GR /MD /DWIN32=1 image_median.cpp ../lib/asf_core.lib -o../lib/image_median.cpp.dll /O2 /Ox /GB -I. -I.. -I../build/.. -I../src /LD /link
cl /nologo /EHsc /GR /MD /DWIN32=1 image_output_binary.cpp ../lib/asf_core.lib -o../lib/image_output_binary.cpp.dll /O2 /Ox /GB -I. -I.. -I../build/.. -I../src /LD /link
cl /nologo /EHsc /GR /MD /DWIN32=1 image_output_jpeg.cpp ../lib/asf_core.lib -ljpeg -o../lib/image_output_jpeg.cpp.dll /O2 /Ox /GB -I. -I.. -I../build/.. -I../src /LD /link
cl /nologo /EHsc /GR /MD /DWIN32=1 image_std_dev.cpp ../lib/asf_core.lib -o../lib/image_std_dev.cpp.dll /O2 /Ox /GB -I. -I.. -I../build/.. -I../src /LD /link
cl /nologo /EHsc /GR /MD /DWIN32=1 image_testpattern.cpp ../lib/asf_core.lib -o../lib/image_testpattern.cpp.dll /O2 /Ox /GB -I. -I.. -I../build/.. -I../src /LD /link
cl /nologo /EHsc /GR /MD /DWIN32=1 string.cpp ../lib/asf_core.lib -o../lib/string.cpp.dll /O2 /Ox /GB -I. -I.. -I../build/.. -I../src /LD /link
cl /nologo /EHsc /GR /MD /DWIN32=1 string_out.cpp ../lib/asf_core.lib -o../lib/string_out.cpp.dll /O2 /Ox /GB -I. -I.. -I../build/.. -I../src /LD /link
cl /nologo /EHsc /GR /MD /DWIN32=1 system.cpp ../lib/asf_core.lib -o../lib/system.cpp.dll /O2 /Ox /GB -I. -I.. -I../build/.. -I../src /LD /link
cl /nologo /EHsc /GR /MD /DWIN32=1 textfile_write.cpp ../lib/asf_core.lib -o../lib/textfile_write.cpp.dll /O2 /Ox /GB -I. -I.. -I../build/.. -I../src /LD /link
cd ..
del *.obj
copy lib\asf_core.dll bin
echo "All ASF tools built!  Now cd plugins and run"
echo "    ..\bin\clui.exe image_checksum.test"
