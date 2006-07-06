rem ASF C++ Prototype Build Script: generated Thu Jul 6 15:17:17 AKDT 2006 by make_make.bat.sh
cl /nologo /GR /EHsc /DWIN32=1 src/asf/plugin.cpp src/asf/plugin_loader.cpp src/asf/plugin_execute.cpp src/asf/image.cpp src/asf/clui.cpp src/asf/ddr.cpp src/asf/util.cpp src/asf/meta.cpp src/asf/meta_earth.cpp src/asf/meta_sar.cpp src/pup/pup.cpp src/osl/dll.cpp src/osl/dir.cpp -olib/asf_core.dll /RTC1 /Zi /Yd -I. -I.. -Ibuild/.. -Isrc -DASF_BUILD_COREDLL=1 -DASF_COREDLL_TOPDIR="/home/olawlor/dev/asf/prototype" /LD /link
cl /nologo /GR /EHsc /DWIN32=1 src/asf/clui_cli.cpp lib/asf_core.lib /o bin/clui.exe /RTC1 /Zi /Yd -I. -I.. -Ibuild/.. -Isrc
cl /nologo /GR /EHsc /DWIN32=1 array_push_back.cpp ../lib/asf_core.lib -o../lib/array_push_back.cpp.dll /RTC1 /Zi /Yd -I. -I.. -I../build/.. -I../src /LD /link
cl /nologo /GR /EHsc /DWIN32=1 assert_equal.cpp ../lib/asf_core.lib -o../lib/assert_equal.cpp.dll /RTC1 /Zi /Yd -I. -I.. -I../build/.. -I../src /LD /link
cl /nologo /GR /EHsc /DWIN32=1 echo.cpp ../lib/asf_core.lib -o../lib/echo.cpp.dll /RTC1 /Zi /Yd -I. -I.. -I../build/.. -I../src /LD /link
cl /nologo /GR /EHsc /DWIN32=1 image_blur.cpp ../lib/asf_core.lib -o../lib/image_blur.cpp.dll /RTC1 /Zi /Yd -I. -I.. -I../build/.. -I../src /LD /link
cl /nologo /GR /EHsc /DWIN32=1 image_checksum.cpp ../lib/asf_core.lib -o../lib/image_checksum.cpp.dll /RTC1 /Zi /Yd -I. -I.. -I../build/.. -I../src /LD /link
cl /nologo /GR /EHsc /DWIN32=1 image_clamp.cpp ../lib/asf_core.lib -o../lib/image_clamp.cpp.dll /RTC1 /Zi /Yd -I. -I.. -I../build/.. -I../src /LD /link
cl /nologo /GR /EHsc /DWIN32=1 image_input_jpeg.cpp ../lib/asf_core.lib -ljpeg -o../lib/image_input_jpeg.cpp.dll /RTC1 /Zi /Yd -I. -I.. -I../build/.. -I../src /LD /link
cl /nologo /GR /EHsc /DWIN32=1 image_input_las.cpp ../lib/asf_core.lib -o../lib/image_input_las.cpp.dll /RTC1 /Zi /Yd -I. -I.. -I../build/.. -I../src /LD /link
cl /nologo /GR /EHsc /DWIN32=1 image_median.cpp ../lib/asf_core.lib -o../lib/image_median.cpp.dll /RTC1 /Zi /Yd -I. -I.. -I../build/.. -I../src /LD /link
cl /nologo /GR /EHsc /DWIN32=1 image_output_binary.cpp ../lib/asf_core.lib -o../lib/image_output_binary.cpp.dll /RTC1 /Zi /Yd -I. -I.. -I../build/.. -I../src /LD /link
cl /nologo /GR /EHsc /DWIN32=1 image_output_jpeg.cpp ../lib/asf_core.lib -ljpeg -o../lib/image_output_jpeg.cpp.dll /RTC1 /Zi /Yd -I. -I.. -I../build/.. -I../src /LD /link
cl /nologo /GR /EHsc /DWIN32=1 image_std_dev.cpp ../lib/asf_core.lib -o../lib/image_std_dev.cpp.dll /RTC1 /Zi /Yd -I. -I.. -I../build/.. -I../src /LD /link
cl /nologo /GR /EHsc /DWIN32=1 image_testpattern.cpp ../lib/asf_core.lib -o../lib/image_testpattern.cpp.dll /RTC1 /Zi /Yd -I. -I.. -I../build/.. -I../src /LD /link
cl /nologo /GR /EHsc /DWIN32=1 string.cpp ../lib/asf_core.lib -o../lib/string.cpp.dll /RTC1 /Zi /Yd -I. -I.. -I../build/.. -I../src /LD /link
cl /nologo /GR /EHsc /DWIN32=1 string_out.cpp ../lib/asf_core.lib -o../lib/string_out.cpp.dll /RTC1 /Zi /Yd -I. -I.. -I../build/.. -I../src /LD /link
cl /nologo /GR /EHsc /DWIN32=1 system.cpp ../lib/asf_core.lib -o../lib/system.cpp.dll /RTC1 /Zi /Yd -I. -I.. -I../build/.. -I../src /LD /link
cl /nologo /GR /EHsc /DWIN32=1 textfile_write.cpp ../lib/asf_core.lib -o../lib/textfile_write.cpp.dll /RTC1 /Zi /Yd -I. -I.. -I../build/.. -I../src /LD /link
copy 'lib\asf_coredll.dll' bin
echo "All ASF tools built!  Now cd plugins and run"
echo "    ..\bin\clui.exe image_checksum.test"
