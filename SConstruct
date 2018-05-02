import os.path
import platform

AddOption("--prefix",
          dest = "prefix",
          type = "string",
          nargs = 1,
          action = "store",
          metavar = "DIR",
          help = "The base path where 'scons install' puts things. The config.h header will use this path unless overridden with '--header_prefix'.",
          default = "/usr/local",
          )

# The header_prefix option, when specified, will write the given directory to the config.h header. This is a hack necessary to allow Jenkins to do the RPM build without hard-coding the Jenkins build root path into the Mapready binaries. config.h itself is an artifact of the make/autotools build system, and when that system goes away, config.h can go away, and a cleaner solution can be found.
AddOption("--header_prefix",
          dest = "header_prefix",
          type = "string",
          nargs = 1,
          action = "store",
          metavar = "DIR",
          help = "What prefix to use in the config.h header. Specify this if config.h should have something different from what's given with the '--prefix' option. Typically only Jenkins will need to use this.",
          )

AddOption("--pkg_version",
          dest = "pkg_version",
          type = "string",
          nargs = 1,
          action = "store",
          metavar = "DIR",
          help = "This defines the version string applied to the entire package.",
          default = "UNDEFINED",
          )

AddOption("--release_build",
          dest = "release_build",
          action = "store_true",
          help = "Set to do a release (i.e., not debug) build.",
          default = False,
          )

AddOption("--no_gui",
          dest = "no_gui",
          action = "store_true",
          help = "Don't build the GUI tools.",
          default = False,
          )

# parse and check command line options
release_build = GetOption("release_build")
inst_base = os.path.expanduser(GetOption("prefix"))
pkg_version = GetOption("pkg_version")

globalenv = Environment(TOOLS = ["default", add_UnitTest, checkEndian])

inst_dirs = {
    "bins":   os.path.join(inst_base, "bin"),
    "libs":   os.path.join(inst_base, "lib"),
    "shares": os.path.join(inst_base, "share/asf_tools"),
    "mans":   os.path.join(inst_base, "man"),
    "docs":   os.path.join(inst_base, "doc"),
    }
globalenv["inst_dirs"] = inst_dirs

header_dirs = {}
if GetOption("header_prefix") is None:
    header_dirs = inst_dirs
else:
    header_prefix = os.path.expanduser(GetOption("header_prefix"))
    header_dirs = {
        "bins":   os.path.join(header_prefix, "bin"),
        "libs":   os.path.join(header_prefix, "lib"),
        "shares": os.path.join(header_prefix, "share/asf_tools"),
        "mans":   os.path.join(header_prefix, "man"),
        "docs":   os.path.join(header_prefix, "doc"),
    }

endian = checkEndian(globalenv)

if endian == "big" :
    globalenv.AppendUnique(CPPDEFINES = ["ASF_BIG_ENDIAN", "ASF_BIG_IEEE"])

elif endian == "little" :
    globalenv.AppendUnique(CPPDEFINES = ["ASF_LIL_ENDIAN", "ASF_LIL_IEEE"])

else:
    pass
    # unknown endianess

# create or overwrite include/config.h
# FIXME This is a very ugly way to pass values to the compiler, and once autotools/make goes away, mapready should be edited to remove the need for config.h. These values can then be supplied on the command line to gcc in Scons's CPPDEFINES variable.
f = open("include/config.h", "w")
f.write("#define ASF_SHARE_DIR \"" + header_dirs["shares"] + "\"\n")
f.write("#define ASF_BIN_DIR \"" + header_dirs["bins"] + "\"\n")
f.write("#define ASF_DOC_DIR \"" + header_dirs["docs"] + "\"\n") 
f.write("#define ASF_TMP_DIR \"/tmp\"\n")
f.close()

# List out all the subdirectories we want to build in, with library directories grouped separately.
lib_subs = [
    "libasf_import",
    "libasf_terrcorr",
    "libasf_ardop",
    "libasf_raster",
    "plan",
    "asf",
    "libasf_sar",
    "libasf_geocode",
    "libasf_proj",
    "asf_fft",
    "libasf_vector",
    "libasf_convert",
    "asf_meta",
    "sgpsdp",
    "libasf_export",
    "libasf_metadata",
    "libasf_remap",
]

gui_tool_subs = [
    "asf_view",
    "metadata_gui",
    "mapready",
    "proj2proj",
]

cmdline_tool_subs = [
    "add_aux_band",
    "adjust_bands",
    "asf_import",
    "asf_export",
    "asf_geocode",
    "asf_terrcorr",
    "asf_mapready",
    "asf_calpol",
    "asf_calibrate",
    "asf_gamma_import",
    "asf_airsar_import",
    "asf_polsarpro_import",
    "brs2jpg",
    "diffimage",
    "diffmeta",
    "trim",
    "asf_subset",
    "make_overlay",
    "asf_kml_overlay",
    "sample_plugin",
    "refine_geolocation",
    "shift_geolocation",
    "flip",
    "write_ppf",
    "analyze_yaw",
    "fftMatch",
    "fit_warp",
    "remap",
    "sr2gr",
    "gr2sr",
    "to_sr",
    "deskew",
    "metadata",
    "resample",
    "fill_holes",
    "meta2envi",
    "meta2xml",
    "mosaic",
    "llh2ls",
    "smooth",
    "farcorr",
    "geoid_adjust",
    "update_state",
    "clm",
    "populate_meta_field",
    "stats",
    "byteswap",
    "write_hdf5_xml",
    "asf2geobrowse",
    "sqrt_img",
    "color_browse",
    "cleanup_pixel",
    "annotate_image",
    "convert2vector",
    "change_value",
    "addapole",
    "trim_wedges",
    "point_target_analysis",
    "raster_calc",
    "kernel",
    "asf_logscale",
    "measures2csv",
    "measures2netcdf",
    "measures2geotiff",
    "measures_hdf2csv",
    "rgps2vector",
]

if GetOption("no_gui") == True:
    src_subs = lib_subs + cmdline_tool_subs
else:
    src_subs = lib_subs + cmdline_tool_subs + gui_tool_subs

# paths where the libraries will be built
build_base = os.path.join("build", platform.system() + "-" + platform.machine() + "-")
if release_build == False:
    build_base += "debug"
else:
    build_base += "release"

rpath_link_paths = [os.path.join(build_base, lib_sub) for lib_sub in lib_subs]

lib_build_paths = [os.path.join("#", rpath_link_path) for rpath_link_path in rpath_link_paths]

# common options
globalenv.AppendUnique(LIBPATH = lib_build_paths) # tell the linker where to find the libraries during the link operation
globalenv.AppendUnique(CCFLAGS = ["-Wall", "-DMAPREADY_VERSION_STRING=\\\"" + pkg_version + "\\\""])
globalenv.AppendUnique(CPPPATH = ["."])

# release-specific options
if release_build == False:
    globalenv.AppendUnique(CCFLAGS = ["-g", "-O0"])
else:
    globalenv.AppendUnique(CCFLAGS = ["-O2"])

# platform-specific options
if platform.system() == "Linux":
    globalenv.AppendUnique(LINKFLAGS = ["-Wl,--as-needed", "-Wl,--no-undefined", "-Wl,-rpath=\\$$ORIGIN/../lib",] + ["-Wl,-rpath-link=" + rpath_link_path for rpath_link_path in rpath_link_paths])
    globalenv.AppendUnique(CPPPATH = [
        "/usr/include/libgeotiff",
        "/usr/include/glib-2.0",
        "/usr/lib64/glib-2.0/include",
        "/usr/include/gdal",
        "/usr/include/gtk-2.0",
        "/usr/include/cairo",
        "/usr/include/pango-1.0",
        "/usr/lib64/gtk-2.0/include",
        "/usr/include/gdk-pixbuf-2.0",
        "/usr/include/atk-1.0",
        "/usr/include/libglade-2.0",
        "/usr/include/libxml2",
	"/usr/include/geotiff",
        "/usr/include/hdf5/serial",
        "/usr/lib/x86_64-linux-gnu/glib-2.0/include",
    ])
elif platform.system() == "Darwin":
    globalenv.AppendUnique(CCFLAGS = ["-Ddarwin",])
    # the default Fink binary paths
    globalenv.AppendUnique(ENV = {"PATH": [os.environ["PATH"], "/sw/bin", "/sw/sbin",]})
    # the default Fink library paths
    globalenv.AppendUnique(LIBPATH = ["/sw/lib",])
    globalenv.AppendUnique(CPPPATH = [
        "/sw/include", # Fink
        "/sw/include/glib-2.0", # Fink
        "/sw/lib/glib-2.0/include", # Fink
        "/sw/include/gdal1",
    ])

# do the actual building
for src_sub in src_subs:
    src_dir = os.path.join("src", src_sub)
    build_dir = os.path.join(build_base, src_sub)
    globalenv.SConscript(
        dirs = src_dir,
        exports = ["globalenv"],
        variant_dir = build_dir,
        duplicate = 0,
    )

# configure targets, and make "build" the default
build_subs = [os.path.join("#", build_base, sub) for sub in src_subs]

globalenv.Alias("build", build_subs)
globalenv.Alias("install", inst_dirs.values())
globalenv.Default("build")
