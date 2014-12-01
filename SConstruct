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

# parse and check command line options
release_build = GetOption("release_build")
inst_base = GetOption("prefix")
pkg_version = GetOption("pkg_version")

globalenv = Environment(TOOLS = ["default", add_UnitTest, checkEndian])

source_root = "src"

build_base_dir = "build"

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
    header_dirs = {
        "bins":   os.path.join(GetOption("header_prefix"), "bin"),
        "libs":   os.path.join(GetOption("header_prefix"), "lib"),
        "shares": os.path.join(GetOption("header_prefix"), "share/asf_tools"),
        "mans":   os.path.join(GetOption("header_prefix"), "man"),
        "docs":   os.path.join(GetOption("header_prefix"), "doc"),
    }

# set up various platform-specific things
if platform.system() == "Darwin":
    # the default Fink binary paths
    globalenv.AppendUnique(ENV = {"PATH": ["/sw/bin", "/sw/sbin",]})

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
f.close()

# get all the subdirectories under the source root directory, and make these the default targets
#src_subs = os.walk(source_root).next()[1]
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

src_subs = lib_subs + [
    "add_aux_band",
    "asf_view",
    "mapready",
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
    "metadata_gui",
    "proj2proj",
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
    "rgps2vector"
    ]

# paths where the libraries will be built
if release_build == False:
    build_head_dir = platform.system() + "-" + platform.machine() + "-" + "debug"
else:
    build_type_path = platform.system() + "-" + platform.machine() + "-" + "release"
rpath_link_paths = [os.path.join(build_base_dir, build_head_dir, lib_sub) for lib_sub in lib_subs]

lib_build_paths = [os.path.join("#", rpath_link_path) for rpath_link_path in rpath_link_paths]

# tell the linker where to find the libraries during the link operation
globalenv.AppendUnique(LIBPATH = lib_build_paths)

# common command line options
globalenv.AppendUnique(CCFLAGS = ["-Wall", "-DMAPREADY_VERSION_STRING=\\\"" + pkg_version + "\\\""])
globalenv.AppendUnique(LINKFLAGS = ["-Wl,--as-needed", "-Wl,--no-undefined", "-Wl,-rpath=\\$$ORIGIN/../lib"] + ["-Wl,-rpath-link=" + rpath_link_path for rpath_link_path in rpath_link_paths])

# release type-specific command line options
if release_build == False:
    globalenv.AppendUnique(CCFLAGS = ["-g", "-O0"])
else:
    globalenv.AppendUnique(CCFLAGS = ["-O2"])

# common include directories
globalenv.AppendUnique(CPPPATH = ["."])

# platform-specific include directories
if platform.system() == "Linux":
    globalenv.AppendUnique(CPPPATH = ["/usr/include/libgeotiff",])
elif platform.system() == "Darwin":
    globalenv.AppendUnique(CPPPATH = ["/sw/include",])

# do the actual building
for src_sub in src_subs:
    src_dir = os.path.join(source_root, src_sub)
    # the next line exists because there seems to be no way of getting the source directory from within a tool run from an SConscript with the variant_dir option
    globalenv["src_dir"] = src_dir
    build_dir = os.path.join(build_base_dir, build_head_dir, src_sub)
    globalenv.SConscript(dirs = src_dir, exports = ["globalenv"], variant_dir = build_dir, duplicate = 0)

# configure targets, and make "build" the default
build_subs = [os.path.join("#", build_base_dir, build_head_dir, sub) for sub in src_subs]

globalenv.Alias("build", build_subs)
globalenv.Alias("install", inst_dirs.values())
globalenv.Default("build")
