import os.path

AddOption("--prefix",
          dest = "prefix",
          type = "string",
          nargs = 1,
          action = "store",
          metavar = "DIR",
          help = "installation prefix",
          )

# parse and check command line options
if GetOption("prefix") is None:
    inst_base = "/usr/local"
else:
    inst_base = GetOption("prefix")

globalenv = Environment(TOOLS = ["default", add_UnitTest, checkEndian])

source_root = "src"

# FIXME hard-coded build params, needed until we 
platform = "linux64"
build_type = "debug"

build_base_dir = "build"

inst_dirs = {
    "bins":   os.path.join(inst_base, "bin"),
    "libs":   os.path.join(inst_base, "lib"),
    "shares": os.path.join(inst_base, "share/asf_tools"),
    "mans":   os.path.join(inst_base, "man"),
    "docs":   os.path.join(inst_base, "doc"),
    }
globalenv["inst_dirs"] = inst_dirs

endian = checkEndian(globalenv)

if endian == "little" :
    globalenv.AppendUnique(CPPDEFINES = ["ASF_BIG_ENDIAN", "ASF_BIG_IEEE"])

elif endian == "big" :
    globalenv.AppendUnique(CPPDEFINES = ["ASF_LIL_ENDIAN", "ASF_LIL_IEEE"])

else:
    pass
    # unknown endianess

# create or overwrite include/config.h
# FIXME This is a very ugly way to pass values to the compiler, and once autotools/make goes away, mapready should be edited to remove the need for config.h. These values can then be supplied on the command line to gcc in Scons's CPPDEFINES variable.
f = open("include/config.h", "w")
f.write("#define ASF_SHARE_DIR \"" + inst_dirs["shares"] + "\"\n")
f.write("#define ASF_BIN_DIR \"" + inst_dirs["bins"] + "\"\n")
f.write("#define ASF_DOC_DIR \"" + inst_dirs["docs"] + "\"\n")
f.close()

# common command line options
globalenv.AppendUnique(CCFLAGS = ["-Wall", "-g"])

# common include directories
globalenv.AppendUnique(CPPPATH = ["."])

# get all the subdirectories under the source root directory, and make these the default targets
#src_subs = os.walk(source_root).next()[1]
src_subs = [
    "add_aux_band",
    "asf_view",
    "asf_convert_gui",
    "adjust_bands",
    "asf_import",
    "asf_export",
    "asf_geocode",
    "asf_terrcorr",
    "asf_calpol",
    "asf_calibrate",
    "asf_gamma_import",
    "asf_airsar_import",
    "asf_polsarpro_import",

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
    ]

# do the actual building
for src_sub in src_subs:
    src_dir = os.path.join(source_root, src_sub)
    # the next line exists because there seems to be no way of getting the source directory from within a tool run from an SConscript with the variant_dir option
    globalenv["src_dir"] = src_dir
    build_dir = os.path.join(build_base_dir, platform + "." + build_type, src_sub)
    globalenv.SConscript(dirs = src_dir, exports = ["globalenv"], variant_dir = build_dir, duplicate = 0)

# configure targets, and make "build" the default
build_subs = [os.path.join("#", build_base_dir, platform + "." + build_type, sub) for sub in src_subs]

globalenv.Alias("build", build_subs)
globalenv.Alias("install", inst_dirs.values())
globalenv.Default("build")
