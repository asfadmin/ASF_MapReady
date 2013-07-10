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

env = Environment(TOOLS = ["default", add_UnitTest, checkEndian])

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

endian = checkEndian(env)

if endian == "little" :
    env.AppendUnique(CPPDEFINES = ["ASF_BIG_ENDIAN", "ASF_BIG_IEEE"])

elif endian == "big" :
    env.AppendUnique(CPPDEFINES = ["ASF_LIL_ENDIAN", "ASF_LIL_IEEE"])

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
env.AppendUnique(CCFLAGS = ["-Wall", "-g"])
env.AppendUnique(LIBPATH = [inst_dirs["libs"]])

# common include directories
env.AppendUnique(CPPPATH = ["."])

# get all the subdirectories under the source root directory
src_subs = os.walk(source_root).next()[1]

# initialize holding dictionary for the build output nodes
output = {}
for product_type in inst_dirs:
    output[product_type] = []

# do the actual building
for src_sub in src_subs:
    src_dir = os.path.join(source_root, src_sub)
    # the next line exists because there seems to be no way of getting the source directory from within a tool run from an SConscript with the variant_dir option
    env["src_dir"] = src_dir
    build_dir = os.path.join(build_base_dir, platform + "." + build_type, src_sub)
    results = SConscript(dirs = src_dir, exports = ["env"], variant_dir = build_dir, duplicate = 0)

    # continue from the top if results is empty (e.g., missing SConscript)
    if results is None: continue

    # fill output with the various build products
    for product_type in results:
        output[product_type].extend(results[product_type])
        Alias("build", results[product_type])

# configure install target
Alias("install", inst_dirs.values())
for product_type in inst_dirs:
    Install(inst_dirs[product_type], output[product_type])
