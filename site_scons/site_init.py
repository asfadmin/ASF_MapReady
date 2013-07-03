def _runUnitTest(target, source, env):
    """Function to run a unit test."""

    cur_dir = env["src_dir"]

    if not source:
        sys.stderr.write("Warning: no unit test for " + cur_dir + "\n")
        return 0

    import subprocess
    test_program = str(source[0].abspath)
    if not subprocess.call(test_program):
        open(str(target[0]), "w").write("passed\n")
        return 0
    else:
        sys.stderr.write("Unit test failed for " + cur_dir + "\n")
        return 1

def add_UnitTest(env):
    """Function to make the UnitTest builder."""
    utbuild = Builder(action = _runUnitTest)
    env.Append(BUILDERS = {"UnitTest": utbuild})
    
def checkEndian(env):
    """Endianness test function. Python already knows what endian it is, just ask Python. This code was tested on Intel, AMD 64, and Cell (Playstation 3)."""
    import struct

    array = struct.pack('cccc', '\x01', '\x02', '\x03', '\x04')

    i = struct.unpack('i', array)

    # Little Endian
    if i == struct.unpack('<i', array):
        return "little"

    # Big Endian
    elif i == struct.unpack('>i', array):
        return "big"

    return "unknown"
