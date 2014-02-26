def _unit_test_pseudo_builder(env, source, extra_depends, **kwargs):
    """Pseudo builder to execute unit test programs after they're built, and to set their dependencies."""

    test_binary = env.Program("test_binary", source, **kwargs)
    test_result = env.Command(target = "test.semaphore",
                              source = test_binary[0].abspath,
                              action = run_test)

    env.Depends(test_result, extra_depends)

def add_UnitTest(env):
    """Function to make the UnitTest builder."""
    env.AddMethod(_unit_test_pseudo_builder, "UnitTest")

def run_test(target, source, env):
    """Run an external program and get its return code."""

    target_file = target[0].abspath
    test_binary = source[0].abspath

    import subprocess
    retcode = subprocess.call(test_binary)
    
    if retcode == 0:
        open(target_file, "w").write("test passed")
        return 0
    else:
        sys.stderr.write("warning: unit test failed")
        return 1
    
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
