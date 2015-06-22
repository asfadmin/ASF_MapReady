#!/usr/bin/env python

# This is a script to test ASF MapReady against reference data that is known to
# be correct. It is run on a directory containing subdirectories which contain
# tests on which MapReady will be run. The results of MapReady are then compared
# against reference data containing '.ref' just before the extension.

import os
import optparse
import subprocess
import logging
import logging.handlers
import shutil
import sys
import ConfigParser
import psycopg2
import traceback
import glob

def main():
        sys.excepthook = log_exceptions
        (args, dirs) = get_arguments()
        (tmpdir, mapready, diffimage, diffmeta, log) =\
                        get_config_options(args.config)
        tmpdir = os.path.abspath(os.path.expandvars(tmpdir))
        mapready = os.path.expandvars(mapready)
        diffimage = os.path.expandvars(diffimage)
        diffmeta = os.path.expandvars(diffmeta)
        if args.asf_tools:
                tool_path = os.path.abspath(os.path.expandvars(args.asf_tools))
                mapready = os.path.join(tool_path, "asf_mapready")
                diffimage = os.path.join(tool_path, "diffimage")
                diffmeta = os.path.join(tool_path, "diffmeta")
        log = os.path.abspath(os.path.expandvars(log))
        logger = logging.getLogger(__name__)
        verbosity = 1
        if args.quiet:
                verbosity = 0
        elif args.debug:
                verbosity = 2
        if verbosity == 2:
                logger.setLevel(logging.DEBUG)
        elif verbosity == 0:
                logger.setLevel(logging.ERROR)
        elif verbosity == 1:
                logger.setLevel(logging.INFO)
        if args.output == "-":
                logger.addHandler(logging.StreamHandler(sys.stdout))
        elif args.output is not None:
                logger.addHandler(logging.FileHandler(args.output))
        else:
                logger.addHandler(logging.handlers.SysLogHandler(log))
        if len(dirs) == 1:
                workdir = os.path.abspath(dirs[0])
        elif len(dirs) == 0:
                workdir = os.getcwd()
        logger.debug("generate_references is {0}".format(
                        args.generate_references))
        logger.debug("tmpdir = {0}".format(tmpdir))
        logger.debug("workdir = {0}".format(workdir))
        if os.path.exists(tmpdir):
                logger.warn("directory {0} exists, deleting contents".format(
                                tmpdir))
                shutil.rmtree(tmpdir)
        logger.debug("creating directory {0}".format(tmpdir))
        os.mkdir(tmpdir)
        db_results = get_db(args.config)
        conn = None
        table = None
        if db_results:
                (conn, table) = db_results
        failures = display_results(autoregress(workdir, tmpdir,
                        not args.noclean, args.generate_references,
                        (mapready, diffimage, diffmeta), conn, args.asf_tools,
                        table))
        if conn:
                conn.close()
        if not args.noclean:
                logger.debug("cleaning up")
                os.rmdir(tmpdir)
        sys.exit(failures)

def get_arguments():
        """Get and return the command-line arguments."""
        # This string really is terrible, but I can't think of a better
        # solution.
        epilog = u"  DIR                   The directory in which to operate (defaults to the    \u2063                        current directory)"
        usage = "Usage: autoregress.py [OPTION]... [DIR]"
        description = "Test the functionality of ASF MapReady against reference data."
        parser = optparse.OptionParser(usage, description = description,
                        epilog = epilog)
        parser.add_option("-v", "--debug", help = "Print all the information",
                        action = "store_true")
        parser.add_option("-q", "--quiet", help = "Print only errors",
                        action = "store_true")
        parser.add_option("-o", "--output", help = "File to which to direct output. Use - for stdout (defaults to system log)")
        parser.add_option("-c", "--config", help = "Configuration file (defaults to $HOME/.config/autoregress.conf)")
        parser.add_option("--generate-references", help = "Generate reference files for autoregress using a version of MapReady known to be working",
                        action="store_true")
        parser.add_option("--noclean", help = "Do not remove the working directory. Only use this option if performing a single test.", action = "store_true")
        parser.add_option("--asf-tools", help = "Directory containing asf_mapready, diffimage, and diffmeta")
        (args, dirs) = parser.parse_args()
        if args.debug and args.quiet:
                parser.error("argument --debug not allowed with argument --quiet")
        if len(dirs) > 1:
                parser.error("Option not allowed: {0}".format(dirs[1]))
        return (args, dirs)

def get_config(config_file):
        """Return the configuration file.

        Search through $HOME/.config/autoregress.conf and /etc/autoregress.conf
        to find the configuration file, and return the name of the file.
        """
        logger = logging.getLogger(__name__)
        if not config_file:
                if os.path.isfile(os.path.join(
                                os.getcwd(), "autoregress.conf")):
                        config_file = os.path.join(
                                        os.getcwd(), "autoregress.conf")
                if os.path.isfile(os.path.expandvars(
                                "$HOME/.config/autoregress.conf")):
                        config_file = os.path.expandvars(
                                        "$HOME/.config/autoregress.conf")
                elif os.path.isfile("/etc/autoregress.conf"):
                        config_file = "/etc/autoregress.conf"
        if config_file is None or not os.path.isfile(config_file):
                logger.warn("Configuration file not found.")
                return None
        return config_file

def get_config_options(config_file):
        """Return configuration file options

        If the configuration file does not exist or the values are not set in
        the configuration file, return default values for these variables.
        """
        tmpdir = os.path.join(os.getcwd(), "autoregress")
        mapready = "asf_mapready"
        diffimage = "diffimage"
        diffmeta = "diffmeta"
        log = "/dev/log"
        config_file = get_config(config_file)
        if not config_file:
                return (tmpdir, mapready, diffimage, diffmeta, log)
        parser = ConfigParser.SafeConfigParser()
        parser.read(config_file)
        if parser.has_section("asf_tools"):
                if parser.has_option("asf_tools", "asf_tools_directory"):
                        tools_directory = parser.get("asf_tools",
                                        "asf_tools_directory")
                        mapready = os.path.join(tools_directory, "mapready")
                        diffimage = os.path.join(tools_directory, "diffimage")
                        diffmeta = os.path.join(tools_direcotry, "diffmeta")
                if parser.has_option("asf_tools", "asf_mapready"):
                        mapready = parser.get("asf_tools", "asf_mapready")
                if parser.has_option("asf_tools", "diffimage"):
                        diffimage = parser.get("asf_tools", "diffimage")
                if parser.has_option("asf_tools", "diffmeta"):
                        diffmeta = parser.get("asf_tools", "diffmeta")
        if parser.has_section("options"):
                if parser.has_option("options", "tmpdir"):
                        tmpdir = parser.get("options", "tmpdir")
                if parser.has_option("options", "log"):
                        log = parser.get("options", "log")
        return (tmpdir, mapready, diffimage, diffmeta, log)

def get_db(config):
        """Return the connection to the database.

        Given the name of the configuration file which specifies how to connect
        to the database, connect and return the connection.
        """
        logger = logging.getLogger(__name__)
        config = get_config(config)
        if not config:
                return None
        parser = ConfigParser.SafeConfigParser()
        parser.read(config)
        if not parser.has_section("postgres"):
                return None
        connect = "host='{0}' dbname='{1}' user='{2}' password='{3}'".format(
                        parser.get("postgres", "host"),
                        parser.get("postgres", "db"),
                        parser.get("postgres", "user"),
                        parser.get("postgres", "password"))
        if not parser.has_option("postgres", "table"):
                table = "autoregress_results"
        else:
                table = parser.get("postgres", "table")
        return (psycopg2.connect(connect), table)

def display_results(results):
        """Log the results of the tests.

        Given a list of booleans corresponding to whether a specific test
        succeeded or failed, log the results in a human-readable format and then
        return the number of failures
        """
        logger = logging.getLogger(__name__)
        successes = results.count(True)
        failures = results.count(False)
        logger.info("{0} tests succeeded and {1} tests failed".format(successes,
                        failures))
        return failures

def autoregress(workdir, tmpdir, clean, gen_refs, tools, db, tools_dir, table):
        """Recurse into workdir and perform tests there.

        In workdir, link all files into tmpdir, and then perform tests on them,
        then enter all subdirectories and do the same. Finally return the
        results of the tests in a list. tools is a tuple containing the paths to
        mapready, diffimage, and diffmeta, and db is the database cursor.
        """
        logger = logging.getLogger(__name__)
        dirname = os.path.basename(workdir)
        logger.debug("In '{0}'.".format(workdir))
        link_data(workdir, tmpdir)
        pre_files = os.listdir(tmpdir)
        for content in os.listdir(tmpdir):
                examine(os.path.join(tmpdir, content), tools, tools_dir)
        post_files = os.listdir(tmpdir)
        diff_files = list(set(post_files) - set(pre_files))
        logger.debug("Differing files:\n{0}".format(diff_files))
        results = []
        for diff_file in diff_files:
                full_path = os.path.join(tmpdir, diff_file)
                if not gen_refs:
                        results.append(test(full_path, tools))
                elif not os.path.isdir(full_path):
                        (name, ext) = os.path.splitext(full_path)
                        name = os.path.basename(name)
                        new_path = os.path.join(workdir, name + ".ref" + ext)
                        logger.debug("moving {0} to {1}".format(
                                        full_path, new_path))
                        os.rename(full_path, new_path)
        if results != []:
                results = [all(v != False for v in results)]
                # This assumes that the directory we are in ends with
                # autoregress_testsuite_[NUM]/testcase[NUM].
                testsuitelen = len("autoregress_testsuite_")
                testcaselen = len("testcase")
                logger.debug("testsuite is {1} and testcase is {2}".format(
                                workdir,
                                os.path.basename(
                                os.path.dirname(workdir))[testsuitelen:],
                                os.path.basename(workdir)[testcaselen:]))
        if db and not gen_refs and results != []:
                suite = os.path.basename(
                os.path.dirname(workdir))[testsuitelen:]
                case = os.path.basename(workdir)[testcaselen:]
                vraw = subprocess.Popen([tools[0], "--version"],
                                stdout=subprocess.PIPE).communicate()[0] 
                vindex = vraw.index("part of MapReady") + 17
                endindex = vraw.index("\n", vindex)
                version = vraw[vindex:endindex]
                with db.cursor() as cur:
                        cur.execute("INSERT INTO {0} ( \
                                        time, version, testsuite, testcase, \
                                        result) \
                                        VALUES ( \
                                        LOCALTIMESTAMP, '{1}', {2}, {3}, \
                                        {4});".format(table, version, suite,
                                        case, results[0]))
                        db.commit()
        if clean:
                for thing in os.listdir(tmpdir):
                        if os.path.isdir(os.path.join(tmpdir, thing)):
                                shutil.rmtree(os.path.join(tmpdir, thing))
                        else:
                                os.remove(os.path.join(tmpdir, thing))
        for content in os.listdir(workdir):
                c = os.path.join(workdir, content)
                if (os.path.isdir(c) and
                                not os.path.samefile(tmpdir, c) and
                                (content.startswith("testcase") or
                                content.startswith("autoregress_testsuite_"))):
                        results.extend(autoregress(c, tmpdir, clean, gen_refs,
                                        tools, db, tools_dir, table))
        return results

# The smart way to write this would be to parse the configuration file and
# decide which data are needed, however it works just fine to link all the data,
# and presumably there is very little performance decrease because symbolic
# links are cheap.
def link_data(workdir, tmpdir):
        """Link all necessary files into tmpdir.

        We will link every single file from workdir into tmpdir, but then also
        link all the files from workdir/../dataset and workdir/../projections,
        because that's the way the old system had the data organized.
        """
        logger = logging.getLogger(__name__)
        updir = os.path.dirname(workdir)
        dataset = os.path.join(updir, "dataset")
        projections = os.path.join(updir, "projections")
        files = [os.path.join(workdir, c) for c in os.listdir(workdir)]
        if os.path.isdir(dataset):
                files.extend([os.path.join(dataset, c)
                                for c in os.listdir(dataset)])
        if os.path.isdir(projections):
                files.extend([os.path.join(projections, c)
                                for c in os.listdir(projections)])
        for content in files:
                logger.debug("examining {0}".format(content))
                if not os.path.isdir(content):
                        basename = os.path.basename(content)
                        newpath = os.path.join(tmpdir, basename)
                        try:
                                os.symlink(content, newpath)
                        except OSError:
                                logger.warn("Could not symlink " + content)

def examine(content, tools, tools_dir):
        """Decide what to do to content and then do it.

        Either call asf_mapready on a configuration file, or execute a script.
        The location of asf_mapready is specified by tools[0].
        """
        logger = logging.getLogger(__name__)
        logger.debug("Calling examine() on {0}".format(content))
        extension = os.path.splitext(content)[1]
        if extension == ".cfg" or extension == ".config":
                logger.debug("Calling {0} on {1}".format(tools[0], content))
                with open(os.devnull, "w") as dev_null:
                        subprocess.call([tools[0], content], stdout = dev_null,
                                        cwd = os.path.dirname(content))
        elif ("script" in subprocess.Popen(["file", os.path.realpath(content)],
                        stdout=subprocess.PIPE).communicate()[0] and
                        os.access(content, os.X_OK)):
                logger.debug("Executing {0}".format(content))
                with open(os.devnull, "w") as dev_null:
                        path = os.path.expandvars("$PATH")
                        new_path = path
                        if tools_dir:
                                new_path = tools_dir + ":" + path
                        subprocess.call(["/usr/bin/env", "PATH=" + new_path,
                                        content],
                                        stdout = dev_null,
                                        cwd = os.path.dirname(content))

def test(gen_file, tools):
        """Test the new files using diffimage or diffmeta.

        Given a single file, determine whether a reference file exists, and if
        it does, then call either diffimage or diffmeta, and return whether the
        test succeeded, or None if there was no reference file. The locations of
        diffimage and diffmeta are specified by tools[1] and tools[2]
        respectively.
        """
        logger = logging.getLogger(__name__)
        (name, extension) = os.path.splitext(gen_file)
        program = ""
        if "image" in subprocess.Popen(["file", gen_file],
                        stdout=subprocess.PIPE).communicate()[0]:
                program = tools[1]
        elif extension == ".meta":
                program = tools[2]
        else:
                return None
        logger.debug("Calling {0} on {1}".format(program, gen_file))
        with open(os.devnull, "w") as dev_null:
                subprocess.call([program, "-output", gen_file + ".diff",
                                name + ".ref" + extension, gen_file], stdout =
                                dev_null, cwd = os.path.dirname(gen_file))
        if os.path.isfile(gen_file + ".diff"):
                with open(gen_file + ".diff") as diff:
                        if len(diff.read()) == 0:
                                logger.info("'{0} {1} {2}' in directory '{3}' succeeded".format(program, name + ".ref" + extension, gen_file, 
                                                os.path.dirname(
                                                os.path.realpath(name + ".ref" +
                                                extension))))
                                return True
                        else:
                                logger.info("'{0} {1} {2}' in directory '{3}' failed!".format(program, name + ".ref" + extension, gen_file, os.path.dirname(
                                                os.path.realpath(name + ".ref" +
                                                extension))))
                                return False
        return None

def log_exceptions(ex_cls, ex, tb):
        logger = logging.getLogger(__name__)
        logger.critical("".join(traceback.format_tb(tb)))
        logger.critical("{0}: {1}".format(ex_cls, ex))

if __name__ == "__main__":
        main()

