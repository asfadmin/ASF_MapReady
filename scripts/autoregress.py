#!/usr/bin/env python

# This is a script to test ASF MapReady against reference data that is known to
# be correct. It is run on a directory containing subdirectories which contain
# tests on which MapReady will be run. The results of MapReady are then compared
# against reference data containing '.ref' just before the extension.

import os
import argparse
import subprocess
import logging
import logging.handlers
import shutil
import sys

def main():
	parser = argparse.ArgumentParser(description = "Test the functionality of ASF MapReady against reference data.")
	parser.add_argument("--tmpdir", help = "specify the working directory (defaults to autoregress)", default = os.getcwd() + "/autoregress")
	verbose = parser.add_mutually_exclusive_group()
	verbose.add_argument("--debug", help = "Print all the information",
			action = "store_true")
	verbose.add_argument("--quiet", help = "Print only errors",
			action = "store_true")
	parser.add_argument("--clean", help = "Delete all files in work directory when done, regardless of success/fail.", action = "store_true")
	parser.add_argument("--output", help = "Where to direct output. Use - for stdout (defaults to system log)")
	parser.add_argument("dir", help = "Directory containing test data",
			default = os.getcwd(), nargs = "?")
	args = parser.parse_args()

	tmpdir = os.path.abspath(args.tmpdir)
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
	elif args.output != None:
		logger.addHandler(logging.FileHandler(args.output))
	else:
		logger.addHandler(logging.handlers.SysLogHandler("/dev/log"))
	clean = args.clean
	workdir = os.path.abspath(args.dir)
	logger.debug("tmpdir = %s" % tmpdir)
	logger.debug("workdir = %s" % workdir)
	if os.path.exists(tmpdir):
		logger.warn("directory %s exists, deleting contents" % tmpdir)
		shutil.rmtree(tmpdir)
	logger.debug("creating directory %s" % tmpdir)
	os.mkdir(tmpdir)
	failures = display_results(autoregress(workdir, tmpdir, verbosity,
			clean))
	if clean:
		os.rmdir(tmpdir)
	sys.exit(failures)

# Log how many tests succeeded and failed and return the number of failures.
def display_results(results):
	logger = logging.getLogger(__name__)
	successes = 0
	failures = 0
	for result in results:
		if result == True:
			successes += 1
		if result == False:
			failures += 1
	logger.info("%i tests succeeded and %i tests failed" % (successes,
			failures))
	return failures

# Regress into workdir and perform tests there, returning the test results.
def autoregress(workdir, tmpdir, verbosity, clean):
	logger = logging.getLogger(__name__)
	dirname = os.path.basename(workdir)
	logger.debug("In '%s'." % workdir)
	for content in os.listdir(workdir):
		content_full_path = os.path.join(workdir, content)
		logger.debug("examining %s" % content_full_path)
		if not os.path.isdir(content_full_path):
			os.symlink(content_full_path, os.path.join(tmpdir,
					content))
	pre_files = os.listdir(tmpdir)
	for content in os.listdir(tmpdir):
		examine(os.path.join(tmpdir, content), verbosity)
	post_files = os.listdir(tmpdir)
	diff_files = list(set(post_files) - set(pre_files))
	results = []
	for diff_file in diff_files:
		results.append(test(os.path.join(tmpdir, diff_file)))
	if clean:
		for thing in os.listdir(tmpdir):
			if os.path.isdir(os.path.join(tmpdir, thing)):
				shutil.rmtree(os.path.join(tmpdir, thing))
			else:
				os.remove(os.path.join(tmpdir, thing))
	for content in os.listdir(workdir):
		content_full_path = os.path.join(workdir, content)
		if os.path.isdir(content_full_path) and not os.path.samefile(
				tmpdir, content_full_path):
			results.extend(autoregress(content_full_path, tmpdir,
					verbosity, clean))
	return results

# Decide what to do to content and then do it.
def examine(content, verbosity):
	logger = logging.getLogger(__name__)
	logger.debug("Calling examine() on %s" % content)
	extension = os.path.splitext(content)[1]
	if extension == ".cfg" or extension == ".config":
		logger.debug("Calling asf_mapready on %s" % content)
		dev_null = open(os.devnull, "w")
		subprocess.call(["asf_mapready", content], stdout = dev_null,
				cwd = os.path.dirname(content))
		dev_null.close()
	elif ("script" in subprocess.check_output(["file", os.path.realpath(
			content)]) and os.access(content, os.X_OK)):
		logger.debug("Executing %s" % content)
		dev_null = open(os.devnull, "w")
		subprocess.call([content], stdout = dev_null, cwd =
				os.path.dirname(content))
		dev_null.close()

# Test gen_file using diffimage or diffmeta with the corresponding reference
# files, and return whether the test succeeded or None if no test was performed.
def test(gen_file):
	logger = logging.getLogger(__name__)
	name, extension = os.path.splitext(gen_file)
	program = ""
	if "image" in subprocess.check_output(["file", gen_file]):
		program = "diffimage"
	elif extension == ".meta":
		program = "diffmeta"
	else:
		return None
	dev_null = open(os.devnull, "w")
	logger.debug("Calling %s on %s" % (program, gen_file))
	subprocess.call([program, "-output", gen_file + ".diff", gen_file,
			name + ".ref" + extension], stdout = dev_null)
	dev_null.close()
	if os.path.isfile(gen_file + ".diff"):
		diff = open(gen_file + ".diff")
		if len(diff.read()) == 0:
			logger.info("'%s %s %s' in directory '%s' succeeded" %
					(program, gen_file, name + ".ref" +
					extension, os.path.dirname(
					os.path.realpath(name + ".ref" +
					extension))))
			diff.close()
			return True
		else:
			logger.info("'%s %s %s' in directory '%s' failed" %
					(program, gen_file, name + ".ref" +
					extension, os.path.dirname(
					os.path.realpath(name + ".ref" +
					extension))))
			diff.close()
		return False

if __name__ == "__main__":
	main()

