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

# Change to True to generate known good files from a reference version of
# MapReady.
gen_refs = False

def main():
	(args, dirs) = get_arguments()
	(clean, tmpdir, mapready, diffimage, diffmeta, log) =\
			get_config_options(args.config)
	tmpdir = os.path.abspath(os.path.expandvars(tmpdir))
	mapready = os.path.expandvars(mapready)
	diffimage = os.path.expandvars(diffimage)
	diffmeta = os.path.expandvars(diffmeta)
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
	elif args.output != None:
		logger.addHandler(logging.FileHandler(args.output))
	else:
		logger.addHandler(logging.handlers.SysLogHandler(log))
	if len(dirs) == 1:
		workdir = os.path.abspath(dirs[0])
	elif len(dirs) == 0:
		workdir = os.getcwd()
	logger.debug("tmpdir = {0}".format(tmpdir))
	logger.debug("workdir = {0}".format(workdir))
	if os.path.exists(tmpdir):
		logger.warn("directory {0} exists, deleting contents".format(
				tmpdir))
		shutil.rmtree(tmpdir)
	logger.debug("creating directory {0}".format(tmpdir))
	os.mkdir(tmpdir)
	conn = get_db(args.config)
	if conn:
		cur = conn.cursor()
	failures = display_results(autoregress(workdir, tmpdir,
			clean, (mapready, diffimage, diffmeta), cur))
	if conn:
		conn.commit()
		cur.close()
		conn.close()
	if clean:
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
		if os.path.isfile(os.path.expandvars(
				"$HOME/.config/autoregress.conf")):
			config_file = os.path.expandvars(
					"$HOME/.config/autoregress.conf")
		elif os.path.isfile("/etc/autoregress.conf"):
			config_file = "/etc/autoregress.conf"
	if config_file == None or not os.path.isfile(config_file):
		logger.warn("Configuration file not found.")
		return None
	return config_file

def get_config_options(config_file):
	"""Return configuration file options

	If the configuration file does not exist or the values are not set in
	the configuration file, return default values for these variables.
	"""
	clean = False
	tmpdir = os.path.join(os.getcwd(), "autoregress")
	mapready = "asf_mapready"
	diffimage = "diffimage"
	diffmeta = "diffmeta"
	log = "/dev/log"
	config_file = get_config(config_file)
	if not config_file:
		return (clean, tmpdir, mapready, diffimage, diffmeta, log)	
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
		if parser.has_option("options", "clean"):
			clean = parser.getboolean("options", "clean")
		if parser.has_option("options", "tmpdir"):
			tmpdir = parser.get("options", "tmpdir")
		if parser.has_option("options", "log"):
			log = parser.get("options", "log")
	return (clean, tmpdir, mapready, diffimage, diffmeta, log)

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
	return psycopg2.connect(connect)

def display_results(results):
	"""Log the results of the tests.

	Given a list of booleans corresponding to whether a specific test
	succeeded or failed, log the results in a human-readable format and then
	return the number of failures
	"""
	logger = logging.getLogger(__name__)
	successes = 0
	failures = 0
	for result in results:
		if result == True:
			successes += 1
		if result == False:
			failures += 1
	logger.info("{0} tests succeeded and {1} tests failed".format(successes,
			failures))
	return failures

def autoregress(workdir, tmpdir, clean, tools, db):
	"""Recurse into workdir and perform tests there.

	In workdir, link all files into tmpdir, and then perform tests on them,
	then enter all subdirectories and do the same. Finally return the
	results of the tests in a list. tools is a tuple containing the paths to
	mapready, diffimage, and diffmeta, and db is the database cursor.
	"""
	logger = logging.getLogger(__name__)
	dirname = os.path.basename(workdir)
	logger.debug("In '{0}'.".format(workdir))
	for content in os.listdir(workdir):
		content_full_path = os.path.join(workdir, content)
		logger.debug("examining {0}".format(content_full_path))
		if not os.path.isdir(content_full_path):
			os.symlink(content_full_path, os.path.join(tmpdir,
					content))
	pre_files = os.listdir(tmpdir)
	for content in os.listdir(tmpdir):
		examine(os.path.join(tmpdir, content), tools)
	post_files = os.listdir(tmpdir)
	diff_files = list(set(post_files) - set(pre_files))
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
		results = [all(results)]
		# This assumes that the directory we are in ends with
		# testsuite_[NUM]/testcase[NUM].
		logger.debug("testsuite is {1} and testcase is {2}".format(
				workdir,
				os.path.basename(os.path.dirname(workdir))[10:],
				os.path.basename(workdir)[8:]))
		if db and not gen_refs:
			suite = os.path.basename(os.path.dirname(workdir))[10:]
			case = os.path.basename(workdir)[8:]
			db.execute("INSERT INTO autoregress_results VALUES (\
					LOCALTIMESTAMP, {0}, {1}, {2})".format(
					suite,
					case,
					results[0]))
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
					clean, tools, db))
	return results

def examine(content, tools):
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
	elif ("script" in subprocess.check_output(["file", os.path.realpath(
			content)]) and os.access(content, os.X_OK)):
		logger.debug("Executing {0}".format(content))
		with open(os.devnull, "w") as dev_null:
			subprocess.call([content], stdout = dev_null, cwd =
					os.path.dirname(content))

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
	if "image" in subprocess.check_output(["file", gen_file]):
		program = tools[1]
	elif extension == ".meta":
		program = tools[2]
	else:
		return None
	logger.debug("Calling {0} on {1}".format(program, gen_file))
	with open(os.devnull, "w") as dev_null:
		subprocess.call([program, "-output", gen_file + ".diff",
				gen_file, name + ".ref" + extension], stdout =
				dev_null)
	if os.path.isfile(gen_file + ".diff"):
		with open(gen_file + ".diff") as diff:
			if len(diff.read()) == 0:
				logger.info("'{0} {1} {2}' in directory '{3}' succeeded".format(program, gen_file, name + ".ref" + extension, os.path.dirname(os.path.realpath(name + ".ref" + extension))))
				return True
			else:
				logger.info("'{0} {1} {2}' in directory '{3}' failed!".format(program, gen_file, name + ".ref" + extension, os.path.dirname(os.path.realpath(name + ".ref" + extension))))
				return False
	return None

if __name__ == "__main__":
	main()

