#!/usr/local/bin/bash

# A quick and automatic way to look up the definition of things in
# asf_tools.  

# Use exuberant ctags to produce an emacs compatible tags table for
# the libraries in asf_tools.  This will produce a file called TAGS in
# this directory.  You can use this tag file from emacs with the
# visit-tags-table command.  If you put (visit-tags-table
# "/wherever_asf_tools_is_these_days/asf_tools/") in your ~/.emacs,
# you will always have the asf library functions in your emacs tags
# table.

# Now you can do M-. from emacs while the point is over some variable,
# function name, type, or macro that you are curious about and emacs
# will pop you into a buffer with the point on the definition of the
# thing you were wondering about.

# asf_tools is big, there are sure to be some namespace collisions in
# the produced TAGS file.  C-u M-. can be used to cycle through tag
# definitions.

ctags -e --recurse --links=no ./
