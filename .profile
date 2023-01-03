

##
# Your previous /Users/mnp/.profile file was backed up as /Users/mnp/.profile.macports-saved_2020-07-20_at_13:19:51
##

# MacPorts Installer addition on 2020-07-20_at_13:19:51: adding an appropriate PATH variable for use with MacPorts.
#export PATH="/opt/local/bin:/opt/local/sbin:$PATH"
# Finished adapting your PATH environment variable for use with MacPorts.



# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

#. "$HOME/.cargo/env"

