# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
fi

test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"

##
# Your previous /Users/anr/.bash_profile file was backed up as /Users/anr/.bash_profile.macports-saved_2016-08-29_at_20:39:45
##

# MacPorts Installer addition on 2016-08-29_at_20:39:45: adding an appropriate PATH variable for use with MacPorts.
export PATH="/opt/local/bin:/opt/local/sbin:$PATH"
# Finished adapting your PATH environment variable for use with MacPorts.


##
# Your previous /Users/anr/.bash_profile file was backed up as /Users/anr/.bash_profile.macports-saved_2016-08-29_at_21:40:11
##

# MacPorts Installer addition on 2016-08-29_at_21:40:11: adding an appropriate PATH variable for use with MacPorts.
export PATH="/opt/local/bin:/opt/local/sbin:$PATH"
# Finished adapting your PATH environment variable for use with MacPorts.

