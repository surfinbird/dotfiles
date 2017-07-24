# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.scripts" ] ; then
    export PATH="$HOME/.scripts:$PATH"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    export PATH="$HOME/bin:$PATH"
fi

# set PATH so it includes cargo binaries if they exist
if [ -d "$HOME/.cargo/bin" ] ; then
    export PATH="$HOME/.cargo/bin:$PATH"
fi

# set PATH so it includes macports bin dir if it exists
if [ -d "/opt/local/bin" ] ; then
    export PATH="/opt/local/bin:$PATH"
fi

if [ -d "$HOME/android/sdk/tools/bin" ] ; then
    export PATH="$HOME/android/sdk/tools/bin:$PATH"
fi

if [ -d "$HOME/android/sdk/platform-tools" ] ; then
    export PATH="$HOME/android/sdk/platform-tools:$PATH"
fi

# Set the list of directories that Zsh searches for programs.
path=(
    /usr/local/{bin,sbin}
    $path
)
