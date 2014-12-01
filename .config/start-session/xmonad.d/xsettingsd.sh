#! /bin/bash

DPI=$(xrandr --current | perl -ne '/connected primary (\d+)x(\d+).* (\d+)mm x (\d+)mm/ && printf "%d\n", 1024*sqrt($1**2+$2**2)*25.4/sqrt($3**2+$4**2)')

file=$HOME/.xsettingsd

if [ ! -f "$file" ]; then
    dump_xsettings | sort > $file
fi

if ! grep -q Xft/DPI $file; then
    echo Xft/DPI $[96*1024] >> $file
fi

if ! grep -q Gdk/UnscaledDPI $file; then
    echo Gdk/UnscaledDPI $[96*1024] >> $file
fi

ed -s $file <<EOF
g,Gdk/UnscaledDPI,d
a
Gdk/UnscaledDPI $DPI
.
g,Xft/DPI,d
a
Xft/DPI $DPI
.
w
q
EOF

xsettingsd &
