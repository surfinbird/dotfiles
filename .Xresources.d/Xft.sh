#! /bin/bash
# Xft.sh - Xft resources                                          -*- sh -*-

xrandr --dpi $REAL_DPI

cat<<EOF
Xft.antialias:	true
Xft.dpi:	REAL_DPI
Xft.hinting:	true
Xft.hintstyle:	hintslight
Xft.rgba:	rgb
EOF

