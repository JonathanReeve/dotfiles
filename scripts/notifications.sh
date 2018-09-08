#!/run/current-system/sw/bin/sh

. "${HOME}/.cache/wal/colors.sh"

pkill dunst
dunst -lb $background -nb $background -cb $color1 \
      -lf $foreground -nf $foreground -cf $color1 \
      -geom 600x80-30+70 -padding 30 -horizontal_padding 30 \
      -word_wrap=true
