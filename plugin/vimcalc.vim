"AUTHOR:   Greg Sexton <gregsexton@gmail.com>
"WEBSITE:  https://github.com/gregsexton/VimCalc
"VERSION:  1.3, for Vim 7.0+
"LICENSE:  Same terms as Vim itself (see :help license).

if exists('g:loaded_vimcalc') || v:version < 700
  finish
endif
let g:loaded_vimcalc = 1

command! -nargs=0 -bar Calc call vimcalc#VCalc_Open()

