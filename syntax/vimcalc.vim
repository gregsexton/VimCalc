" Vim syntax file
" Language:    VimCalc vim plugin
" Maintainer:  Greg Sexton <gregsexton@gmail.com>
" URL:         TODO:
"

if version < 600
    syntax clear
elseif exists("b:current_syntax")
    finish
endif

syntax keyword vcalcAns ans
syntax keyword vcalcE   e
syntax keyword vcalcPi  pi
syntax keyword vcalcPhi phi

syntax keyword vcalcLet let

syntax keyword vcalcFuncs abs acos asin atan atan2 ceil cos cosh deg exp floor hypot inv ldexp lg ln log log10 max min nrt pow rad rand round sin sinh sqrt tan tanh

syntax match vcalcOps "\*\*=\|%=\|/=\|\*=\|-=\|+=\|<<\|>>\|\*\*\|=\|!\|%\|/\|\*\|-\|+"
syntax match vcalcDelim "(\|)"

syntax match vcalcDecNum "[0-9]\+\(\.[0-9]\+\)\?\(e[+-]\?[0-9]\+\)\?"
syntax match vcalcHexNum "0x[0-9a-fA-F]\+"
syntax match vcalcOctNum "0[0-7]\+"

syntax match vcalcSynErr "^Syntax error: .*"
syntax match vcalcParErr "^Parse error: .*"

"TODO: syntax match vcalcPrompt

if version >= 600
	command -nargs=+ HiLink highlight default link <args>
else
	command -nargs=+ HiLink highlight         link <args>
endif

"Special Symbols
HiLink vcalcAns         vcalcSymbol
HiLink vcalcE           vcalcSymbol
HiLink vcalcPi          vcalcSymbol
HiLink vcalcPhi         vcalcSymbol
HiLink vcalcSymbol      Constant

"Keywords
HiLink vcalcLet         vcalcKeyword
HiLink vcalcKeyword     Keyword

HiLink vcalcFuncs       Function
HiLink vcalcOps         Operator
"HiLink vcalcDelim       Delimiter

"Numbers
HiLink vcalcDecNum      vcalcNumber
HiLink vcalcHexNum      vcalcNumber
HiLink vcalcOctNum      vcalcNumber
HiLink vcalcNumber      Number

"Errors
HiLink vcalcSynErr      vcalcError
HiLink vcalcParErr      vcalcError
"HiLink vcalcError       Error

delcommand HiLink

let b:current_syntax = "clojure"
