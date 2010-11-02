"configurable options
let g:GCalc_Title = "__GCALC__"
let g:GCalc_Prompt = "> "
let g:GCalc_Win_Size = 10

command! -nargs=0 -bar Calc call s:GCalc_Open()

function! s:GCalc_Open()
    "validate
    let valid = <SID>GCalc_ValidateVim()
    if valid == -1
        return
    endif

    "if the window is open, jump to it
    let winnum = bufwinnr(g:GCalc_Title)
    if winnum != -1
        "jump to the existing window
        if winnr() != winnum
            exe winnum . 'wincmd w'
        endif
        return
    endif

    "if the buffer already exists edit otherwise create.
    let bufnum = bufnr(g:GCalc_Title)
    if bufnum == -1
        let wcmd = g:GCalc_Title
    else
        let wcmd = '+buffer' . bufnum
    endif
    exe 'silent! ' . g:GCalc_Win_Size . 'new ' . wcmd

    "set options
    silent! setlocal buftype=nofile
    silent! setlocal nobuflisted
    silent! setlocal noswapfile
    silent! setlocal bufhidden=delete
    silent! setlocal nonumber
    silent! setlocal nowrap
    setlocal filetype=gregcalc

    "set mappings
    nnoremap <buffer> <silent> <CR> :call <SID>GCalc_REPL(0)<CR>
    inoremap <buffer> <silent> <CR> <C-o>:call <SID>GCalc_REPL(1)<CR>

    "don't allow inserting new lines
    nnoremap <buffer> <silent> o :call <SID>GCalc_JumpToPrompt()<CR>
    nnoremap <buffer> <silent> O :call <SID>GCalc_JumpToPrompt()<CR>

    call setline(1, g:GCalc_Prompt)
    startinsert!
endfunction

function! s:GCalc_ValidateVim()
    if has('python') != 1
        echohl WarningMsg | echomsg "GCalc requires the Python interface to be installed." | echohl None
        return -1
    endif

    return 0
endfunction

function! s:GCalc_REPL(continueInsert)

    let expr = getline(".")
    if match(expr, g:GCalc_Prompt) != 0
        return
    else
        let expr = strpart(expr, matchend(expr, g:GCalc_Prompt))
    endif

    exe "python repl(\"" . expr . "\")"

    "TODO: possibly test these returns?
    "let failed = append(line('$'), expr)
    let failed = append(line('$'), g:GCalc_Prompt)

    if a:continueInsert == 1
        call <SID>GCalc_JumpToPrompt()
    endif
endfunction

function! s:GCalc_JumpToPrompt()
    call setpos(".", [0, line('$'), col('$'), 0])
    startinsert!
endfunction

" **********************************************************************************************************
" **** PYTHON **********************************************************************************************
" **********************************************************************************************************

if has('python')

python << EOF

import vim, math, cmath, re

def repl(expr):
    result = str(eval(expr))
    vim.command("call append(line('$'), \"" + result + "\")")

#lexemes

#digit  = [0-9]
#digits = digit+

#uppercase = [A-Z]
#lowercase = [a-z]

#alpha        = (uppercase|lowercase)
#alphanumeric = (alpha|digits)

#hexdigit  = [0-9a-fA-F]
#hexdigits = hexdigit+

#octdigit  = [0-7]
#octdigits = octdigit+

#decnumber   = digits(. digits)?(e[+-]? digits)? TODO: negative numbers?
#hexnumber   = 0xhexdigits  NOTE: hex can only represent unsigned integers
#octalnumber = ??

#whitespace = [\t ]+

#ident = (alpha|_)(alphanumeric|_)*'?

#plus      = '+'
#subtract  = '-'
#multiply  = '*'
#divide    = '/'
#modulo    = '%'
#exponent  = '**'
#lShift    = '<<'
#rShift    = '>>'
#factorial = '!'
#increment = '++'
#decrement = '--'

#unaryOp  = factorial|increment|decrement
#binaryOp = plus|subtract|multiply|divide|modulo|exponent|lShift|rShift
#operator = unaryOp|binaryOp

#lParen    = '('
#rParen    = ')'
#comma     = ','
#assign    = '='
#pAssign   = '+='
#sAssign   = '-='
#mAssign   = '*='
#dAssign   = '/='
#modAssign = '%='
#expAssign = '**='

#delimiters = lParen|rParen|comma|assign|pAssign|sAssign|mAssign|dAssign|modAssign|expAssign

#let = 'let'
#keywords = let

class Token(object):
    def __init__(self, tokenID, attrib):
        self._tokenID = tokenID
        self._attrib = attrib
    def getID(self):
        return self._tokenID
    def getAttrib(self):
        return self._attrib
    ID = property(getID, doc='id')
    attrib = property(getAttrib, doc='attrib')

class Lexeme(object):
    def __init__(self, identifier, regex):
        self._ID = identifier
        self._regex = regex
    def getID(self):
        return self._ID
    def getRegex(self):
        return self._regex
    ID = property(getID, doc='id')
    regex = property(getRegex, doc='regex')

#language lexemes
lexemes = [Lexeme('whitespace', r'\s+'),
           Lexeme('decnumber',  r'[0-9]+(\.[0-9]+)?(e[+-]?[0-9]+)?'),
           Lexeme('hexnumber',  r'0x[0-9a-fA-F]+'),
           Lexeme('let',        r'let'),
           Lexeme('ident',      r"[A-Za-z_][A-Za-z0-9_]*'?"),
           Lexeme('expAssign',  r'\*\*='),
           Lexeme('modAssign',  r'%='),
           Lexeme('dAssign',    r'/='),
           Lexeme('mAssign',    r'\*='),
           Lexeme('sAssign',    r'-='),
           Lexeme('pAssign',    r'\+='),
           Lexeme('decrement',  r'--'),
           Lexeme('increment',  r'\+\+'),
           Lexeme('lShift',     r'<<'),
           Lexeme('rShift',     r'>>'),
           Lexeme('exponent',   r'\*\*'),
           Lexeme('assign',     r'='),
           Lexeme('comma',      r','),
           Lexeme('lParen',     r'\('),
           Lexeme('rParen',     r'\)'),
           Lexeme('factorial',  r'!'),
           Lexeme('modulo',     r'%'),
           Lexeme('divide',     r'/'),
           Lexeme('multiply',   r'\*'),
           Lexeme('subtract',   r'-'),
           Lexeme('plus',       r'\+')]

#takes an expression and uses the language lexemes
#to produce a sequence of tokens 
def tokenize(expr):  #TODO: error handle
    tokens = []
    while expr != "":
        for lexeme in lexemes:
            match = matchesFront(lexeme.regex, expr)
            if match != "":
                tokens.append(Token(lexeme.ID, match))
                expr = expr[len(match):]
                break
    return tokens

#returns the match if regex matches beginning of string
#otherwise returns the emtpy string
def matchesFront(regex, string):
    rexp = re.compile(regex)
    m = rexp.match(string)
    if m:
        return m.group()
    else:
        return ""

#useful for testing with map(...)
def getAttrib(token):
    return token.attrib

def getID(token):
    return token.ID

EOF

endif
