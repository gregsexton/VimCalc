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
    result = str(parse(expr))
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
    ID = property(getID, doc='Token ID [string].')
    attrib = property(getAttrib, doc='Token attribute [string].')

class Lexeme(object):
    def __init__(self, identifier, regex):
        self._ID = identifier
        self._regex = regex
    def getID(self):
        return self._ID
    def getRegex(self):
        return self._regex
    ID = property(getID, doc='Lexeme ID [string].')
    regex = property(getRegex, doc='Regex to match the Lexeme.')

#language lexemes
lexemes = [Lexeme('whitespace', r'\s+'),
           Lexeme('hexnumber',  r'0x[0-9a-fA-F]+'),
           Lexeme('decnumber',  r'[0-9]+(\.[0-9]+)?(e[+-]?[0-9]+)?'),
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
    return filter(lambda t: t.ID != 'whitespace', tokens)

#returns the match if regex matches beginning of string
#otherwise returns the emtpy string
def matchesFront(regex, string):
    rexp = re.compile(regex)
    m = rexp.match(string)
    if m:
        return m.group()
    else:
        return ""

#useful for testing tokenize with map(...)
def getAttrib(token):
    return token.attrib

def getID(token):
    return token.ID

#gcalc context-free grammar

#line    -> expr | assign
#assign  -> let ident = expr
#expr    -> expr + term | expr - term | expr ++ | expr -- | func | term
#func    -> ident ( args )
#args    -> expr , args | expr
#term    -> term * factor | term / factor | term % factor
#           | term << factor | term >> factor | term ! | factor
#factor  -> expt ** factor | expt
#expt    -> number | ident | ( expr )
#number  -> decnumber | hexnumber | octalnumber

class ParseNode(object):
    def __init__(self, success, result, consumedTokens):
        self._success = success
        self._result = result
        self._consumedTokens = consumedTokens
    def getSuccess(self):
        return self._success
    def getResult(self):
        return self._result
    def getConsumed(self):
        return self._consumedTokens
    success = property(getSuccess, doc='Successfully evaluated?')
    result = property(getResult, doc='The evaluated result at this node.')
    consumeCount = property(getConsumed, doc='Number of consumed tokens.')

#recursive descent parser -- simple and befitting the needs of this small program
def parse(expr):
    tokens = tokenize(expr)
    lineNode = line(tokens)
    if lineNode.success:
        return lineNode.result
    else:
        return "Error!"

def line(tokens):
    exprNode = expr(tokens)
    if exprNode.success:
        if exprNode.consumeCount == len(tokens):
            return exprNode
        else
            return ParseNode(False, 0, exprNode.consumeCount)

    assignNode = assign(tokens)
    if assignNode.success:
        if assignNode.consumeCount == len(tokens):
            return assignNode
        else
            return ParseNode(False, 0, assignNode.consumeCount)

    return ParseNode(False, 0, 0)

def assign(tokens):
    if map(getID, tokens[0:3]) == ['let', 'ident', 'assign']:
        exprNode = expr(tokens[3:])
        if exprNode.consumeCount+3 == len(tokens):
            storeSymbol(tokens[1].attrib, exprNode.result)
            return exprNode
        else:
            return ParseNode(False, 0, exprNode.consumeCount+3)
    else:
        return ParseNode(False, 0, 0)

def expr(tokens):
    None

def func(tokens):
    if map(getID, tokens[0:2]) == ['ident', 'lParen']:
        argsNode = args(tokens[2:])
        if tokens[argsNode.consumeCount+2] == 'rParen':
            return ParseNode(True, 'TODO', argsNode.consumeCount+3)
        else
            return ParseNode(False, 0, argsNode.consumeCount+2)
    else:
        return ParseNode(False, 0, 0)

def args(tokens):
    None

def term(tokens):
    None

def factor(tokens):
    None

def expt(tokens):
    token = tokens[0]
    if token.ID == 'ident':
        return ParseNode(True, lookupSymbol(token.attrib), 1)

    numberNode = number(tokens)
    if numberNode.success:
        return numberNode

    if token.ID == 'lParen':
        exprNode = expr(tokens[1:])
        if exprNode.success:
            if tokens[exprNode.consumeCount+1] == 'rParen':
                return ParseNode(True, exprNode.result, exprNode.consumeCount+2)
    
    return ParseNode(False, 0, 0)

def number(tokens):
    token = tokens[0]
    if token.ID == 'decnumber':
        return ParseNode(True, float(token.attrib), 1)
    elif token.ID == 'hexnumber':
        return ParseNode(True, long(token.attrib,16), 1)
    elif token.ID == 'octnumber':
        return ParseNode(True, long(token.attrib,8), 1)
    else:
        return ParseNode(False, 0, 0)

def lookupSymbol(symbol):
    return symbol

def storeSymbol(symbol, value):
    None

EOF

endif
