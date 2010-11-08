"TODO: finish off parser
"TODO: implement symbol lookup table
"TODO: present much friendlier error messages
"TODO: move most of the functionality to autoload script?
"TODO: write all of the math functions including hex/dec/oct conversion
"TODO: implement octal numbers
"TODO: built-in help like taglist/NerdTree?
"TODO: Arbitrary precision numbers!!!
"TODO: write documentation
"TODO: syntax highlighting
"TODO: autoload script?

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

#### syntactic analysis functions ##################################################

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
#octalnumber = ?? TODO:

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
    def __repr__(self):
        return str(self._tokenID) + ':' + str(self._attrib)
    def __str__(self):
        return str(self._tokenID) + ':' + str(self._attrib)
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
def tokenize(expr):
    tokens = []
    while expr != "":
        matchedLexeme = False
        for lexeme in lexemes:
            match = matchesFront(lexeme.regex, expr)
            if match != "":
                tokens.append(Token(lexeme.ID, match))
                expr = expr[len(match):]
                matchedLexeme = True
                break
        if not matchedLexeme: return [Token('ERROR', expr)]
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

#### parser functions ##############################################################

#TODO: this is all a bit messy due to passing essentially a vector around
# instead of a list and not having shared state. Could be made a
# lot simpler by using shared state...

#gcalc context-free grammar
#line    -> expr | assign
#assign  -> let ident = expr
#expr    -> expr + term | expr - term | func | term
#func    -> ident ( args )
#args    -> expr , args | expr
#term    -> term * factor | term / factor | term % factor
#           | term << factor | term >> factor | term ! | factor
#factor  -> expt ** factor | expt
#expt    -> number | ident | ( expr )
#number  -> decnumber | hexnumber | octalnumber

#gcalc context-free grammar LL(1) -- to be used with a recursive descent parser
#line    -> expr | assign
#assign  -> let ident = expr
#expr    -> expr' {(+|-) expr'}
#expr'   -> func|term
#func    -> ident ( args )
#args    -> expr {, expr}
#term    -> factor {(*|/|%|<<|>>) factor} [!]
#factor  -> expt {** expt}
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
#generates the parse tree with evaluated decoration
def parse(expr):
    tokens = tokenize(expr)
    lineNode = line(tokens)
    if lineNode.success:
        return lineNode.result
    else:
        return "Error: Result:" + str(lineNode.result) + ' Consumed:' + str(lineNode.consumeCount)

def line(tokens):
    assignNode = assign(tokens)
    if assignNode.success:
        if assignNode.consumeCount == len(tokens):
            return assignNode
        else:
            return ParseNode(False, 0, assignNode.consumeCount)
    exprNode = expr(tokens)
    if exprNode.success:
        if exprNode.consumeCount == len(tokens):
            return exprNode
        else:
            return ParseNode(False, 0, exprNode.consumeCount)
    return ParseNode(False, 0, 0)

def assign(tokens):
    if map(getID, tokens[0:3]) == ['let', 'ident', 'assign']:
        exprNode = expr(tokens[3:])
        if exprNode.consumeCount+3 == len(tokens):
            storeSymbol(tokens[1].attrib, exprNode.result)
            return ParseNode(True, exprNode.result, exprNode.consumeCount+3)
        else:
            return ParseNode(False, 0, exprNode.consumeCount+3)
    else:
        return ParseNode(False, 0, 0)

def expr(tokens):
    exprNode = exprPrime(tokens)
    consumed = exprNode.consumeCount
    if exprNode.success:
        foldNode = foldlParseMult(exprPrime,
                                  [lambda x,y:x+y, lambda x,y:x-y],
                                  ['plus','subtract'],
                                  exprNode.result,
                                  tokens[consumed:])
        consumed += foldNode.consumeCount
        return ParseNode(foldNode.success, foldNode.result, consumed)
    else:
        return ParseNode(False, 0, consumed)

def exprPrime(tokens):
    funcNode = func(tokens)
    if funcNode.success:
        return funcNode
    termNode = term(tokens)
    if termNode.success:
        return termNode
    return ParseNode(False, 0, 0)

def func(tokens):
    if map(getID, tokens[0:2]) == ['ident', 'lParen']:
        argsNode = args(tokens[2:])
        if symbolCheck('rParen', argsNode.consumeCount+2, tokens):
            return ParseNode(True, argsNode.result, argsNode.consumeCount+3)
        else:
            return ParseNode(False, 0, argsNode.consumeCount+2)
    else:
        return ParseNode(False, 0, 0)

def args(tokens):
    #returns a list of exprNodes to be used as function arguments
    exprNode = expr(tokens)
    consumed = exprNode.consumeCount
    if exprNode.success:
        foldNode = foldlParse(expr, snoc, 'comma', [exprNode.result], tokens[consumed:])
        return ParseNode(foldNode.success, foldNode.result, consumed+foldNode.consumeCount)
    else:
        return ParseNode(False, 0, consumed)

def term(tokens):
    factNode = factor(tokens)
    consumed = factNode.consumeCount
    if factNode.success:
        foldNode = foldlParseMult(factor, #TODO: change from int in lambdas
                                  [lambda x,y:x*y, lambda x,y:x/y, lambda x,y:x%y,
                                      lambda x,y:int(x)<<int(y), lambda x,y:int(x)>>int(y)],
                                  ['multiply', 'divide', 'modulo', 'lShift', 'rShift'],
                                  factNode.result,
                                  tokens[consumed:])
        consumed += foldNode.consumeCount
        if symbolCheck('factorial', consumed, tokens):
            return ParseNode(foldNode.success, factorial(foldNode.result), consumed+1)
        else:
            return ParseNode(foldNode.success, foldNode.result, consumed)
    else:
        return ParseNode(False, 0, consumed)

def factor(tokens):
    exptNode = expt(tokens)
    consumed = exptNode.consumeCount
    result = exptNode.result
    if exptNode.success:
        foldNode = foldlParse(expt, lambda x,y:x**y, 'exponent', result, tokens[consumed:])
        return ParseNode(foldNode.success, foldNode.result, consumed+foldNode.consumeCount)
    else:
        return ParseNode(False, 0, consumed)

def expt(tokens):
    if symbolCheck('ident', 0, tokens):
        return ParseNode(True, lookupSymbol(tokens[0].attrib), 1)
    numberNode = number(tokens)
    if numberNode.success:
        return numberNode
    if symbolCheck('lParen', 0, tokens):
        exprNode = expr(tokens[1:])
        if exprNode.success:
            if tokens[exprNode.consumeCount+1].ID == 'rParen':
                return ParseNode(True, exprNode.result, exprNode.consumeCount+2)
    return ParseNode(False, 0, 0)

def number(tokens):
    if symbolCheck('decnumber', 0, tokens):
        return ParseNode(True, float(tokens[0].attrib), 1)
    elif symbolCheck('hexnumber', 0, tokens):
        return ParseNode(True, long(tokens[0].attrib,16), 1)
    elif symbolCheck('octnumber', 0, tokens):
        return ParseNode(True, long(tokens[0].attrib,8), 1)
    else:
        return ParseNode(False, 0, 0)

#### helper functions for use by the parser ########################################

def foldlParse(parsefn, resfn, symbol, initial, tokens):
    consumed = 0
    result = initial
    if tokens == []:
        return ParseNode(True, result, consumed)
    else:
        while tokens[consumed].ID == symbol:
            parseNode = parsefn(tokens[consumed+1:])
            consumed += parseNode.consumeCount+1
            if parseNode.success:
                result = resfn(result, parseNode.result)
                if consumed >= len(tokens): return ParseNode(True,result,consumed)
            else:
                return ParseNode(False, 0, consumed)
        return ParseNode(True, result, consumed)

def foldlParseMult(parsefn, resfns, syms, initial, tokens):
    consumed = 0
    result = initial
    if tokens == []:
        return ParseNode(True, result, consumed)
    else:
        while tokens[consumed].ID in syms:
            sym = tokens[consumed].ID
            parseNode = foldlParse(parsefn, resfns[syms.index(sym)], sym, result, tokens[consumed:])
            if parseNode.success:
                result = parseNode.result
                consumed += parseNode.consumeCount
                if consumed >= len(tokens): return ParseNode(True,result,consumed)
            else:
                return ParseNode(False, 0, consumed)
        return ParseNode(True, result, consumed)

def symbolCheck(symbol, index, tokens):
    if index < len(tokens):
        if tokens[index].ID == symbol:
            return True
    return False

def snoc(seq, x):  #TODO: find more pythonic way of doing this
    a = seq
    a.append(x)
    return a

#### symbol table manipulation functions ###########################################

def lookupSymbol(symbol):
    return 5

def storeSymbol(symbol, value):
    print str(symbol) + ':' + str(value)


#### mathematical functions (built-ins) ############################################

def factorial(n):
    acc = 1
    for i in xrange(int(n)): #TODO: change from int
        acc *= i+1
    return acc


EOF

endif
