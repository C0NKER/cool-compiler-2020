import ply.lex as lex
from .errors import LexicographicError

def find_column(code, token):
    line_start = code.rfind('\n', 0, token.lexpos) + 1
    return token.lexpos - line_start + 1

###### TOKEN LISTS ######
reserved = [
	'CLASS',
	'INHERITS',
	'IF',
	'THEN',
	'ELSE',
	'FI',
	'WHILE',
	'LOOP',
	'POOL',
	'LET',
	'IN',
	'CASE',
	'OF',
	'ESAC',
	'NEW',
	'ISVOID',
]

tokens = [
    # Literals
    'LIT',
	# Identifiers
	'TYPE', 'ID',
	# Primitive data types
	'INTEGER', 'STRING', 'BOOL',
	# Special keywords
	'ACTION',
	# Operators
	'ASSIGN', 'LESS', 'LESSEQUAL', 'EQUAL', 'INT_COMPLEMENT', 'NOT',
] + reserved

# Ignore rule for single line comments
# t_ignore_SINGLE_LINE_COMMENT = r"\-\-[^\n]*"
def t_SINGLE_LINE_COMMENT(token):
    r"\-\-[^\n]*"
    token.lexer.lineno += 1
    token.lexer.skip(1)

###### TOKEN RULES ######

# Operators
t_LIT = r"[\+\-\*\/\:\;\(\)\{\}\@\.\,]"
t_ASSIGN = r'<-'
t_LESS = r'<'
t_LESSEQUAL = r'<='
t_EQUAL = r'='
t_INT_COMPLEMENT = r'~'

# Special keywords
t_ACTION = r'=>'

# Primitive data types
def t_INTEGER(t):
	r"[0-9]+"
	t.value = int(t.value)
	return t

def t_BOOL(t):
	r"t[rR][uU][eE]|f[aA][lL][sS][eE]"
	t.value = True if t.value.lower() == 'true' else False
	return t



# Other tokens with precedence before TYPE and ID
def t_NOT(t):
	r"[nN][oO][tT]"
	return t

# Identifiers
def check_RESERVED(t):
    tupper = t.value.upper()

    if tupper in reserved:
        t.type = tupper

def t_TYPE(t):
    r"[A-Z][A-Za-z0-9_]*"
    check_RESERVED(t)
    return t

def t_ID(t):
    r"[a-z][A-Za-z0-9_]*"
    check_RESERVED(t)
    return t

def t_newline(token):
    r"\n+"
    token.lexer.lineno += len(token.value)

t_ignore = ' \t\r\f'

# LEXER STATES
states = (
        ("STRING", "exclusive"),
        ("COMMENT", "exclusive")
    )
###

# THE STRING STATE
def t_start_string(token):
    r"\""
    token.lexer.push_state("STRING")
    token.lexer.string_backslashed = False
    token.lexer.stringbuf = ""

def t_STRING_newline(token):
    r"\n"
    token.lexer.lineno += 1
    if not token.lexer.string_backslashed:
        errors.append(LexicographicError((token.lineno, find_column(token.lexer.lexdata, token)), 'Unterminated string constant'))
        token.lexer.pop_state()
    else:
        token.lexer.string_backslashed = False

def t_STRING_null(token):
    r"\0"
    errors.append(LexicographicError((token.lineno, find_column(token.lexer.lexdata, token)), 'Null character in string'))
    token.lexer.skip(1)

def t_STRING_end(token):
    r"\""
    if not token.lexer.string_backslashed:
        token.lexer.pop_state()
        token.value = token.lexer.stringbuf
        token.type = "STRING"
        return token
    else:
        token.lexer.stringbuf += '"'
        token.lexer.string_backslashed = False

def t_STRING_anything(token):
    r"[^\n]"
    if token.lexer.string_backslashed:
        if token.value == 'b':
            token.lexer.stringbuf += '\b'
        elif token.value == 't':
            token.lexer.stringbuf += '\t'
        elif token.value == 'n':
            token.lexer.stringbuf += '\n'
        elif token.value == 'f':
            token.lexer.stringbuf += '\f'
        elif token.value == '\\':
            token.lexer.stringbuf += '\\'
        else:
            token.lexer.stringbuf += token.value
        token.lexer.string_backslashed = False
    else:
        if token.value != '\\':
            token.lexer.stringbuf += token.value
        else:
            token.lexer.string_backslashed = True

# STRING ignored characters
t_STRING_ignore = ''

# STRING error handler
def t_STRING_error(token):
    errors.append(LexicographicError((token.lineno, find_column(token.lexer.lexdata, token)), 'ERROR at or near ' + token.value[:10]))
    token.lexer.skip(1)

def t_STRING_eof(token):
    errors.append(LexicographicError((token.lineno, find_column(token.lexer.lexdata, token)), 'EOF in string'))
    token.lexer.pop_state()

###
# THE COMMENT STATE
def t_start_comment(token):
    r"\(\*"
    token.lexer.push_state("COMMENT")
    token.lexer.comment_count = 0

def t_COMMENT_newline(token):
    r"\n+"
    token.lexer.lineno += len(token.value)


def t_COMMENT_startanother(t):
    r"\(\*"
    t.lexer.comment_count += 1

def t_COMMENT_end(token):
    r"\*\)"
    if token.lexer.comment_count == 0:
        token.lexer.pop_state()
    else:
        token.lexer.comment_count -= 1

# COMMENT ignored characters
t_COMMENT_ignore = ''

# COMMENT error handler
def t_COMMENT_error(token):
    token.lexer.skip(1)

def t_COMMENT_eof(token):
    errors.append(LexicographicError((token.lineno, find_column(token.lexer.lexdata, token)), 'EOF in comment'))
    token.lexer.pop_state()

###### SPECIAL RULES ######
errors = []

def t_error(token):
    errors.append(LexicographicError((token.lineno, find_column(token.lexer.lexdata, token)), 'ERROR at ooooor near ' + token.value[:10]))
    token.lexer.skip(1)


###### CREATE LEXER ######
lex.lex()

###### TOKENIZER ######
def tokenizer(code):
    lex.input(code)

    token_list = []

    while True:
        token = lex.token()

        if token is None:
            break

        token.lexpos = find_column(code, token)

        token_list.append(token)
    
    return errors, token_list