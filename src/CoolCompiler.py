from cool import CompilerError, LexicographicError, SyntacticError, tokenizer, remove_comments
from cool import CoolGrammar, CoolParser
from cool.cmp import evaluate_reverse_parse, Token
from cool import FormatVisitor, TypeCollector, TypeBuilder, TypeChecker

import sys

clfile = sys.argv[1]
errors = []

try:
    fd = open(clfile, 'r')
    text = fd.read()
    fd.close()
except FileNotFoundError:
    errors.append(CompilerError((0,0), 'El archivo ' + clfile + ' no se pudo encontrar.'))
    print(errors[0])
    exit(1)

terrors, tokens = tokenizer(text)
# print(tokens, terrors)

if terrors:
    errors.extend(terrors)
    print(errors[0])
    exit(1)

tokens = [Token(t.value, CoolGrammar[t.value] if t.type == 'LIT' else CoolGrammar[t.type.lower()], t.lineno, t.lexpos) for t in tokens]
tokens.append(Token('$', CoolGrammar.EOF))

# Parser ...
parse, operations = CoolParser(tokens)

if not operations:
    errors.append(SyntacticError((parse.line, parse.column), 'No se esperaba el token ' + str(parse.lex)))
    print(errors[0])
    exit(1)

# Semantic ...
ast = evaluate_reverse_parse(parse, operations, tokens)
formatter = FormatVisitor()
# tree = formatter.visit(ast)

collector = TypeCollector(errors)
collector.visit(ast)
context = collector.context

builder = TypeBuilder(context, errors)
builder.visit(ast)

checker = TypeChecker(context, errors)
scope = checker.visit(ast)


if errors:
    for error in errors:
        print(error)
    exit(1)

exit(0)