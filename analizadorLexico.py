import ply.lex as lex

tokens = (
    'NUMBER',
    'DECIMAL',
    'LITERAL',
    'BOOL',
    'FUNCION',
    'PLUS',
    'MINUS',
    'MULT',
    'DIVIDE',
    'LPAREN',
    'RPAREN',
    'KEYL',
    'KEYR',
    'COMMENT',
    'TYPEINT',
    'TYPEBOOL',
    'TYPEFLOAT',
    'TYPESTRING',
    'EQUAL',
    'COMPAR',
    'DIFERENT',
    'MOREE',
    'LESSE',
    'MORE',
    'LESS', 
    'BUCLEF',
    'BUCLEW', 
    'ID',
    'IF',
    'ELSE',
    'DPUNTO',
    'FINAL',
  )


t_PLUS    = r'\+'
t_MINUS   = r'-'
t_MULT   = r'\*'
t_DIVIDE  = r'/'
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_KEYL  = r'\{'
t_KEYR  = r'\}'
t_COMMENT = r'//'
t_TYPEINT = r'int'
t_TYPEBOOL = r'FiftyFifty'
t_TYPEFLOAT = r'float'
t_DIFERENT = r'\!='
t_MOREE     = r'\>=' 
t_LESSE    = r'\<='
t_MORE     = r'\>' 
t_LESS     = r'\<'
t_BUCLEF  = r'repeat'
t_BUCLEW  = r'meanwhile'
t_LITERAL = r'\".*\"'
t_FUNCION = r'def'
t_IF     = r'if'
t_ELSE   = r'else'
t_DPUNTO = r'\:'
t_FINAL  = r'\;'


def t_EQUAL(t):
  r'='
  if(t.value == '=='):
     t.type = 'COMPAR'
  return t

def t_ID(t):
  r'([a-z]|[A-Z])([a-z]|[A-Z]|[0-9]|_)*'
  if (t.value == 'int'):
      t.type = 'TYPEINT'
      return t
  elif(t.value == 'FiftyFifty'):
      t.type = 'TYPEBOOL'
  elif(t.value == 'true'):
      t.type = 'BOOL'
  elif(t.value == 'false'):
      t.type = 'BOOL'
  elif(t.value == 'float'):
      t.type = 'TYPEFLOAT'
  elif(t.value == 'String'):
      t.type = 'TYPESTRING'
  elif(t.value == '(\".*\")|(“.*”)'):
      t.type = 'LITERAL'
  elif(t.value == 'def'):
      t.type = 'FUNCION'
  elif(t.value == 'repeat'):
      t.type = 'BUCLEF'
  elif(t.value == 'meanwhile'):
      t.type = 'BUCLEW'
  elif(t.value == 'if'):
      t.type = 'IF'
  elif(t.value == 'else'):
      t.type = 'ELSE'
  return t

def t_DECIMAL(t):
  r'\d+(\.\d+)'
  t.value = float(t.value)
  return t

def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value) 
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

 
t_ignore  = ' \t'


def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


lexer = lex.lex()


def lector(filename):
  with open(filename,'r') as file:
    return file.read()



def test(data):
  lexer.input(data)
  tokens_list = []
  while True:
    tok = lexer.token()
    if not tok: 
        break 
    tokens_list.append({ "type":tok.type,"value":tok.value, "line":tok.lineno, "column":tok.lexpos})
  return tokens_list



if __name__ == '__main__':
    filename = 'factorialRecursivo.txt'
    data = lector(filename)
    tokens = test(data)
    for token in tokens:
        print(token, '\n')