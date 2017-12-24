load 'strings'

NB. small helper functions
cocurrent 'z'
void =: 0 0&$@
NILF =: ''"_
perr =: 'Error: ' stderr@, ,&LF
die =: exit@[ [ perr@]
lines =: LF&cut
isbox =: -.@(-: >)
enbox =: [: < >^:_
unbox =: >^:_
boxif =: ([: enbox ;/)@]^:(isbox@[)
lpad =: ] , ' ' #~ 0 >. [ - #@]
rpad =: ] ,~ ' ' #~ 0 >. [ - #@]
head =: {.
behead =: }.
tail =: {:
curtail =: }:
isquote =: '''''' -: 0 _1&{
unquote =: ".^:isquote
default =: #@[ {. ] , #@] }. [
clamp =: head@[ >. tail@[ <. ]
repr =: (LF joinstring ;/)^:(#@$ > 1:)@":

safedo =: monad define
  try.
    v =. ". y
    NB. This means one of two things.
    NB. 1: y defines a verb
    NB. 2: y evaluates to the empty string
    if. v -: '' do.
      NB. attempt evaluating as a verb
      try.      (;: y)`:6
                y
      NB. when it fails, y must have evaluated to an empty string
      catch.    v
      end.
    end.
  catch.
    y
  end.
)

flatten =: monad define
  res =. ''
  for_el. y do.
    if. isbox >el do.
      res =. res , flatten > el
    else.
      res =. res , el
    end.
  end.
  res
)

getquotes_sq =: 4 2 2$ 1 1 2 1  1 0 2 2  2 0 3 0  1 2 2 0
getquotes =: (0;getquotes_sq;''''=a.)&;:
argparse =: [: flatten unquote@(cut^:(-.@isquote))each@getquotes

getfunc =: adverb define
  echo u
  try.      u~
  catch.    (;: u)`:6
  end.
)

Note 'fmt documention'
    str fmt x0;x1;...;xN

example:

   'Observe: {} + {} = {}' fmt 1 ; 2 ; 3
Observe: 1 + 2 = 3
)

fmt =: dyad define
  y =. ":each y
  y ('{}' rxmatches x) rxmerge x
)

ENUM =: dyad define void
  i =. 0
  for_j. >;:y do.
    ". x , j , '=:' , ": i
    i =. >:i
  end.
  ''
)
enumerate =: (,.~ ] boxif i.@#)

check =: dyad define
  if. y do. return. end.
  try.      'code message args' =. x
  catch.    'code message' =. x
            args =. <''
  end.
  
  code die ": message fmt args
)

NB. error enum
'ERR' ENUM 'NONE ARG FILE EMPTY NOPLOT RUNTIME'

readf =: monad define
  try.      1!:1 enbox y
  catch.    ERRFILE die 'Unable to read file ' , quote ": unbox y
  end.
)

cocurrent 'base'