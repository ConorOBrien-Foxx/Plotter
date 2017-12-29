NB. small helper functions
cocurrent 'z'

load 'strings math/misc/bigpi tables/csv'

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
isnan =: 128!:5
lasterr =: 13!:12@NILF
sum =: +/
freq =: 1 #. =
range =: [ + i.@(-~&|)
to =: range >:
time =: 6!:0

bigpi =: bigpi f. M.

NB. todo: better bigpi function (since we only usually need a single digit)
NB. bigpi produces numbers in batches of 14
npi =: (0 ". ] { [: ":@bigpi 14 >.@%~ ])"0
NB. pi digit range
rpi =: (0&$:) : (0 "."0 range { [: ":@bigpi 14 >.@%~ +)

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

uniqby =: adverb def ']{~[:(i.~.) i.~@:u'

getquotes_sq =: 4 2 2$ 1 1 2 1  1 0 2 2  2 0 3 0  1 2 2 0
getquotes =: (0;getquotes_sq;''''=a.)&;:
argparse =: NILF`([: flatten unquote@(cut^:(-.@isquote))each@getquotes)@.(*@#)

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

fmt =: ('{}'$:<) : (dyad define)
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
'ERR' ENUM 'NONE ARG FILE EMPTY PLOT RUNTIME NOCMD'

readf =: monad define
  try.      1!:1 enbox y
  catch.    ERRFILE die 'Unable to read file ' , quote ": unbox y
  end.
)

NB. interpolation
interp_pat_temp_ =: rxcomp '\$(\w+)'
interp =: (coname'')&$: : (dyad define)
  matches =. interp_pat_temp_ rxmatches y
  results =. ''
  retbase =. enbox x
  for_match. matches do.
    'start size' =. 1 { match
    name =. (start + i.size) { y
    ". 'results =. results , <fmt {}__retbase' fmt <name
  end.
  results (,:@{."_1 matches) rxmerge y
)

NB. bound pinching
bound_temp_ =: 5
MAXITER_temp_ =: 10^bound_temp_
SMALL_temp_ =. 10^-bound_temp_
verify_temp_ =. [: *./ _ __&~:
testbound =: conjunction define
  res =. ''
  for_el. enumerate y do.
    'ind tmp' =. el
    sgn =. ind { v
    iter =. 0
    while. iter <: MAXITER_temp_ do.
      try.
        if. verify_temp_ u tmp do. break. end.
      catch. end.
      iter =. >: iter
      tmp =. tmp + sgn * SMALL_temp_
    end.
    res =. res , tmp
  end.
  res
)

NB. functions primarily defined for use of the user
dataf =: ".@readf
d =: 1&$: : (1#.>:@?@#)
sim =: simulate =: 1000&$: : ([:".@>[$<@])
NB. from http://code.jsoftware.com/wiki/Plot/Function
sombrero0 =: [: (1&o. % ]) [: %: [: +/~ *:
dyasombrero=: (4 : '(1&o. % ]) %:+/*:x,y')"0/


cocurrent 'base'