load 'strings plot numeric regex trig'
load 'defs.ijs'

args =: 2 }. ARGV

helpmsg =: }: noun define
Usage: jconsole plotter.ijs <in-file>
)

(ERRARG;'Expected 1 argument, received {}.{}{}';<(2-~#ARGV);LF;helpmsg) check 2 < #ARGV

filename =: 2 pick ARGV

content =: readf filename

main =: monad define
  (ERREMPTY;'File {} was empty.{}{}';<(quote filename);LF;helpmsg)check 0 ~: #y
  
  l =. lines y
  
  showplot =: 1
  
  for_it. enumerate l do.
    'i line' =. it
    first =. 0 pick words =. ;: line =. CRLF-.~>line
    args =: (#first) }. line
    select. first
      case. 'data'      do.
        for_file. argparse args do.
          pd ". readf >file
        end.
      
      case. 'window'    do.
        'xmin ymin xmax ymax' =. 4 $ (#~ 4 <.@% #) argparse args
        pd 'xrange {} {}' fmt xmin; xmax
        pd 'yrange {} {}' fmt ymin; ymax
      
      case. 'plotf'     do.
        'func xmin xmax' =: ('cos';'_5';'5') default argparse args
        cocurrent 'progvars'
        xmin_base_ =: ". xmin_base_
        xmax_base_ =: ". xmax_base_
        tmpf_base_ =: (;: func_base_)`:6
        cocurrent 'base'
        pd (xmin,xmax); 'tmpf'
      
      case. 'defun'     do.
        echo argparse args
        name =. 1 pick words
        body =. ' ' joinstring unquote each 2 }. words
        ". '{}_progvars_ =: 3 : {}' fmt name; quote body
      
      case. 'def'       do.
        name =. 1 pick words
        body =. ' ' joinstring unquote each 2 }. words
        ". '{}_progvars_ =: {}' fmt name; body
      
      case. 'echo'      do.
        cocurrent 'progvars'
        echo safedo args_base_
        cocurrent 'base'
        
      case. ,'?'        do.
        cocurrent 'progvars'
        stdout repr safedo args_base_
        cocurrent 'base'
        
      case. 'die'       do.
        cocurrent 'progvars'
        stderr LF ,~ repr safedo args_base_
        exit ERRRUNTIME
        cocurrent 'base'
        
      case. ,'!'        do.
        cocurrent 'progvars'
        stderr repr safedo args_base_
        exit ERRRUNTIME
        cocurrent 'base'
      
      case. 'hide'      do.
        showplot =: 0
      
      NB. unary functions
    end.
  end.
  
  if. showplot do.
    try.
      pd 'show'
    catch.
      ERRNOPLOT die 'No plot was constructed.'
    end.
  end.
)

main content

exit ERRNONE