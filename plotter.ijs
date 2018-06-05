load 'strings plot numeric regex trig'
load './defs.ijs'
args =: 2 }. ARGV

helpmsg =: }: noun define
Usage: jconsole plotter.ijs <in-file>
)

out_default =: 'c:/users/conorob/j64-806-user/temp/plot.pdf'

NB. randomizing the seed (usually the same state upon startup?)
(9!:1) 1000 * sum time ''

(ERRARG;'Expected 1 argument, received {}.{}{}';<(2-~#ARGV);LF;helpmsg) check 2 < #ARGV

filename =: 2 pick ARGV

ARGV_progvars_ =: 3 }. ARGV
ARG_progvars_ =: pick&ARGV_progvars_

content =: readf filename

COMMENT_WORDS =: deb each lines (noun define)
    note
    aside
    comment
    nb
    nop
    noop
    explanation
    todo
    bug
    #
)
GENERICS =: deb each lines (noun define)
    xcaption
    ycaption
    captionfont
    titlefont
    labelfont
    keyfont
    keypos
    keystyle
    keymarkers
    keycolor
    pensize
    aspect
)

TYPE =: 'line'
settype =: monad def 'pd ''type '', line =: y'

main =: monad define
  (ERREMPTY;'File {} was empty.{}{}';<(quote filename);LF;helpmsg)check 0 ~: #y -. CRLF, ' '
  
  l =. lines y -. CR
  
  showplot =: 1
  building =: 0
  save_loc =: ''
  build =: ''
  
  for_it. enumerate l do.
    'i line' =. it
    if. building do.
      if. -. 'end' -: deb line do.
        build =: build , <line
        continue.
      end.
    end.
    
    first =. 0 pick words =. ;: line =. CRLF-.~>line
    args =: (1 + #first) }. line
    argv =: argparse args
    select. tolower first -. '.:'
      case. 'dataf'             do.
        for_file. argparse args do.
          pd ". readf >file
        end.
      
      case. 'saveto'            do.
        save_loc =: deb args
      
      case. 'legend';'key'      do.
        pd 'key ' , args
      
      case. 'data'              do.
        cocurrent 'progvars'
        pd ". args_base_
        cocurrent 'base'
      
      case. 'title'             do.
        pd 'title {}' fmt <deb 'progvars' interp args
      
      case. 'points'            do.
        oldtype =. TYPE
        settype 'dot'
        cocurrent 'progvars'
        pd ". args_base_
        cocurrent 'base'
        settype oldtype
      
      case. 'type'              do.
        settype 0 pick argv
      
      case. 'freq';'frequency'  do.
        cocurrent 'progvars'
        data_base_ =: ". args_base_
        cocurrent 'base'
        if. isbox data do.
          'keys data' =: data
          data =: sort data
        else.
          data =: sort data
          keys =: <~. data
        end.
        pd freq data
        if. isbox keys do.
          keys =: ' ' joinstring dquote@fmt each keys
        end.
        pd 'xlabel {}' fmt <keys
        
      case. 'xrange';'yrange'   do.
        cocurrent 'progvars'
        res_base_ =: ". each 2 {. argv_base_
        cocurrent 'base'

        pd '{} {}' fmt first;>res
      
      case. 'xlabel';'ylabel'   do.
        pd '{} {}' fmt first;args
      
      case. 'axes'              do.
        pd 'axes {}' fmt deb args
      
      case. 'color'             do.
        colors =. ', ' joinstring deb argv
        pd 'color {}' fmt <colors
      
      case. GENERICS            do.
        pd '{} {}' fmt first;deb args
      
      case. 'freqkey'           do.
        cocurrent 'progvars'
        data_base_ =: ". args_base_
        cocurrent 'base'
        data =: sort data
        pd 'key {}' fmt <~. data
        pd freq data
      
      case. 'immediate';'raw'   do.
        building =: 1
      
      case. 'end'               do.
        building =: 0
        cocurrent 'progvars'
        monad def build_base_ :: _: ''
        cocurrent 'base'
      
      case. 'do'                do.
        cocurrent 'progvars'
        monad def args_base_ :: _: ''
        cocurrent 'base'
      
      case. 'window'            do.
        'xmin ymin xmax ymax' =. 4 $ (#~ 4 <.@% #) argv
        pd 'xrange {} {}' fmt xmin; xmax
        pd 'yrange {} {}' fmt ymin; ymax
      
      case. 'plotf'             do.
        'func xmin xmax scale' =: ('cos';'_5';'5';_.) default argv
        NB. evaluate xmin, xmax in the user's scope
        cocurrent 'progvars'
        xmin_base_ =: ". xmin_base_
        xmax_base_ =: ". xmax_base_
        tmpf_base_ =: (;: func_base_)`:6
        cocurrent 'base'
        NB. test min, max - functional plotting does not like plotting with domain
        NB. errors in the bounds'
        'xmin xmax' =: (tmpf testbound _1 1) xmin,xmax
        if.     isnan scale do.  pd (xmin,xmax); 'tmpf'
        else.                    pd (xmin,xmax,scale); 'tmpf'
        end.
      
      case. 'plot'              do.
        cocurrent 'progvars'
        plot ". args_base_
        cocurrent 'base'
        showplot =: 0
      
      case. 'pd'                do.
        cocurrent 'progvars'
        pd ". args_base_
        cocurrent 'base'
      
      case. 'defun'             do.
        name =. 0 pick words
        body =. ' ' joinstring unquote each 1 }. argv
        ". '{}_progvars_ =: 3 : {}' fmt name; quote body
      
      case. 'def';'set'         do.
        name =: 0 pick argv
        body =: deb (#name) }. args
        cocurrent 'progvars'
        try.
          ". '{} =: {}' fmt name_base_; body_base_
        catch.
          cocurrent 'base'
          ERRRUNTIME die '{}: In `{}`:{}|{}' fmt (geterr '') ; first ; LF ; line
        end.
        cocurrent 'base'
      
      case. 'echo'              do.
        cocurrent 'progvars'
        echo safedo args_base_
        cocurrent 'base'
        
      case. ,'?'                do.
        cocurrent 'progvars'
        stdout repr safedo args_base_
        cocurrent 'base'
        
      case. 'die'               do.
        cocurrent 'progvars'
        stderr LF ,~ repr safedo args_base_
        exit ERRRUNTIME
        cocurrent 'base'
        
      case. ,'!'                do.
        cocurrent 'progvars'
        stderr repr safedo args_base_
        exit ERRRUNTIME
        cocurrent 'base'
      
      case. 'hide'              do.
        showplot =: 0
    
      NB. comment symbol
      case. COMMENT_WORDS       do.
        NB. do nothing
      
      case. do.
        ERRNOCMD die '[line {}] No such command {}.' fmt i; quote first
    end.
  end.
  
  if. showplot do.
    try.
      pd 'show'
      if. -. save_loc -: '' do.
        content =. 1!:1 <out_default
        content 1!:2 <save_loc
      end.
    catch.
      stderr lasterr ''
      exit ERRPLOT
    end.
  end.
)

main content

exit ERRNONE