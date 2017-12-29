load 'strings plot numeric regex trig'
load 'defs.ijs'
args =: 2 }. ARGV

helpmsg =: }: noun define
Usage: jconsole plotter.ijs <in-file>
)

NB. randomizing the seed (usually the same state upon startup?)
(9!:1) 1000 * sum time ''

(ERRARG;'Expected 1 argument, received {}.{}{}';<(2-~#ARGV);LF;helpmsg) check 2 < #ARGV

filename =: 2 pick ARGV

content =: readf filename

TYPE =: 'line'
settype =: monad def 'pd ''type '', line =: y'

main =: monad define
  (ERREMPTY;'File {} was empty.{}{}';<(quote filename);LF;helpmsg)check 0 ~: #y -. CRLF, ' '
  
  l =. lines y -. CR
  
  showplot =: 1
  building =: 0
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
    select. first
      case. 'dataf'             do.
        for_file. argparse args do.
          pd ". readf >file
        end.
      
      case. 'legend';'key'      do.
        pd 'key ' , args
      
      case. 'data'              do.
        cocurrent 'progvars'
        pd ". args_base_
        cocurrent 'base'
      
      case. 'title'             do.
        pd 'title {}' fmt <'progvars' interp args
      
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
        pd 'xlabel {}' fmt <keys
        
      case. 'xrange';'yrange'   do.
        cocurrent 'progvars'
        res_base_ =: ". each 2 {. argv_base_
        cocurrent 'base'

        pd '{} {}' fmt first;>res
      
      case. 'xlabel';'ylabel'   do.
        pd '{} {}' fmt first;args
      
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
        ". '{} =: {}' fmt name_base_; body_base_
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
      
      case. do.
        ERRNOCMD die '[line {}] No such command {}.' fmt i; quote first
    end.
  end.
  
  if. showplot do.
    try.
      pd 'show'
    catch.
      stderr lasterr ''
      exit ERRPLOT
    end.
  end.
)

main content

exit ERRNONE