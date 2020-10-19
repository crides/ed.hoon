/+  shoe, verb, dbug, default-agent, *sole
|%
+$  anypath  [rel=? =path]
+$  cmd-st-ty
  $%
    [%norm ~]
    [%insert line=@ud]
    [%append line=@ud]
  ==
+$  state-ty
  $:
    file=(unit path)
    line=@ud
    buff=wain
    oldbuff=wain
    mark=(map char @ud)
    cmd-st=cmd-st-ty
    lastpat=cord
    remote=(set @p)
  ==
+$  line-head-ty
  $%
    [%abs line=@ud]
    [%mark mark=char]
    [%last ~]
    [%none ~]
    [%current ~]
  ==
+$  line-ty
  $:
    head=line-head-ty
    rel=(unit @sd)
  ==
+$  frange                                                    ::  Flexible range in parsing
  $%
    [%line line=line-ty]
    [%range start=line-ty end=line-ty]
  ==
+$  range-ty  [start=@ud len=@ud]
+$  ext-cmd-ty
  $%
    [%allow-remote ship=@p]
    [%remove-remote ship=@p]
    [%list-remote ~]
  ==
+$  cmd-ty
  $%
    [%append line=line-ty]
    [%change range=frange]
    [%delete range=frange]
    [%edit path=(unit anypath)]
    [%file path=(unit anypath)]
    [%goto line=line-ty]
    [%insert line=line-ty]
    [%kark line=line-ty mark=char]                            ::  Kreate mark
    [%print range=frange]
    [%undo ~]
    [%write path=(unit anypath)]
    [%extern cmd=ext-cmd-ty]

    [%text text=(unit tape)]
  ==
+$  card  card:shoe
++  shoe-fec
  |=  fec=sole-effect  ^-  (list card)
  ~[[shoe+~+sole+fec]]
++  prompt
  |=  p=tape  ^-  (list card)
  (shoe-fec pro+[%& %prompt p])
++  shoe-print
  |=  t=tape  ^-  (list card)
  (shoe-fec txt+t)
++  more-fec
  |=  fecs=(list sole-effect)  ^-  (list card)
  (shoe-fec mor+fecs)
--
=/  state=state-ty
  :*
    file=~
    line=1
    buff=*wain
    oldbuff=*wain
    mark=*(map char @ud)
    cmd-st=[%norm ~]
    lastpat=*cord
    remote=(sy ~[~zod])
  ==
%+  verb  |
%-  agent:dbug
^-  agent:gall
%-  (agent:shoe (pair cmd-ty tape))
^-  (shoe:shoe (pair cmd-ty tape))
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
    des   ~(. (default:shoe this (pair cmd-ty tape)) bowl)
::
++  on-init   on-init:def
++  on-save   !>(state)
++  on-load
  |=  old=vase  ^-  (quip card _this)
  [(prompt ": ") this(state !<(state-ty old))]
++  on-poke   on-poke:def
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-agent  on-agent:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
++  command-parser
  |=  sole-id=@ta
  ^+  |~(nail *(like [? (pair cmd-ty tape)]))
  %+  stag  %.n
  |^
    %+  cook  |=(cmd=cmd-ty [cmd (repr cmd)])
    ?-  cmd-st.state
    [%norm ~]
    ;~  pose
      change
      edit
      file
      print
      kark
      (cold [%undo ~] (just 'u'))                             ::  Undo
      extern
      write
      goto
    ==
    [%append *]  (stag %text text:pars)
    [%insert *]  (stag %text text:pars)
    ==

  ++  extern
    %+  knee  *cmd-ty  |.  ~+
    %+  stag  %extern
    ;~  pfix  zap
      ;~  pose
        %+  stag  %allow-remote
        ;~(pfix (jest 'allow') ws:pars patp:pars)
        %+  stag  %remove-remote
        ;~(pfix (jest 'remove') ws:pars patp:pars)
        %+  stag  %list-remote
        (cold ~ (jest 'remote'))
      ==
    ==

  ++  file                                                    ::  Print file/set file name
    %+  knee  *cmd-ty  |.  ~+
    %+  stag  %file
    ;~(pfix (just 'f') optpath:pars)
  ++  edit                                                    ::  Open/Edit a file
    %+  knee  *cmd-ty  |.  ~+
    %+  stag  %edit
    ;~(pfix (just 'e') optpath:pars)
  ++  change                                                  ::  File content edit
    %+  knee  *cmd-ty  |.  ~+
    ;~  pose
      %+  cook
        |=  [a=line-ty b=cord]  ^-  cmd-ty
        ?:  =(b 'a')  [%append a]
        ?:  =(b 'i')  [%insert a]
        ~|  "???"  !!
      ;~  plug  line:pars
        ;~  pfix  ws:pars 
          ;~  pose
            (just 'a')                                          ::  Append after line
            (just 'i')                                          ::  Insert after line
          ==
        ==
      ==
      %+  stag  %change
      ;~(sfix frange:pars ;~(plug ws:pars (just 'c')))          ::  Change range
      %+  stag  %delete
      ;~(sfix frange:pars ;~(plug ws:pars (just 'd')))          ::  Delete range
    ==
  ++  kark
    %+  knee  *cmd-ty  |.  ~+
    %+  stag  %kark
    ;~(plug line:pars ;~(pfix (just 'k') low))
  ++  print
    %+  knee  *cmd-ty  |.  ~+
    %+  stag  %print
    ;~(sfix frange:pars ;~(plug ws:pars (just 'p')))
  ++  goto
    %+  knee  *cmd-ty  |.  ~+
    (stag %goto line:pars)
  ++  write                                                   ::  Open/Edit a file
    %+  knee  *cmd-ty  |.  ~+
    %+  stag  %write
    ;~(pfix (just 'w') optpath:pars)
  ++  repr
    |=  cmd=cmd-ty  ^-  tape
    |^
      ?-  -.cmd
      %append  (welp (line line.cmd) "a")
      %change  (welp (ranj range.cmd) "c")
      %delete  (welp (ranj range.cmd) "d")
      %edit  (welp "e " (biff path.cmd path))
      %file  (welp "f " (biff path.cmd path))
      %goto  (line line.cmd)
      %insert  (welp (line line.cmd) "i")
      %kark  (welp (line line.cmd) "k{<mark.cmd>}")
      %print  (welp (ranj range.cmd) "p")
      %undo  "u"
      %write  (welp "w " (biff path.cmd path))
      %extern
        ?-  -.cmd.cmd
        %allow-remote  "!allow {<ship.cmd.cmd>}"
        %remove-remote  "!remove {<ship.cmd.cmd>}"
        %list-remote  "!remote"
        ==
      %text  ?~  text.cmd  "."  u.text.cmd
      ==
    ++  path
      |=  pax=anypath  ^-  tape
      (welp ?:(rel.pax "" "%") <path.pax>)
    ++  line
      |=  line=line-ty  ^-  tape
      =/  head=tape  ?-  -.head.line
        %abs  <line.head.line>
        %mark  "'{<mark.head.line>}"
        %last  "$"
        %none  ""
        %current  "."
        ==
      =/  rel  ?~  rel.line  ""
        =/  old  (old:si u.rel.line)
        ?:  -.old  "+{<+.old>}"  "-{<+.old>}"
      (welp head rel)
    ++  ranj
      |=  ranj=frange  ^-  tape
      ?-  -.ranj
      %line  (line line.ranj)
      %range  "{(line start.ranj)},{(line end.ranj)}"
      ==
    --
  ++  pars
    |%
    ++  ws  (cold ~ (star (just ' ')))
    ++  empty  (cold ~ (just ''))
    ++  path                                                  ::  Abs/Rel path
      %+  knee  *anypath  |.  ~+
      ;~  plug  ;~(pose (cold %.n cen) (easy %.y))
        ;~  pfix  fas
          %+  more  fas
          %+  cook  ,@ta  urs:ab
        ==
      ==
    ++  optpath
      %+  knee  *(unit anypath)  |.  ~+
      ;~  pose
        %+  cook  some  ;~(pfix ws:pars path:pars)
        (easy ~)
      ==
    ++  frange
      %+  knee  *^frange  |.  ~+
      ;~  pose
        %+  stag  %range
        ;~(plug line ;~(pfix com line))
        %+  stag  %line
        line
      ==
    ++  line
      %+  knee  *line-ty  |.  ~+
      ;~  plug
        ;~  pose
          (cold current+~ dot)                                  ::  Current line
          (cold last+~ buc)                                     ::  Last line
          (stag %abs dem)                                       ::  Absolute line
          (stag %mark ;~(pfix (just '\'') low))                 ::  Mark
          (easy none+~)                                         ::  Nothing
        ==
        ;~  pose
          ;~  pfix  lus                                         ::  Relative after
            ;~  pose
              (cook |=(d=@ud `(sun:si d)) dem)
              (easy `--1)
            ==
          ==
          ;~  pfix  hep                                         ::  Relative before
            ;~  pose
              %+  cook  |=(d=@ud `(dif:si --0 (sun:si d)))  dem
              (easy `-1)
            ==
          ==
          (easy ~)
        ==
      ==
    ++  text  (cook |=(t=tape ?:(=(t ".") ~ `t)) (star prn))
    ++  patp
      %+  knee  *@p  |.  ~+
      ;~(pfix sig fed:ag)
    --
  --

++  tab-list  tab-list:des
++  on-command
  |=  [sole-id=@ta cmd=(pair cmd-ty tape)]
  |^  ^-  (quip card _this)
    =/  cmd-tape=tape  q.cmd
    =/  cmd=cmd-ty  p.cmd
    =/  new=(unit (quip card _this))  ?-  cmd
      [%append *]
        %+  biff  (resolve-line line.cmd)
        |=  line=@ud
        `[(prompt ": append: ") this(cmd-st.state [%append line], oldbuff.state buff.state)]
      [%change *]  (biff (resolve-range range.cmd) change)
      [%delete *]  (biff (resolve-range range.cmd) delete)
      [%edit *]  (biff (get-file path.cmd) edit)
      [%file *]  (biff (get-file path.cmd) file)
      [%goto *]  (biff (resolve-line-none line.cmd) goto)
      [%insert *]
        %+  biff  (resolve-line line.cmd)
        |=  line=@ud
        `[(prompt ": insert: ") this(cmd-st.state [%insert (dec line)], oldbuff.state buff.state)]
      [%kark *]
        %+  biff  (resolve-line line.cmd)
        |=  line=@ud
        ``this(mark.state (~(put by mark.state) mark.cmd line))
      [%print *]  (biff (resolve-range range.cmd) print)
      [%undo ~]
        ?:  =(buff.state oldbuff.state)
          ~&  >>  "Nothing to undo"  ~
        ``this(buff.state oldbuff.state, oldbuff.state buff.state)
      [%write *]  (biff (get-file path.cmd) write)
      [%extern *]  (extern cmd.cmd)
      [%text *]
        ?+  cmd-st.state  ~&  >>>  "Impossible state"  ~
        [%append *]  (append line.cmd-st.state text.cmd)
        [%insert *]  (insert line.cmd-st.state text.cmd)
        ==
      ==
    =/  print-cmd  (shoe-print (welp "> " cmd-tape))
    ?~  new  [print-cmd this]  [(welp print-cmd -.u.new) +.u.new]

  ::  State related element handlers
  ::
  ++  get-file
    |=  pax=(unit anypath)  ^-  (unit path)
    =/  pax=(unit path)  (bind pax resolve-path)
    =/  pax=(unit path)  ?~  pax  file.state  pax
    ?~  pax  ~&  >>>  "No current filename"  ~
    pax
  ++  actual-path
    |=  pax=path  ^-  path
    (en-beam:format [byk.bowl(r [%da now.bowl]) (flop pax)])
  ++  resolve-path
    |=  pax=anypath  ^-  path
    ?.  rel.pax  path.pax  ~&  >>>  "Relative paths not implemented yet"  path.pax
  ++  resolve-line-none                                       ::  Resolve parsed line, ignoring %none
    |=  line=line-ty  ^-  (unit @ud)
    ?:  =([[%none ~] ~] line)  `0
    =/  l=(unit @ud)
      ?-  head.line
      [%abs *]  `line.head.line
      [%current *]  `line.state
      [%mark *]
        =/  line  (~(get by mark.state) mark.head.line)
        ?~  line  ~&  >>>  "No such mark"  ~
        line
      [%last ~]  `(lent buff.state)
      [%none *]  `line.state
      ==
    =/  line=@sd  ?~  rel.line  (sun:si (need l))  (sum:si u.rel.line (sun:si (need l)))
    ?:  |(=(-1 (cmp:si line --1)) =(--1 (cmp:si line (sun:si (lent buff.state)))))
      ~&  >>>  "Invalid address"  ~
    `(abs:si line)
  ++  resolve-line                                            ::  Replace %none with current
    |=  line=line-ty  ^-  (unit @ud)
    =/  line=(unit @ud)  (resolve-line-none line)
    ?~  line  ~
    ?:  =(u.line 0)  `line.state
    line
  ++  resolve-range
    |=  range=frange  ^-  (unit range-ty)
    ?-  range
    [%line *]
      =/  line  (resolve-line line.range)
      ?~  line  ~
      `[u.line 1]
    [%range *]
      =+  [start=(resolve-line-none start.range) end=(resolve-line-none end.range)]
      ?~  start  ~  ?~  end  ~
      =+  [start=u.start end=u.end]
      ?:  =(start 0)
        ?:  =(end 0)
          `[1 (lent buff.state)]
        `[1 end]
      ?:  =(end 0)
        `[start 1]
      ?:  (gth start end)  ~&  >>>  "Invalid range"  ~
      `[start +((sub end start))]
    ==

  ++  append
    |=  [line=@ud t=(unit tape)]  ^-  (unit (quip card _this))
    ?~  t
      `[(prompt ": ") this(cmd-st.state [%norm ~], line.state line)]
    %-  some
    :-  ~
    %=  this
      buff.state  (into buff.state line (crip u.t))
      cmd-st.state  [%append +(line)]
    ==

  ++  change
    |=  range=range-ty  ^-  (unit (quip card _this))
    %-  some
    :-  (prompt ": change: ")
    %=  this
      cmd-st.state  [%insert (dec start.range)]
      buff.state  (oust [(dec start.range) len.range] buff.state)
      oldbuff.state  buff.state
    ==

  ++  delete
    |=  range=range-ty  ^-  (unit (quip card _this))
    =/  buff=wain  (oust [(dec start.range) len.range] buff.state)
    =/  bufflent  (lent buff)
    =/  line  ?:  (gth start.range bufflent)  bufflent  start.range
    `[~ this(line.state line, buff.state buff, oldbuff.state buff.state)]

  ++  edit
    |=  pax=path  ^-  (unit (quip card _this))
    =/  file  (bind (file:space:userlib (actual-path pax)) |=(a=* ;;(@t a)))
    ?~  file  ~&  >>>  "file doesn't exist"  ~
    =/  byte-len  (lent (trip u.file))
    =/  buff  (to-wain:format u.file)
    =/  lines  (lent buff)
    %-  some
    :-  (shoe-print "{<byte-len>} bytes, {<lines>} lines")
    %=  this
      buff.state  buff
      oldbuff.state  buff
      file.state  `pax
      mark.state  *(map char @ud)
    ==

  ++  file
    |=  file=path  ^-  (unit (quip card _this))
    `[(shoe-print <file>) this(file.state `file)]

  ++  insert
    |=  [line=@ud t=(unit tape)]  ^-  (unit (quip card _this))
    ?~  t
      `[(prompt ": ") this(cmd-st.state [%norm ~], line.state ?:(=(line 0) 1 line))]
    %-  some
    :-  ~
    this(buff.state (into buff.state line (crip u.t)), cmd-st.state [%insert +(line)])

  ++  goto
    |=  line=@ud  ^-  (unit (quip card _this))
    =.  line  ?:  =(0 line)  +(line.state)  line
    ?:  (gth line (lent buff.state))  ~&  >>>  "Invalid address"  ~
    =/  text=cord  (snag (dec line) buff.state)
    `[(shoe-fec (with-num:util line text)) this(line.state line)]

  ++  print
    |=  range=range-ty  ^-  (unit (quip card _this))
    ?:  =(len.range 0)  ~&  >>>  "Invalid range"  ``this
    =/  lines  (swag [(dec start.range) len.range] buff.state)
    =/  numbered-lines=(list [@ud cord])  (zip:util (gulf start.range (dec (add start.range len.range))) lines)
    `[(more-fec (turn numbered-lines with-num:util)) this(line.state (dec (add start.range len.range)))]

  ++  write
    |=  file=path  ^-  (unit (quip card _this))
    =/  lines  (lent buff.state)
    =/  bytes  (of-wain:format buff.state)
    =/  byte-len  (lent (trip bytes))
    %-  some
    :_  this
    :-
      ^-  card:agent:gall
      :*  %pass  /file  %arvo  %c
          %info
          (foal:space:userlib (en-beam:format [byk.bowl (flop file)]) [%noun !>((of-wain:format buff.state))])
      ==
    (shoe-print "{<byte-len>} bytes, {<lines>} lines written")

  ++  extern
    |=  cmd=ext-cmd-ty  ^-  (unit (quip card _this))
    ?-  cmd
    [%allow-remote *]  ``this(remote.state (~(put in remote.state) ship.cmd))
    [%remove-remote *]  ``this(remote.state (~(del in remote.state) ship.cmd))
    [%list-remote ~]  (some [(shoe-print <remote.state>) this])
    ==

  ++  util
    |%
    ++  with-num
      |=  [l=@ud t=cord]  ^-  sole-effect
      =/  l  (scow %ud l)
      =/  max-len  (lent (scow %ud (lent buff.state)))
      =/  this-len  (lent l)
      klr+~[[[`%br ~ `%g] (weld (weld (reap (sub max-len this-len) ' ') l) " ")] t]

    ++  zip
      |*  [a=(list) b=(list)]
      =/  out=(list [_-.a _-.b])  ~
      |-
      ?:  ?=([^ ^] [a b])
        $(out (snoc out [(head a) (head b)]), a (tail a), b (tail b))
      out
    --
  --

++  can-connect
  |=  sole-id=@ta  ^-  ?
  ?|
    =(our.bowl src.bowl)
    (~(has in remote.state) src.bowl)
  ==
++  on-connect      on-connect:des
++  on-disconnect   on-disconnect:des
--
