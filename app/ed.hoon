/+  shoe, verb, dbug, default-agent, *sole
|%
+$  anypath  [rel=? =path]
+$  cmd-st-ty
  $%
    [%norm ~]
    [%output ~]
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
    [%void ~]                             ::  It's here to prevent mint-vain on the handler; never constructed
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
    [%print range=frange]
    [%extern cmd=ext-cmd-ty]

    [%text text=(unit tape)]
    [%cont ~]
  ==
+$  card  card:shoe
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
%-  (agent:shoe cmd-ty)
^-  (shoe:shoe cmd-ty)
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
    des   ~(. (default:shoe this cmd-ty) bowl)
::
++  on-init   on-init:def
++  on-save   !>(state)
++  on-load
  |=  old=vase  ^-  (quip card _this)
  [~[shoe+[~ sole+pro+[%.y dap.bowl ": "]]] this(state !<(state-ty old))]
++  on-poke   on-poke:def
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-agent  on-agent:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
++  command-parser
  |=  sole-id=@ta
  ^+  |~(nail *(like [? cmd-ty]))
  %+  stag  %.n
  |^
    ?-  cmd-st.state
    [%norm ~]
    ;~  pose
      extern
      print
      edit
      change
      file
      goto
    ==
    [%append *]  (stag %text text:pars)
    [%insert *]  (stag %text text:pars)
    [%output ~]  (stag %cont empty:pars)
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
  ++  print
    %+  knee  *cmd-ty  |.  ~+
    %+  stag  %print
    ;~(sfix frange:pars ;~(plug ws:pars (just 'p')))
  ++  goto
    %+  knee  *cmd-ty  |.  ~+
    (stag %goto line:pars)
  ++  pars
    |%
    ++  ws  (cold ~ (star (just ' ')))
    ++  empty  (cold ~ (just ''))
    ++  path                                                  ::  Abs/Rel path
      %+  knee  *anypath  |.  ~+
      ;~  plug  ;~(pose (cold %.y cen) (easy %.n))
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
  |=  [sole-id=@ta cmd=cmd-ty]
  ~&  >  cmd
  |^  ^-  (quip card _this)
    ?+  cmd  ~&  >>>  "Unimplemented"  `this
    [%append *]
      =/  line  (resolve-line line.cmd)
      ?~  line  `this  `this(cmd-st.state [%append u.line])
    [%change *]
      =/  range  (resolve-range range.cmd)
      ?~  range  `this  (change u.range)
    [%delete *]
      =/  range  (resolve-range range.cmd)
      ?~  range  `this  (delete u.range)
    [%edit *]  (edit (bind path.cmd resolve-path))
    [%file *]  (file (bind path.cmd resolve-path))
    [%insert *]
      =/  line  (resolve-line line.cmd)
      ?~  line  `this  `this(cmd-st.state [%insert (dec u.line)])
    [%goto *]
      =/  line  (resolve-line-none line.cmd)
      ?~  line  `this  (goto u.line)
    [%print *]
      =/  range  (resolve-range range.cmd)
      ?~  range  `this  (print u.range)
    [%extern *]  (extern cmd.cmd)
    [%text *]
      ?+  cmd-st.state  ~&  >>>  "Impossible state"  `this
      [%append *]  (append line.cmd-st.state text.cmd)
      [%insert *]  (insert line.cmd-st.state text.cmd)
      ==
    ==
  ::  State related element handlers
  ::
  ++  resolve-path
    |=  pax=anypath
    ?.  rel.pax  path.pax  (weld /(scot %p our.bowl)/[q.byk.bowl]/(scot %da now.bowl) path.pax)
  ++  resolve-line-none                                       ::  Resolve parsed line, ignoring %none
    |=  line=line-ty  ^-  (unit @ud)
    ?:  =([[%none ~] ~] line)  `0
    =/  l=@ud
      ?-  head.line
      [%abs *]  line.head.line
      [%current *]  line.state
      [%last ~]  (lent buff.state)
      [%none *]  line.state
      ==
    =/  line=@sd  ?~  rel.line  (sun:si l)  (sum:si u.rel.line (sun:si l))
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
      ?~  start  ~  ?~  end    ~
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
    |=  [line=@ud t=(unit tape)]
    ?~  t
      [~[shoe+[~ sole+txt+"."]] this(cmd-st.state [%norm ~], line.state line)]
    [~[shoe+[~ sole+txt+u.t]] this(buff.state (into buff.state line (crip u.t)), cmd-st.state [%append +(line)])]

  ++  change
    |=  range=range-ty  ^-  (quip card _this)
    `this(cmd-st.state [%insert (dec start.range)], buff.state (oust [(dec start.range) len.range] buff.state))

  ++  delete
    |=  range=range-ty  ^-  (quip card _this)
    =/  buff=wain  (oust [(dec start.range) len.range] buff.state)
    =/  bufflent  (lent buff)
    =/  line  ?:  (gth start.range bufflent)  bufflent  start.range
    `this(line.state line, buff.state buff)

  ++  edit
    |=  pax=(unit path)  ^-  (quip card _this)
    =.  pax  ?~  pax  file.state  pax
    ~&  pax
    ?~  pax  ~&  >>>  "No current filename"  `this
    =/  finfo  .^(arch %cy u.pax)
    ?~  fil.finfo
      ?~  dir.finfo
        ~&  >>>  "file doesn't exist"  `this
      ~&  >>  "directory not supported yet"  `this
    =/  buff  (to-wain:format .^(cord %cx u.pax))
    [~ this(buff.state (scag (dec (lent buff)) buff), file.state pax)]

  ++  file
    |=  file=(unit path)  ^-  (quip card _this)
    =.  file  ?~  file  file.state  file
    ?~  file  ~&  >>>  "No current filename"  `this
    [~[shoe+[~ sole+txt+<u.file>]] this(file.state file)]

  ++  insert
    |=  [line=@ud t=(unit tape)]
    ?~  t
      [~[shoe+[~ sole+txt+"."]] this(cmd-st.state [%norm ~], line.state ?:(=(line 0) 1 line))]
    [~[shoe+[~ sole+txt+u.t]] this(buff.state (into buff.state line (crip u.t)), cmd-st.state [%insert +(line)])]

  ++  goto
    |=  line=@ud  ^-  (quip card _this)
    =.  line  ?:  =(0 line)  +(line.state)  line
    ?:  (gth line (lent buff.state))  ~&  >>>  "Invalid address"  `this
    =/  text=cord  (snag (dec line) buff.state)
    [~[shoe+[~ sole+(with-num:util line text)]] this(line.state line)]

  ++  print
    |=  range=range-ty  ^-  (quip card _this)
    =/  lines  (swag [(dec start.range) len.range] buff.state)
    =/  numbered-lines=(list [@ud cord])  (zip:util (gulf start.range (dec (add start.range len.range))) lines)
    [~[shoe+[~ sole+mor+(turn numbered-lines with-num:util)]] this(line.state (dec (add start.range len.range)))]

  ++  extern
    |=  cmd=ext-cmd-ty  ^-  (quip card _this)
    ?+  cmd  ~&  >>>  "Unknown command"  `this
    [%allow-remote *]  `this(remote.state (~(put in remote.state) ship.cmd))
    [%remove-remote *]  `this(remote.state (~(del in remote.state) ship.cmd))
    ==

  ++  util
    |%
    ++  with-num
      |=  [l=@ud t=cord]  ^-  sole-effect
      =/  l  (scow %ud l)
      =/  max-len  (lent (scow %ud (lent buff.state)))
      =/  this-len  (lent l)
      klr+~[[[`%br ~ `%g] (weld (weld (reap (sub max-len this-len) ' ') l) (reap (sub 8 (lent l)) ' '))] t]

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
