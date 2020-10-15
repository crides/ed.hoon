/+  shoe, verb, dbug, default-agent, *sole
|%
+$  anypath  [rel=? =path]
+$  state-ty
  $:
    file=(unit path)
    line=@ud
    buff=wain
    oldbuff=wain
    mark=(map char @ud)
    cmd-st=?(%norm %edit %output)
    lastpat=cord
    remote=(set @p)
  ==
+$  line-ty
  $%
    [%abs line=@ud]
    [%rel diff=@sd]
    [%last ~]
    [%none ~]
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
    [%edit path=(unit anypath)]
    [%file path=(unit anypath)]
    [%goto line=line-ty]
    [%insert line=line-ty]
    [%print range=frange]
    [%extern cmd=ext-cmd-ty]

    [%text text=tape]
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
    cmd-st=%norm
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
      %norm
      ;~  pose
        extern
        print
        edit
        change
        file
        goto
      ==
      %edit  (stag %text text:pars)
      %output  (stag %cont empty:pars)
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
      ;~  pose
        (cold rel+--0 dot)                                    ::  Current line
        (cold last+~ buc)                                     ::  Last line
        (stag %abs dem)                                       ::  Absolute line
        ;~  pfix  lus                                         ::  Relative after
          %+  stag  %rel
          ;~  pose
            (cook sun:si dem)
            (easy --1)
          ==
        ==
        ;~  pfix  hep                                         ::  Relative before
          %+  stag  %rel
          ;~  pose
            %+  cook  |=(d=@ud (dif:si --0 (sun:si d)))  dem
            (easy -1)
          ==
        ==
        (easy none+~)
      ==
    ++  text  (star prn)
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
    ?+  cmd  ~&  >>>  "Unimplemented"  [~ this]
      [%edit *]  (edit (bind path.cmd resolve-path))
      [%goto *]
        =/  line  (resolve-line-none line.cmd)
        ?~  line  `this  (goto u.line)
      [%print *]
        =/  range  (resolve-range range.cmd)
        ?~  range  `this  (print u.range)
      [%file *]  (file (bind path.cmd resolve-path))
      [%extern *]  (extern cmd.cmd)
    ==
  ::  State related element handlers
  ::
  ++  resolve-path
    |=  pax=anypath
    ?.  rel.pax  path.pax  (weld /(scot %p our.bowl)/[q.byk.bowl]/(scot %da now.bowl) path.pax)
  ++  resolve-line-none
    |=  line=line-ty  ^-  (unit @ud)
    ?:  =(%none -.line)  `0
    =/  line=@sd
      ?-  line
        [%abs *]  (sun:si line.line)
        [%rel *]  (sum:si diff.line (sun:si line.state))
        [%last ~]  (sun:si (lent buff.state))
        [%none *]  ~|  "???"  !!
      ==
    ?:  |(=(-1 (cmp:si line --1)) =(--1 (cmp:si line (sun:si (lent buff.state)))))
      ~&  >>>  "Invalid address"  ~
    `(abs:si line)
  ++  resolve-line
    |=  line=line-ty  ^-  (unit @ud)
    =/  line=(unit @ud)  (resolve-line-none line)
    ?~  line  ~
    ?:  =(u.line 0)  `(lent buff.state)
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
      ?:  (gth start end)  ~&  >>>  "Invalid address"  ~
      `[start +((sub end start))]
    ==

  ++  extern
    |=  cmd=ext-cmd-ty  ^-  (quip card _this)
    ?+  cmd  ~&  >>>  "Unknown command"  `this
      [%allow-remote *]  `this(remote.state (~(put in remote.state) ship.cmd))
      [%remove-remote *]  `this(remote.state (~(del in remote.state) ship.cmd))
    ==

  ++  file
    |=  file=(unit path)  ^-  (quip card _this)
    =.  file  ?~  file  file.state  file
    ?~  file  ~&  >>>  "No current filename"  `this
    [~[shoe+[~ sole+txt+<u.file>]] this(file.state file)]

  ++  goto
    |=  line=@ud  ^-  (quip card _this)
    =.  line  ?:  =(0 line)  +(line.state)  line
    ?:  (gte line (lent buff.state))  ~&  >>>  "Invalid address"  `this
    =/  text=tape  (trip (snag (dec line) buff.state))
    [~[shoe+[~ sole+txt+text]] this(line.state line)]

  ++  print
    |=  range=range-ty  ^-  (quip card _this)
    =/  texts=(list sole-effect)  (turn (swag [(dec start.range) len.range] buff.state) |=(t=cord txt+(trip t)))
    [~[shoe+[~ sole+mor+texts]] this(line.state (dec (add start.range len.range)))]

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
