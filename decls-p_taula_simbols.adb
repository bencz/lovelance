package body decls.p_taula_simbols is

    procedure tbuida (ts:    out t_taula_simbols) is
        d : descr;
    begin        
        for i in id_nom'first..id_nom'last loop
            ts.tdc(i) := (0, d, 0);
        end loop;
        ts.nprof := 1;
    end tbuida;


    procedure posa (ts: in out t_taula_simbols;
                    id: in     id_nom;
                    d : in     descr;
                    e :    out boolean) is
        error : exception;
    begin
        e := false;
        if ts.tdc(id).p = ts.nprof then
            raise error;
        end if;

        ts.ta(ts.nprof) := ts.ta(ts.nprof) + 1;
        ts.ndespl := ts.ta(ts.nprof);
        ts.tdp(ts.ndespl) := (id, ts.tdc(id).p, ts.tdc(id).d, 0);
        ts.tdc(id) := (ts.nprof, d, 0);

        exception
            when error => e := true;
    end posa;


    function cons(ts: in    t_taula_simbols;
                  id: in id_nom) return descr is
    begin
        return ts.tdc(id).d;
    end cons;

    function consprof(ts : in t_taula_simbols) return nambits is
    begin
        return ts.nprof;
    end consprof;


    procedure entrabloc (ts: in out t_taula_simbols) is
    begin
        ts.nprof := ts.nprof + 1;
        ts.ta(ts.nprof) := ts.ta(ts.nprof - 1);
    end entrabloc;


    procedure surtbloc (ts : in out t_taula_simbols) is
        idespl1, idespl2 : ndesplacament;
        id : id_nom;
    begin
        if ts.nprof <= 1 then 
            raise error_surtbloc;
        end if;
        
        idespl1 := ts.ta(ts.nprof);
        ts.nprof := ts.nprof - 1;
        idespl2 := ts.ta(ts.nprof);
        
        while idespl1 > idespl2 loop
            if ts.tdp(idespl1).p /= -1 then
                id := ts.tdp(idespl1).id;
                ts.tdc(id).p := ts.tdp(idespl1).p;
                ts.tdc(id).d := ts.tdp(idespl1).d;
            end if;
            idespl1 := idespl1 - 1;
        end loop;
    end surtbloc;
    

    procedure posacamp (ts       : in out t_taula_simbols;
                        idr, idc : in     id_nom;
                        dc       : in     descr;
                        e        :    out boolean) is
        desc   : descr;
        pdesp  : ndesplacament;
        error  : exception;
    begin
        e := false;

        desc := ts.tdc(idr).d;
        if desc.td /= dtipus or desc.dt.ts /= tsrec then
            raise error;
        end if;
             
        pdesp := ts.tdc(idr).s;
        while pdesp /= 0 and ts.tdp(pdesp).id /= idc loop
            pdesp := ts.tdp(pdesp).s;
        end loop;
            
        if pdesp /= 0 then
            raise error;
        end if;
        
        ts.ta(ts.nprof) := ts.ta(ts.nprof) + 1;
        ts.ndespl := ts.ta(ts.nprof);
        ts.tdp(ts.ndespl) := (id => idc,
                              p  => 0,
                              d  => dc,
                              s  => ts.tdc(idr).s);                
        ts.tdc(idr).s := ts.ndespl;
        
        exception
            when error => e := true;
    end posacamp;

    

    function conscamp (ts       : in     t_taula_simbols;
                       idr, idc : in     id_nom) return descr is
        dr, dc  : descr;
        pdesp   : ndesplacament;
    begin
        dr := ts.tdc(idr).d;
        if dr.td /= dtipus or dr.dt.ts /= tsrec then
            raise error_consultacamp;
        end if;
        
        pdesp := ts.tdc(idr).s;
        while pdesp /= 0 and ts.tdp(pdesp).id /= idc loop
            pdesp := ts.tdp(pdesp).s;
        end loop;
        
        if pdesp = 0 then
            dc := (td => dnul_la);
        else
            dc := ts.tdp(pdesp).d;
        end if;
        return dc;
    end conscamp;
    

    procedure posaindex (ts       : in out t_taula_simbols;
                         ida, idi : in     id_nom;
                         d        : in     descr) is
        desc   : descr;
        pdesp  : ndesplacament;
        pdespp : ndesplacament;
    begin
        desc := ts.tdc(ida).d;                
        if not (desc.td = dtipus and then desc.dt.ts = tsarr) then
            raise error_posaindex;
        end if;
        
        pdesp  := ts.tdc(ida).s;
        pdespp := 0;
        
        while pdesp /= 0 loop
            pdespp := pdesp;
            pdesp  := ts.tdp(pdesp).s;
        end loop;
        
        ts.ta(ts.nprof) := ts.ta(ts.nprof) + 1;
        pdesp := ts.ta(ts.nprof);
        ts.tdp (pdesp) := (id => idi,
                           p  => 0,
                           d  => d,
                           s  => 0);
        
        if pdespp = 0 then
            ts.tdc(ida).s := pdesp;
        else
            ts.tdp(pdespp).s := pdesp;
        end if;  
    end posaindex;


    procedure consindex (ts      : in     t_taula_simbols;
                         indxarr : in     indexarr;
                         ida     :    out id_nom;
                         d       :    out descr) is
    begin
        ida := ts.tdp(indxarr).id;
        d   := ts.tdp(indxarr).d;
    end consindex;


    function primerindex (ts : in     t_taula_simbols;
                          ida: in     id_nom) return indexarr is
    begin
        return ts.tdc(ida).s;
    end;
    

    function succindex (ts: in     t_taula_simbols;
                        ci: in     indexarr) return indexarr is
    begin
        if esvalidarr(ci) then
            return ts.tdp(ci).s;
        else
            raise error_succindex;
        end if;
    end succindex;
    

    function esvalidarr (indxarr : in     indexarr) return boolean is
    begin
        return indxarr /= 0;
    end esvalidarr;
    
    procedure posaarg(ts       : in out t_taula_simbols;
                      idp, ida : in     id_nom;
                      d        : in     descr;
                      e        :    out boolean) is
        desc   : descr;
        pdesp  : ndesplacament;
        pdespp : ndesplacament;
        error  : exception;
    begin
        e := false;
    
        desc := ts.tdc(idp).d;
        if desc.td /= dproc then
            raise error;
        end if;
        
        pdesp  := ts.tdc(idp).s;
        pdespp := 0;
        while pdesp /= 0 and ts.tdp(pdesp).id /= ida loop
            pdespp := pdesp;
            pdesp  := ts.tdp(pdesp).s;
        end loop;
        
        if pdesp /= 0 then
            raise error;
        end if;
        
        ts.ta(ts.nprof) := ts.ta(ts.nprof) + 1;
        pdesp := ts.ta(ts.nprof);
        ts.tdp(pdesp) := (ida, 0, d, 0);
        
        if pdespp = 0 then
            ts.tdc(idp).s := pdesp;
        else
            ts.tdp(pdespp).s := pdesp;
        end if;      
        exception
            when error => e := true;
    end posaarg;


    function primerarg(ts  : in     t_taula_simbols;
                       idp : in     id_nom) return indexarg is
    begin
        return ts.tdc(idp).s;
    end primerarg;
    

    function succarg(ts      : in     t_taula_simbols;
                     indxarg : in     indexarg) return indexarg is
    begin
        if esvalidarr(indxarg) then
            return ts.tdp(indxarg).s;
        else
            raise error_succarg;
        end if;
    end succarg;


    procedure consarg(ts      : in     t_taula_simbols;
                      indxarg : in     indexarg;
                      ida     :    out id_nom;
                      d       :    out descr) is
    begin
        ida := ts.tdp(indxarg).id;
        d   := ts.tdp(indxarg).d;
    end consarg;
      

    function esvalidarg (indxarg : in     indexarg) return boolean is
    begin
        return indxarg /= 0;
    end esvalidarg;


    procedure actualitza(ts : in out t_taula_simbols;
                         id : in     id_nom;
                         d  : in     descr) is
    begin
        ts.tdc(id).d := d;
    end actualitza;
    
    procedure actualitza_arg (ts   : in out t_taula_simbols; 
                              inda : in     indexarg; 
                              da   : in     descr) is
    begin
        ts.tdp(inda).d := da;                              
    end actualitza_arg;
    
    procedure imprimir (ts : in t_taula_simbols) is
        ftd  : file_type; -- Taula descripcio
        ftdp : file_type; -- Taula desplaçament
        fa   : file_type; -- Taula d'ambits
        d : descr;
    begin  	
        create(ftd , out_file, "Ts_Taula_Descripcio.txt");
        create(ftdp, out_file, "Ts_Taula_Desplaçament.txt");
        create(fa, out_file, "Ts_Taula_Ambit.txt");

        for i in ts.tdc'first..ts.tdc'last loop
            put(ftd, i'img & " :___");
            put(ftd, "p: " & ts.tdc(i).p'img & "   ");
            put(ftd, "d: " & "   ");
            d := ts.tdc(i).d;
            case d.td is
              when dnul_la => put(ftd, "dnul_la   ");
                              put(ftd, "null");
              when dconst  => put(ftd, "dconst   ");
                              put(ftd, "tc: " & d.tc'img & "   ");
                              put(ftd, "vc: " & d.vc'img & "   ");
                              put(ftd, "nc: " & d.nc'img);
              when dtipus  => 
                 put(ftd, "ocup: " & d.dt.ocup'img & "   ");
                 put(ftd, "dtipus: ");
                 case d.dt.ts is
                   when tsnul    => put(ftd, "tsnull   ");
                                    put(ftd, "null");
                   when tsbool |
                        tscar  |
                        tsent    => put(ftd, "tsbool | tscar | tsent   ");
                                    put(ftd, "linf: " & d.dt.linf'img & "   ");
                                    put(ftd, "lsup: " & d.dt.lsup'img); 
                   when tsarr    => put(ftd, "tsarr   ");
                                    put(ftd, "b: " & d.dt.b'img & "   ");
                                    put(ftd, "tcomp: " & d.dt.tcomp'img);
                   when tsrec    => put(ftd, "tsrec   ");
                                    put(ftd, "null");
                   when tsstr    => put(ftd, "tsstr   ");
                                    put(ftd, "null");
                 end case;
              when dvar    => put(ftd, "dvar   ");
                              put(ftd, "tv: " & d.tv'img & "   ");
                              put(ftd, "nv: " & d.nv'img);
              when dproc   => put(ftd, "dproc   ");
                              put(ftd, "np: " & d.np'img);
              when dcamp   => put(ftd, "dcamp   ");
                              put(ftd, "tcp: " & d.tcp'img & "   ");
                              put(ftd, "dsp: " & d.dsp'img);
              when darg    => put(ftd, "darg   ");
                              put(ftd, "ta: " & d.ta'img & "   ");
                              put(ftd, "na: " & d.na'img);
            end case;
            put_line(ftd, "   s: " & ts.tdc(i).s'img);
        end loop;

        for i in ts.tdp'first..ts.tdp'last loop
            put(ftdp, i'img & " :___");
            put(ftdp, "id: " & ts.tdp(i).id'img & "   ");
            put(ftdp, "p: " & ts.tdp(i).p'img & "   ");
            put(ftdp, "d: " & "   ");
            d := ts.tdp(i).d;
            case d.td is
              when dnul_la => put(ftdp, "dnul_la   ");
                              put(ftdp, "null");
              when dconst  => put(ftdp, "dconst   ");
                              put(ftdp, "tc: " & d.tc'img & "   ");
                              put(ftdp, "vc: " & d.vc'img & "   ");
                              put(ftdp, "nc: " & d.nc'img);
              when dtipus  => 
                put(ftdp, "ocup: " & d.dt.ocup'img & "   ");
                put(ftdp, "dtipus: ");
                case d.dt.ts is
                  when tsnul    => put(ftdp, "tsnull   ");
                                   put(ftdp, "null");
                  when tsbool |
                       tscar  |
                       tsent    => put(ftdp, "tsbool | tscar | tsent   ");
                                   put(ftdp, "linf: " & d.dt.linf'img & "   ");
                                   put(ftdp, "lsup: " & d.dt.lsup'img); 
                  when tsarr    => put(ftdp, "tsarr   ");
                                   put(ftdp, "b: " & d.dt.b'img & "   ");
                                   put(ftdp, "tcomp: " & d.dt.tcomp'img);
                  when tsrec    => put(ftdp, "tsrec   ");
                                   put(ftdp, "null");
                  when tsstr    => put(ftdp, "tsstr   ");
                                   put(ftdp, "null");
                end case;
              when dvar    => put(ftdp, "dvar   ");
                              put(ftdp, "tv: " & d.tv'img & "   ");
                              put(ftdp, "nv: " & d.nv'img);
              when dproc   => put(ftdp, "dproc   ");
                              put(ftdp, "np: " & d.np'img);
              when dcamp   => put(ftdp, "dcamp   ");
                              put(ftdp, "tcp: " & d.tcp'img & "   ");
                              put(ftdp, "dsp: " & d.dsp'img);
              when darg    => put(ftdp, "darg   ");
                              put(ftdp, "ta: " & d.ta'img & "   ");
                              put(ftdp, "na: " & d.na'img);
            end case;
            put_line(ftdp, "   s: " & ts.tdp(i).s'img);
        end loop;
        
        for i in ts.ta'first..ts.ta'last loop
            put_line(fa, i'img & " :___" & ts.ta(i)'img);
        end loop;

        close(ftd);
        close(ftdp);
        close(fa);
    end imprimir;

end decls.p_taula_simbols;
