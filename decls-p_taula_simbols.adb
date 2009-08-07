--with ada.Text_IO; use ada.Text_IO;
package body decls.p_taula_simbols is

    procedure tbuida (ts:    out t_taula_simbols) is
        d : descr;
    begin
        ts.nprof := 1;
        ts.ta(ts.nprof) := 0;

        for i in id_nom'first..id_nom'last loop
            ts.tdc(i) := (0, d, 0);
        end loop;
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

------------ SURTBLOC ORIGINAL ------------
--    procedure surtbloc (ts : in out t_taula_simbols) is
--        idespl1, idespl2 : ndesplacament;
--        id : id_nom;
--    begin
--        if ts.nprof = 1 then
--            raise error_surtbloc;
--        end if;
--        
--        idespl1 := ts.ta(ts.nprof);
--        ts.nprof := ts.nprof - 1;
--        idespl2 := ts.ta(ts.nprof) + 1;
--                
--        for i in reverse idespl1..idespl2 loop
--            if ts.tdp(i).p /= -1 then
--                id := ts.tdp(i).id;
--                ts.tdc(ts.tdp(i).id).p := ts.tdp(i).p;
--                ts.tdc(ts.tdp(i).id).d := ts.tdp(i).d;
--            end if;
--        end loop;
--    end surtbloc;


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

------------ ORIGINAL -------------------
--    procedure posacamp (ts       : in out t_taula_simbols;
--                        idr, idc : in     id_nom;
--                        d        : in     descr;
--                        e        :    out boolean) is
--        desc   : descr;
--        desc_t : descr_tipus;
--        pdesp  : ndesplacament;
--        error  : exception;
--    begin
--        e := false;
--    
--        desc := ts.tdc(idr).d;
--        if desc.td /= dtipus then
--            raise error;
--        end if;
--        
--        desc_t := desc.dt;
--        if desc_t.ts /= tsrec then
--            raise error;
--        end if;
--        
--        pdesp := ts.tdc(idr).s;
--        while pdesp /= 0 and ts.tdp(pdesp).id /= idc loop
--            pdesp := ts.tdp(pdesp).s;
--        end loop;
--            
--        if pdesp /= 0 then
--            raise error;
--        end if;

--        ts.ta(ts.nprof) := ts.ta(ts.nprof) + 1;
--        ts.ndespl := ts.ta(ts.nprof);
--        ts.tdp(ts.ndespl) := (idc, -1, d, ts.tdc(idr).s);
--        ts.tdc(idr).s := ts.ndespl;

--        exception
--            when error => e := true;
--    end posacamp;
    

    procedure posacamp (ts       : in out t_taula_simbols;
                        idr, idc : in     id_nom;
                        d        : in     descr;
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
                              p  => -1,
                              d  => d,
                              s  => ts.tdc(idr).s);                
        ts.tdc(idr).s := ts.ndespl;
        
        exception
            when error => e := true;
    end posacamp;

    

    function conscamp (ts       : in     t_taula_simbols;
                       idr, idc : in     id_nom) return descr is
        desc   : descr;
        pdesp  : ndesplacament;
    begin
        desc := ts.tdc(idr).d;
        if desc.td /= dtipus or desc.dt.ts /= tsrec then
            raise error_consultacamp;
        end if;
        
        pdesp := ts.tdc(idr).s;
        while pdesp /= 0 and ts.tdp(pdesp).id /= idc loop
            pdesp := ts.tdp(pdesp).s;
        end loop;
        
        if pdesp = 0 then
            return (td => dnul_la);
        else
            return ts.tdp(pdesp).d;
        end if;
    end conscamp;

--------------- ORIGINAL --------------------------
--    procedure posaindex (ts      : in out t_taula_simbols;
--                         ida, idi: in     id_nom) is
--        desc   : descr;
--        descn  : descr;
--        desc_t : descr_tipus;
--        pdesp  : ndesplacament;
--        pdespp : ndesplacament;
--    begin
--        desc := ts.tdc(ida).d;
--        if desc.td /= dtipus then
--            raise error_posaindex;
--        end if;
--        
--        desc_t := desc.dt;
--        
--        if desc_t.ts /= tsarr then
--            raise error_posaindex;
--        end if;
--        
--        pdesp  := ts.tdc(ida).s;
--        pdespp := 0;
--        while pdesp /= 0 loop
--            pdespp := pdesp;
--            pdesp  := ts.tdp(pdesp).s;
--        end loop;
--        
--        ts.ta(ts.nprof) := ts.ta(ts.nprof) + 1;
--        ts.ndespl := ts.ta(ts.nprof);
--        ts.tdp (ts.ndespl) := (idi, -1, descn, 0);
--        
--        if pdespp = 0 then
--            ts.tdc(ida).s := ts.ndespl;
--        else
--            ts.tdp(pdespp).s := ts.ndespl;
--        end if;
--    end posaindex;
    

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
                           p  => -1,
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
        ts.ndespl := ts.ta(ts.nprof);
        ts.tdp (ts.ndespl) := (ida, -1, d, 0);
        
        if pdespp = 0 then
            ts.tdc(idp).s := ts.ndespl;
            --ts.tdc(idp).s := pdesp;
        else
            ts.tdp(pdespp).s := ts.ndespl;
            --ts.tdp(pdespp).s := pdesp;
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
        return ts.tdp(indxarg).s;
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

end decls.p_taula_simbols;
