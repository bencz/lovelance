with p_semantica.missatges, ada.text_io;

use  p_semantica.missatges, ada.text_io;

package body p_semantica.ctipus is

    procedure ct_prog (decl_prog : in     t_atribut;
                       error     : in out boolean) is
    -- PROG:
    --     DECL_PROG
    
        indxarg : indexarg;
    begin
        if decl_prog.id = ID_NUL then
            raise e_previ;
        end if;
    
        indxarg := primerarg (ts, decl_prog.id);
        if esvalidarg (indxarg) then
            -- Programa principal amb arguments
            raise err;
        end if;
        
        exception
            when err     =>  me_args_princ(decl_prog.linia,
                                           decl_prog.columna);
                             error := true;
            when e_previ => null;
    end ct_prog;
    

    procedure ct_decl_prog (decl_prog : in out t_atribut;
                            encap     : in     t_atribut;
                            ident     : in     t_atribut;
                            error     : in out boolean) is
    -- DECL_PROG:
    --     pc_procedure ENCAP pc_is
    --       DECLS
    --     pc_begin M_DECL_PROG
    --       SENTS
    --     pc_end identificador s_punticoma
    begin
        decl_prog := (linia   => encap.linia,
                      columna => encap.columna,
                      a       => id,
                      id      => encap.id);
        
        if decl_prog.id = ID_NUL then
            raise e_previ;
        end if;
    
        surtbloc (ts);
        if ident.id /= encap.id then
            -- Identificadors diferents
            raise err;
        end if;
        
        exception
            when err     => me_difer_id (decl_prog.linia,
                                         decl_prog.columna,
                                         decl_prog.id,
                                         ident.id);
                            error := true;
            when e_previ => null;
    end ct_decl_prog; 
    

    procedure ct_encap_id (encap : in out t_atribut;
                           iden  : in     t_atribut;
                           error : in out boolean) is
    -- ENCAP:
    --     identificador
    
        dp : descr;
        e  : boolean;
    begin
        encap := (linia   => iden.linia,
                  columna => iden.columna,
                  a       => id,
                  id      => ID_NUL);
    
        np := np + 1;
        dp := (td => dproc,
               np => np);
        posa (ts, iden.id, dp, e);
        if e then
            -- Identificador existent
            raise err;
        end if;
        
        encap.id := iden.id;
        entrabloc(ts);
        
        exception
            when err => me_id_exist (iden.linia,
                                     iden.columna,
                                     iden.id);
                        error := true;
    end ct_encap_id;
    

    procedure ct_encap_p_encap (encap   : in out t_atribut;
                                p_encap : in     t_atribut) is
    -- ENCAP:
    --     P_ENCAP s_partancat
    
        indxarg : indexarg;
        darg    : descr;
        idarg   : id_nom;
        e       : boolean;
    begin
        entrabloc(ts);
        encap := (linia   => p_encap.linia,
                  columna => p_encap.columna,
                  a       => id,
                  id      => p_encap.id);
        
        if encap.id = ID_NUL then
            raise e_previ;
        end if;
        
        indxarg := primerarg (ts, p_encap.id);
        
        while esvalidarg (indxarg) loop
            nv := nv + 1;
            consarg (ts, indxarg, idarg, darg);
            
            if darg.td = dvar then
                darg.nv := nv;
            else
                darg.na := nv;
            end if;
            
            posa (ts, idarg, darg, e);
            actualitza (ts, idarg, darg);
            indxarg := succarg (ts, indxarg);
        end loop;
        
        exception
            when e_previ => null;
    end ct_encap_p_encap;
    

    procedure ct_p_encap_id (p_encap : in out t_atribut;
                             iden    : in     t_atribut;
                             argum   : in     t_atribut;
                             error   : in out boolean) is
    -- P_ENCAP:
    --     identificador s_parobert ARGUMENT
    
        dp, dtarg, dcarg : descr;
        e : boolean;
    begin
        p_encap := (linia   => iden.linia,
                    columna => iden.columna,
                    a       => id,
                    id      => iden.id);
    
        np := np + 1;
        dp := (td => dproc,
               np => np);
        posa (ts, iden.id, dp, e);
        
        if e then
            -- Identificador existent
            me_id_exist (iden.linia,
                         iden.columna,
                         iden.id);
            raise err;
        end if;
        
        dtarg := cons (ts, argum.arg_idt);
        if dtarg.td /= dtipus then
            -- No existeix el tipus
            me_no_tipus (iden.linia,
                         iden.columna,
                         argum.arg_idt);
            raise err;
        end if;
        
        case argum.arg_mde is
            when mdout    |
                 mdinout    => dcarg := (td => dvar,
                                         tv => argum.arg_idt,
                                         nv => 0);
            when mdin      => dcarg := (td => darg,
                                        ta => argum.arg_idt,
                                        na => 0);
        end case;
        
        posaarg (ts, iden.id, argum.arg_idarg, dcarg, e);
        if e then
            -- Argument existent
            me_arg_exist (argum.linia,
                          argum.columna,
                          argum.arg_idarg,
                          iden.id);
            raise err;
        end if;
        
        exception
            when err => error := true;
    end ct_p_encap_id;
    

    procedure ct_p_encap_rec (p_encap0 : in out t_atribut;
                              p_encap1 : in     t_atribut;
                              argum    : in     t_atribut;
                              error    : in out boolean) is
    -- P_ENCAP:
    --     P_ENCAP s_punticoma ARGUMENT
        dcarg, dtarg : descr;
        e            : boolean;
    begin
        p_encap0 := (linia   => p_encap1.linia,
                     columna => p_encap1.columna,
                     a       => id,
                     id      => p_encap1.id);
                     
        if p_encap0.id = ID_NUL then
            raise e_previ;
        end if;
    
        dtarg := cons (ts, argum.arg_idt);
        if dtarg.td /= dtipus then
            -- No existeix el tipus
            me_no_tipus (p_encap1.linia,
                         p_encap1.columna,
                         argum.arg_idt);
            raise err;
        end if;
        
        case argum.arg_mde is
            when mdout    |
                 mdinout   => dcarg := (td => dvar,
                                        tv => argum.arg_idt,
                                        nv => 0);
            when mdin      => dcarg := (td => darg,
                                        ta => argum.arg_idt,
                                        na => 0);
        end case;
        
        posaarg (ts, p_encap0.id, argum.arg_idarg, dcarg, e);
        if e then
            -- Argument existent
            me_arg_exist (p_encap1.linia,
                          p_encap1.columna,
                          argum.arg_idarg,
                          p_encap1.id);
            raise err;
        end if;

        exception
            when err     => error := true;
            when e_previ => null;
    end ct_p_encap_rec;
    

    procedure ct_argument (argum : in out t_atribut;
                           iden0 : in     t_atribut;
                           mode  : in     t_atribut;
                           iden1 : in     t_atribut) is
    -- ARGUMENT:
    --     identificador s_dospunts MODE identificador
    begin
        argum := (linia     => iden0.linia,
                  columna   => iden0.columna,
                  a         => arg,
                  arg_idarg => iden0.id,
                  arg_idt   => iden1.id,
                  arg_mde   => mode.mode_mde);
    end ct_argument;
    

    procedure ct_mode_in (mde : in out t_atribut) is
    -- MODE:
    --     pc_in
    begin
        mde := (linia    => 0,
                columna  => 0,
                a        => mode,
                mode_mde => mdin);
    end ct_mode_in;
    

    procedure ct_mode_out (mde  : in out t_atribut) is
    -- MODE:
    --     pc_out
    begin
        mde := (linia    => 0,
                columna  => 0,
                a        => mode,
                mode_mde => mdout);
    end ct_mode_out;
    

    procedure ct_mode_in_out (mde  : in out t_atribut) is
    -- MODE:
    --     pc_in pc_out
    begin
        mde := (linia    => 0,
                columna  => 0,
                a        => mode,
                mode_mde => mdinout);
    end ct_mode_in_out;
    

    procedure ct_decl_var (decl_var   : in out t_atribut;
                           iden       : in     t_atribut;
                           c_decl_var : in     t_atribut;
                           error      : in out boolean) is
    -- DECL_VAR:
    --     identificador C_DECL_VAR
    
        e  : boolean;
        dv : descr;
    begin
        if c_decl_var.var_idt = ID_NUL then
            raise e_previ;
        end if;
        
        nv := nv + 1;
        dv := (dvar, c_decl_var.var_idt, nv);
        posa(ts, iden.id, dv, e);
        
        if e then
            -- Identificador existent
            raise err;
        end if;
        
        exception
            when err     => error := true;
                            me_id_exist(iden.linia,
                                        iden.columna,
                                        iden.id);
            when e_previ => null;
    end ct_decl_var;
    

    procedure ct_c_decl_var (c_decl_var : in out t_atribut;
                             iden       : in     t_atribut;
                             error      : in out boolean) is
    -- C_DECL_VAR:
    --     s_dospunts identificador s_punticoma
    
        d : descr;
    begin
        c_decl_var := (linia   => iden.linia,
                       columna => iden.columna,
                       a       => var,
                       var_idt => iden.id);
                       
        d := cons(ts, iden.id);
        if d.td /= dtipus then
            -- No existeix el tipus
            raise err;
        end if;
        
        c_decl_var.var_idt := iden.id;
        
        exception
            when err => error := true;
                        c_decl_var.var_idt := ID_NUL;
                        me_no_tipus(iden.linia,
                                    iden.columna,
                                    iden.id);
    end ct_c_decl_var;
    

    procedure ct_c_decl_var_rec(c_decl_var0 : in out t_atribut;
                                c_decl_var1 : in     t_atribut;
                                iden        : in     t_atribut;
                                error       : in out boolean) is
    -- C_DECL_VAR:
    --     s_coma identificador C_DECL_VAR
    
        dv : descr;
        e  : boolean;
    begin
        c_decl_var0.var_idt := c_decl_var1.var_idt;
        if c_decl_var1.var_idt = ID_NUL then
            raise e_previ;
        end if;
        
        nv := nv + 1;
        dv := (dvar, c_decl_var1.var_idt, nv);
        posa(ts, iden.id, dv, e);
        
        if e then
            -- Identificador existent
            raise err;
        end if;
        
        exception
            when err     => error := true;
                            me_id_exist(iden.linia,
                                        iden.columna,
                                        iden.id);
            when e_previ => null;
    end ct_c_decl_var_rec;
    

    procedure ct_decl_const (iden0 : in     t_atribut;
                             iden1 : in     t_atribut;
                             liter : in     t_atribut;
                             error : in out boolean) is
    -- DECL_CONST:
    --     identificador s_dospunts pc_constant identificador 
    --     op_assignacio literal s_punticoma
    
        d, dc : descr;
        e     : boolean;
    begin
        d := cons(ts, iden1.id);
        
        if d.td /= dtipus then
            -- No existeix el tipus
            me_no_tipus (iden1.linia,
                         iden1.columna,
                         iden1.id);
            raise err;
        end if;
        
        if d.dt.ts > tsent then
            -- No es un tipus escalar
            me_no_escalar (iden1.linia,
                           iden1.columna,
                           iden1.id);
            raise err;
        end if;
        
        if d.dt.ts /= tsub_lit(liter) then
            -- Tipus subjacent no compatible
            me_t_sub (iden1.linia,
                      iden1.columna,
                      iden1.id,
                      liter);
            raise err;
        end if;
        
        if valor_lit(liter) < d.dt.linf or
           valor_lit(liter) > d.dt.lsup then
            -- Valor fora dels límits
            me_fora_limits (iden1.linia,
                            iden1.columna,
                            iden1.id,
                            liter,
                            d.dt.linf,
                            d.dt.lsup);
            raise err;
        end if;
        
        nv := nv + 1;      
        dc := (td => dconst,
               tc => iden1.id,
               vc => valor_lit(liter),
               nc => nv);
        posa(ts, iden0.id, dc, e);
        
        if e then
            raise err;
        end if;
        
        exception
            when err => error := true;
    end ct_decl_const;
    

    procedure ct_decl_record (p_record : in     t_atribut) is
    -- DECL_RECORD:
    --     P_RECORD pc_end pc_record s_punticoma
        
        d : descr;
    begin
        d := cons (ts, p_record.rec_idr);
        d.dt.ocup := p_record.rec_ocup;
        actualitza(ts, p_record.rec_idr, d);
    end ct_decl_record;
    

    procedure ct_p_record_rec (p_record0 : in out t_atribut;
                               p_record1 : in     t_atribut;
                               iden0     : in     t_atribut;
                               iden1     : in     t_atribut;
                               error     : in out boolean) is
    -- P_RECORD:
    --     P_RECORD identificador s_dospunts identificador s_punticoma
    
        dtc, dc : descr;
        e       : boolean;
    begin
        p_record0 := (linia    => p_record1.linia,
                      columna  => p_record1.columna,
                      a        => rec,
                      rec_idr  => p_record1.rec_idr,
                      rec_ocup => 0);
        
        if p_record1.rec_idr = ID_NUL then
            raise e_previ;
        end if;
        
        dtc := cons(ts, iden1.id);
        if dtc.td /= dtipus then
            -- No existeix el tipus
            me_no_tipus (iden1.linia,
                         iden1.columna,
                         iden1.id);
            raise err;
        end if;
        
        dc := (td  => dcamp,
               tcp => iden1.id,
               dsp => p_record1.rec_ocup);
        posacamp(ts, p_record1.rec_idr, iden0.id, dc, e);
        
        if e then
            -- El camp ja existeix
            me_camp_existent (p_record1.linia,
                              p_record1.columna,
                              p_record1.rec_idr,
                              iden0.id);
            raise err;
        end if;
        
        p_record0.rec_ocup := p_record1.rec_ocup + dtc.dt.ocup;
        
        exception
            when err     => error := true;
            when e_previ => null;
    end ct_p_record_rec;
    

    procedure ct_p_record (p_record : in out t_atribut;
                           iden0    : in     t_atribut;
                           iden1    : in     t_atribut;
                           iden2    : in     t_atribut;
                           error    : in out boolean) is
    -- P_RECORD:
    --     pc_type identificador pc_is pc_record identificador s_dospunts identificador s_punticoma
    
        dr, dtc, dc: descr;
        tdr : descr_tipus;
        e       : boolean;
    begin
        p_record := (linia    => iden0.linia,
                     columna  => iden0.columna,
                     a        => rec,
                     rec_idr  => ID_NUL,
                     rec_ocup => 0);
    
        tdr := (tsrec, 0);
        dr  := (dtipus, tdr);
        posa (ts, iden0.id, dr, e);
        
        if e then
            -- Tipus existent
            me_tipus_existent (iden0.linia,
                               iden0.columna,
                               iden0.id);
            raise err;
        end if;
        
        dtc := cons(ts, iden2.id);
        if dtc.td /= dtipus then
            -- No existeix el tipus
            me_no_tipus (iden2.linia,
                         iden2.columna,
                         iden2.id);
            raise err;
        end if;
        
        dc := (td   => dcamp,
               tcp  => iden2.id,
               dsp  => 0);
        posacamp(ts, iden0.id, iden1.id, dc, e);
        
        if e then
            -- El camp ja existeix
            me_camp_existent (iden1.linia,
                              iden1.columna,
                              iden1.id,
                              iden0.id);
            raise err;
        end if;
        
        p_record.rec_idr  := iden0.id;
        p_record.rec_ocup := dtc.dt.ocup;
        
        exception
            when err => error := true;
    end ct_p_record;
    

    procedure ct_decl_array (p_array : in     t_atribut;
                             iden    : in     t_atribut;
                             error   : in out boolean) is
    -- DECL_ARRAY:
    --     P_ARRAY s_partancat pc_of identificador s_punticoma
        
        dtb, dta: descr;
    begin
        if p_array.arr_ida = ID_NUL then
            raise e_previ;
        end if;
        
        dtb := cons(ts, iden.id);
        if dtb.td /= dtipus then
            -- No existeix el tipus
            raise err;
        end if;
        
        dta := cons(ts, p_array.arr_ida);
        dta.dt.ocup  := despl(integer(dta.dt.ocup) *
                              integer(p_array.arr_ncomp));
        dta.dt.tcomp := iden.id;        
        actualitza(ts, p_array.arr_ida, dta);
        
        exception
            when err     => me_no_tipus (iden.linia,
                                         iden.columna,
                                         iden.id);
                            error := true;
            when e_previ => null;
    end ct_decl_array;
    

    procedure ct_p_array (p_array : in out t_atribut;
                          iden0   : in     t_atribut;
                          iden1   : in     t_atribut;
                          error   : in out boolean) is
    -- P_ARRAY:
    --     pc_type identificador pc_is pc_array s_parobert identificador
        da, di : descr;
        dta    : descr_tipus;
        e      : boolean;
    begin
        p_array := (linia     => iden0.linia,
                    columna   => iden0.columna,
                    a         => arr,
                    arr_ida   => ID_NUL,
                    arr_ncomp => 0,
                    arr_b     => 0);
    
        dta := (ts    => tsarr,
                ocup  => 0,
                tcomp => ID_NUL,
                b     => 0);
                
        da  := (td => dtipus,
                dt => dta);
                
        posa (ts, iden0.id, da, e);
        
        if e then
            -- Tipus existent
            me_tipus_existent (iden0.linia,
                               iden0.columna,
                               iden0.id);
            raise err;
        end if;
        
        p_array.arr_ida := iden0.id;
        di := cons (ts, iden1.id);
        
        if di.td /= dtipus then
            -- No existeix el tipus
            me_no_tipus (iden1.linia,
                         iden1.columna,
                         iden1.id);
            raise err;
        end if;
        
        if di.dt.ts > tsent then
            -- No és un tipus escalar
            me_no_escalar (iden1.linia,
                           iden1.columna,
                           iden1.id);
            raise err;
        end if;
        
        p_array.arr_b := despl (di.dt.linf);
        p_array.arr_ncomp := comps_array(di.dt.lsup - di.dt.linf + 1);
        posaindex (ts, iden0.id, iden1.id, di);
        
        exception
            when err => error := true;
    end ct_p_array;
    

    procedure ct_p_array_rec (p_array0 : in out t_atribut;
                              p_array1 : in     t_atribut;
                              iden     : in     t_atribut;
                              error    : in out boolean) is
    -- P_ARRAY:
    --     P_ARRAY s_coma identificador
    
        nind : comps_array;
        di   : descr;
    begin
        p_array0 := (linia     => p_array1.linia,
                     columna   => p_array1.columna,
                     a         => arr,
                     arr_ida   => p_array1.arr_ida,
                     arr_ncomp => 0,
                     arr_b     => 0);
    
        if p_array0.arr_ida = ID_NUL then
            raise e_previ;
        end if;
    
        di := cons (ts, iden.id);
        if di.td /=dtipus then
            -- No existeix el tipus
            me_no_tipus (iden.linia,
                         iden.columna,
                         iden.id);
            raise err;
        end if;
        
        if di.dt.ts > tsent then
            -- No és un tipus escalar
            me_no_escalar (iden.linia,
                           iden.columna,
                           iden.id);
            raise err;
        end if;
        
        posaindex (ts, p_array1.arr_ida, iden.id, di);
        
        nind := comps_array(di.dt.lsup - di.dt.linf + 1);
        p_array0.arr_ncomp := p_array1.arr_ncomp * nind;
        p_array0.arr_b     := p_array1.arr_b * despl(nind) +
                              despl(di.dt.linf);
        
        exception
            when err     => error := true;
            when e_previ => null;
    end ct_p_array_rec;
    

    procedure ct_decl_subrang (iden0 : in     t_atribut;
                               iden1 : in     t_atribut;
                               lim0  : in     t_atribut;
                               lim1  : in     t_atribut;
                               error : in out boolean) is
    -- DECL_SUBRANG:
    --     pc_type identificador pc_is pc_new identificador 
    --     pc_range LIM s_rang LIM s_punticoma
    
        d, dsr : descr;
        dt     : descr_tipus;
        e      : boolean;
    begin
        d := cons (ts, iden1.id);
        
        if d.td /= dtipus then
            -- No existeix el tipus
            me_no_tipus(iden1.linia,
                        iden1.columna,
                        iden1.id);
            raise err;
        end if;
        
        dt := d.dt;
        if lim0.limit_idt = ID_NUL then        
            if lim0.limit_tsub /= dt.ts then
                -- Tipus del límit no compatible
                me_tsub_no_comp (lim0.linia,
                                 lim0.columna,
                                 iden1.id,
                                 lim0.limit_val);
                raise err;
            end if;
        
            if lim0.limit_val < dt.linf or
               lim0.limit_val > dt.lsup then
                -- Valor fora dels límits
                me_fora_limits (lim0.linia,
                                lim0.columna,
                                lim0.limit_idt,
                                lim0.limit_val,
                                dt.linf,
                                dt.lsup);
                raise err;
            end if;
        else
            if lim0.limit_idt /= iden1.id then
                -- Tipus del límit no compatible
                me_limit_no_comp (lim0.linia,
                                  lim0.columna,
                                  iden1.id,
                                  lim0.limit_val);
                raise err;
            end if;
        end if;
        
        if lim1.limit_idt = ID_NUL then
            if lim1.limit_tsub /= dt.ts then
                -- Tipus del límit no compatible
                me_tsub_no_comp (lim1.linia,
                                 lim1.columna,
                                 iden1.id,
                                 lim1.limit_val);
                raise err;
            end if;
            
            if lim1.limit_val < dt.linf or
               lim1.limit_val > dt.lsup then
                -- Valor fora dels límits
                me_fora_limits (lim1.linia,
                                lim1.columna,
                                lim1.id,
                                lim1.limit_val,
                                dt.linf,
                                dt.lsup);
                raise err;
            end if;
        else
            if lim1.limit_idt /= iden1.id then
                -- Tipus del límit no compatible
                me_limit_no_comp (lim1.linia,
                                  lim1.columna,
                                  iden1.id,
                                  lim1.limit_val);
                raise err;
            end if;
        end if;
        
        if lim0.limit_val > lim1.limit_val then
            -- Els limits estàn invertits
            me_limits_invertits (lim0.linia,
                                 lim0.columna,
                                 lim0.limit_val,
                                 lim1.limit_val);
            raise err;
        end if;

        dt.linf := lim0.limit_val;
        dt.lsup := lim1.limit_val;
        dsr := (dtipus, dt);
        posa (ts, iden0.id, dsr, e);
        
        if e then
            -- Tipus existent
            me_tipus_existent (iden0.linia,
                               iden0.columna,
                               iden0.id);
            raise err;
        end if;
        
        exception
            when err => error := true;
    end ct_decl_subrang;


    procedure ct_lim_id (lim   : in out t_atribut;
                         ident : in     t_atribut;
                         error : in out boolean) is
    -- LIM:
    --     identificador
        d, dc : descr;
    begin
        lim := (linia      => ident.linia,
                columna    => ident.columna,
                a          => limit,
                limit_idt  => ID_NUL,
                limit_tsub => tsnul,
                limit_val  => 0);
        d := cons (ts, ident.id);
        
        if d.td /= dconst then
            -- No és una constant
            raise err;
        end if;
        
        dc             := cons(ts, d.tc);
        lim.limit_idt  := d.tc;
        lim.limit_tsub := dc.dt.ts;
        lim.limit_val  := d.vc;
        
        exception
            when err => error := true;
                        me_no_constant (lim.linia,
                                        lim.columna,
                                        ident.id);
    end ct_lim_id;
    

    procedure ct_lim_lit (lim   : in out t_atribut;
                          lit   : in     t_atribut;
                          error : in out boolean) is
    -- LIM:
    --     literal
    begin
        lim := (linia      => lit.linia,
                columna    => lit.columna,
                a          => limit,
                limit_idt  => ID_NUL,
                limit_tsub => tsnul,
                limit_val  => 0);
        
        if tsub_lit(lit) > tsent then
            -- El límit no és un tipus escalar
            raise err;
        end if;
        
        lim.limit_tsub := tsub_lit(lit);
        lim.limit_val  := valor_lit(lit);
        lim.limit_idt  := ID_NUL;
        
        exception
            when err => error := true;
                        me_no_escalar (lim.linia,
                                       lim.columna);
    end ct_lim_lit;
    

    procedure ct_ref_id (refer : in out t_atribut;
                         iden  : in     t_atribut;
                         error : in out boolean) is
    -- REF:
    --     identificador
    
        d, dtid : descr;
        --ind     : indexarg;
    begin
        d     := cons(ts, iden.id);
        refer := (linia    => iden.linia,
                  columna  => iden.columna,
                  a        => ref,
                  ref_idb  => iden.id,
                  ref_tip  => rvar,
                  ref_idt  => ID_NUL,
                  ref_tsub => tsnul,
                  ref_mde  => var,
                  ref_b    => VAR_NUL, 
                  ref_d    => VAR_NUL,
                  ref_p    => PROC_NUL);
        
        case d.td is
            when dvar   => refer.ref_tip  := rvar;
                           dtid           := cons (ts, d.tv);
                           refer.ref_tsub := dtid.dt.ts;
                           refer.ref_idt  := d.tv;
                           refer.ref_mde  := var;
                           
            when dconst => refer.ref_tip  := rconst;
                           dtid           := cons (ts, d.tc);
                           refer.ref_tsub := dtid.dt.ts;
                           refer.ref_idt  := d.tc;
                           refer.ref_mde  := const;
                           
            when darg   => refer.ref_tip  := rconst;
                           dtid           := cons (ts, d.ta);
                           refer.ref_tsub := dtid.dt.ts;
                           refer.ref_idt  := d.ta;
                           refer.ref_mde  := const;
                           
            when dproc  => refer.ref_tip  := rproc;
                           
            when others  => -- Identificador no existent
                            raise err;
        end case;
        
        exception
            when err => me_id_inexist (iden.linia,
                                       iden.columna,
                                       iden.id);
                        error := true;
    end ct_ref_id;
    

    procedure ct_ref_rec (refer0  : in out t_atribut;
                          refer1  : in     t_atribut;
                          iden    : in     t_atribut;
                          error   : in out boolean) is
    -- REF:
    --     REF s_punt identificador
    
        d, dcamp, dtcamp : descr;
    begin
        if refer1.ref_idt = ID_NUL then
            raise e_previ;
        end if;
        
        d := cons (ts, refer1.ref_idt);    
        if d.dt.ts /= tsrec then
            -- No és un tipus record
            me_no_record (refer1.linia,
                          refer1.columna,
                          refer1.ref_idb);
            raise err;
        end if;

        dcamp := conscamp (ts, refer1.ref_idt, iden.id);

        if dcamp.td = dnul_la then
            -- El camp no existeix
            me_no_camp (iden.linia,
                        iden.columna,
                        refer1.ref_idb, 
                        iden.id);
            raise err;
        end if;

        refer0.ref_tip := refer1.ref_tip;
        refer0.ref_idt := dcamp.tcp;
        dtcamp := cons (ts, dcamp.tcp);
        refer0.ref_tsub := dtcamp.dt.ts;
        refer0.ref_mde  := refer1.ref_mde;
  
        exception
            when err     => error := true;
            when e_previ => null;
    end ct_ref_rec;
    

    procedure ct_ref_prmb_rind (refer     : in out t_atribut;
                                prmb_rind : in     t_atribut;
                                error     : in out boolean) is
    -- REF:
    --     PRMB_RIND s_partancat
        da, dcamp : descr;
    begin
        refer := (linia    => prmb_rind.linia, 
                  columna  => prmb_rind.columna, 
                  a        => ref,
                  ref_idb  => prmb_rind.prmb_idb,
                  ref_tip  => prmb_rind.prmb_tr, 
                  ref_idt  => ID_NUL, 
                  ref_tsub => tsnul, 
                  ref_mde  => var,
                  ref_b    => VAR_NUL,
                  ref_d    => VAR_NUL, 
                  ref_p    => PROC_NUL);

        case prmb_rind.prmb_tr is
            when rvar   => 
                           if esvalidarr (prmb_rind.prmb_idxar) then
                               -- Falten índexos a l'array
                               me_falten_index (prmb_rind.linia, 
                                                prmb_rind.columna, 
                                                prmb_rind.prmb_idb);
                               raise err;
                           end if;
                           
                           refer.ref_tip  := prmb_rind.prmb_tr;
                           refer.ref_idb  := prmb_rind.prmb_idb;
                           if prmb_rind.prmb_idt = ID_NUL then
            		       raise e_previ;
            		   end if;
                           da := cons(ts, prmb_rind.prmb_idt);
                           refer.ref_idt  := da.dt.tcomp;
                           dcamp := cons(ts, da.dt.tcomp);
                           refer.ref_tsub := dcamp.dt.ts;
                           
            when rconst => if esvalidarr (prmb_rind.prmb_idxar) then
                               -- Falten índexos a l'array
                               me_falten_index (prmb_rind.linia, 
                                                prmb_rind.columna, 
                                                prmb_rind.prmb_idb);
                               raise err;
                           end if;
                           da := cons(ts, prmb_rind.prmb_idb);
                           refer.ref_idt  := da.dt.tcomp;
                           dcamp := cons(ts, da.dt.tcomp);
                           refer.ref_tsub := dcamp.dt.ts;
                           refer.ref_mde  := const;
                           
            when rproc  => refer.ref_tip  := rprocparam;
                           refer.ref_idb  := prmb_rind.prmb_idb;
                           refer.ref_idt  := ID_NUL;
                           refer.ref_tsub := tsnul;
                           refer.ref_mde  := const;
                           if esvalidarg (prmb_rind.prmb_idxag) then
                               -- Falten paràmetres al procediment
                               me_falten_param (prmb_rind.linia, 
                                                prmb_rind.columna, 
                                                prmb_rind.prmb_idb);
                               raise err;
                           end if;
                            
            when others => raise err;
        end case;

        exception
            when err     => error := true;
            when e_previ => null;
    end ct_ref_prmb_rind;
    

    procedure ct_prmb_rind (prmb_rind : in out t_atribut;
                            refer     : in     t_atribut;
                            e         : in     t_atribut;
                            error     : in out boolean) is
    -- PRMB_RIND:
    --     REF s_parobert E
        indarr         : indexarr;
        indarg         : indexarg;
        dti, dcarg, dt : descr;
        di, ta, idarg  : id_nom;
    begin
        prmb_rind := (a          => prmb, 
                      linia      => refer.linia, 
                      columna    => refer.columna, 
                      prmb_tr    => refer.ref_tip,
                      prmb_idt   => refer.ref_idt, 
                      prmb_idb   => refer.ref_idb, 
                      prmb_idxar => 0, 
                      prmb_idxag => 0,
                      prmb_r     => VAR_NUL, 
                      prmb_d     => VAR_NUL,
                      prmb_db    => VAR_NUL, 
                      prmb_p     => PROC_NUL);
        case refer.ref_tip is
            when rvar | 
                 rconst => prmb_rind.prmb_idb := refer.ref_idb;
                           if refer.ref_tsub /= tsarr then
                               -- El tipus no és un array
                               me_no_array (refer.linia, 
                                            refer.columna, 
                                            refer.ref_idb);
                               raise err;
                           end if;
                           
                           indarr := primerindex (ts, refer.ref_idt);

                           if not esvalidarr (indarr) then
                               raise err;
                           end if;

                           consindex (ts, indarr, di, dti);

                           if e.exp_tsub /= dti.dt.ts then
                               -- Tipus subjacent no compatibles
                               me_no_compatibles (e.linia, 
                                                  e.columna, 
                                                  e.exp_idb, 
                                                  di);
                               raise err;
                           end if;

                           if e.exp_idt /= ID_NUL then
                               if e.exp_idt /= di then
                                   -- Tipus no compatibles
                                   me_tipus_no_comp (e.linia, 
                                                     e.columna, 
                                                     e.exp_idt, 
                                                     di);
                                   raise err;
                               end if;
                           end if;
                           prmb_rind.prmb_idxar := succindex (ts, indarr);
                           prmb_rind.prmb_idt   := refer.ref_idt;
                           
            when rproc  => prmb_rind.prmb_idxag := ndesplacament (refer.ref_idb);
                           indarg := primerarg (ts, refer.ref_idb);
                           
                           if not esvalidarg (indarg) then
                               -- Procediment cridat amb arguments 
                               -- i declarat sense
                               me_no_param (refer.linia, 
                                            refer.columna, 
                                            refer.ref_idb);
                               raise err;
                           end if;
                           
                           consarg (ts, indarg, idarg, dcarg);
                           if dcarg.td = dvar then
                               ta := dcarg.tv;
                           elsif dcarg.td = darg then
                               ta := dcarg.ta;
                           else
                               raise e_previ;
                           end if;
                           
                           dt := cons (ts, ta);
                           if e.exp_tsub /= dt.dt.ts then
                               -- Tipus subjacent no compatibles
                               me_no_compatibles (e.linia, 
                                                  e.columna, 
                                                  e.exp_idb, 
                                                  idarg);
                               raise err;
                           end if;
                           
                           if e.exp_idt /= ID_NUL then
                               if e.exp_idt /= ta then
                                   -- Tipus no compatibles
                                   me_tipus_no_comp (e.linia, 
                                                     e.columna, 
                                                     e.exp_idt, 
                                                     idarg);
                                   raise err;
                               end if;
                           end if;
                           
                           if dcarg.td = dvar then
                               if e.exp_mde /= evar then
                                   -- Mode de paràmetres incorrecte
                                   me_mode_arg (e.linia, 
                                                e.columna, 
                                                refer.ref_idb, 
                                                idarg);
                                   raise err;
                               end if;
                           end if;
                           prmb_rind.prmb_idxag := succarg (ts, indarg);
                           
            when others => raise err;
        end case;
        
        exception
            when err     => error := true;
            when e_previ => null;
    end ct_prmb_rind;
    

    procedure ct_prmb_rind_rec (prmb_rind0 : in out t_atribut;
                                prmb_rind1 : in     t_atribut;
                                e          : in     t_atribut;
                                error      : in out boolean) is
    -- PRMB_RIND:
    --     PRMB_RIND s_coma E
        dti, dcarg, dt : descr;
        di, idarg, ta : id_nom;
    begin
        prmb_rind0 := (a          => prmb, 
                       linia      => prmb_rind1.linia, 
                       columna    => prmb_rind1.columna, 
                       prmb_tr    => prmb_rind1.prmb_tr,
                       prmb_idt   => prmb_rind1.prmb_idt, 
                       prmb_idb   => prmb_rind1.prmb_idb, 
                       prmb_idxar => 0, 
                       prmb_idxag => 0,
                       prmb_r     => VAR_NUL, 
                       prmb_d     => VAR_NUL,
                       prmb_db    => VAR_NUL, 
                       prmb_p     => PROC_NUL);

        case prmb_rind1.prmb_tr is
            when rvar |
                 rconst => if not esvalidarr (prmb_rind1.prmb_idxar) then
                               -- Massa índexos a l'array
                               me_massa_index (prmb_rind1.linia, 
                                               prmb_rind1.columna, 
                                               prmb_rind1.prmb_idb);
                               raise err;
                           end if;
                           
                           consindex (ts, prmb_rind1.prmb_idxar, di, dti);
                           if e.exp_tsub /= dti.dt.ts then
                               -- Tipus subjacent no compatibles
                               me_no_compatibles (e.linia, 
                                                  e.columna, 
                                                  e.exp_idb, 
                                                  di);
                               raise err;
                           end if;
                           
                           if e.exp_idt /= ID_NUL then
                               if e.exp_idt /= di then
                                   -- Tipus no compatibles
                                   me_tipus_no_comp (e.linia, 
                                                     e.columna, 
                                                     e.exp_idt, 
                                                     di);
                                   raise err;
                               end if;
                           end if;

                           prmb_rind0.prmb_idxar := succindex (ts, prmb_rind1.prmb_idxar);
                           prmb_rind0.prmb_idt   := prmb_rind1.prmb_idt;
                       
            when rproc  => if not esvalidarg (prmb_rind1.prmb_idxag) then
                               -- Massa paràmetres al procediment
                               me_massa_param (prmb_rind1.linia, 
                                               prmb_rind1.columna, 
                                               prmb_rind1.prmb_idb);
                               raise err;
                           end if;

                           consarg (ts, prmb_rind1.prmb_idxag, idarg, dcarg);
                           if dcarg.td = dvar then
                               ta := dcarg.tv;
                           elsif dcarg.td = darg then
                               ta := dcarg.ta;
                           else
                               raise e_previ;
                           end if;
                         
                           dt := cons (ts, ta);
                           if e.exp_tsub /= dt.dt.ts then
                               -- Tipus subjacent no compatibles                               
                               me_no_compatibles (e.linia, 
                                                  e.columna);
                               raise err;
                           end if;

                           if e.exp_idt /= ID_NUL then
                               if e.exp_idt /= ta then
                                   -- Tipus no compatibles
                                   me_tipus_no_comp (e.linia, 
                                                     e.columna, 
                                                     e.exp_idt, 
                                                     idarg);
                                   raise err;
                               end if;
                           end if;
                           
                           if dcarg.td = dvar then
                               if e.exp_mde /= evar then
                                   -- Mode de paràmetres incorrecte
                                   me_mode_arg (e.linia, 
                                                e.columna, 
                                                prmb_rind1.prmb_idb, 
                                                idarg);
                                   raise err;
                               end if;
                           end if;
                         
                           prmb_rind0.prmb_idxag := succarg (ts, prmb_rind1.prmb_idxag);
                         
            when others => raise err;
        end case;

        exception
            when err     => error := true;
            when e_previ => null;
    end ct_prmb_rind_rec;


    procedure ct_e_menys_unitari (e0    : in out t_atribut;
                                  e1    : in     t_atribut;
                                  error : in out boolean) is
    -- E:
    --     op_resta E %prec menys_unitari
    begin
        e0 := (linia    => e1.linia,
               columna  => e1.columna,
               a        => exp,
               exp_idt  => ID_NUL,
               exp_idb  => e1.exp_idb,
               exp_tsub => e1.exp_tsub,
               exp_mde  => eres,
               exp_res  => VAR_NUL,
               exp_d    => 0);
    
        if e1.exp_tsub /= tsent then
            -- El tipus subjacent no és enter
            raise err;
        end if;
        
        e0.exp_idt := e1.exp_idt;
        
        exception
            when err => me_no_enter (e0.linia,
                                     e0.columna);
                        error := true;
    end ct_e_menys_unitari;
    

    procedure ct_e_par (e0    : in out t_atribut;
                        e1    : in     t_atribut) is
    -- E:
    --     s_parobert E s_partancat
    begin
        e0 := (linia    => e1.linia,
               columna  => e1.columna,
               a        => exp,
               exp_idt  => e1.exp_idt,
               exp_idb  => e1.exp_idb,
               exp_tsub => e1.exp_tsub,
               exp_mde  => e1.exp_mde,
               exp_res  => VAR_NUL,
               exp_d    => 0);
    end ct_e_par;
    

    procedure ct_e_lit (e    : in out t_atribut;
                        lit  : in     t_atribut) is
    -- E:
    --     literal
    begin
        e := (linia    => lit.linia,
              columna  => lit.columna,
              a        => exp,
              exp_idt  => ID_NUL,
              exp_idb  => ID_NUL,
              exp_tsub => tsub_lit(lit),
              exp_mde  => econst,
              exp_res  => VAR_NUL,
              exp_d    => 0);
    end ct_e_lit;
    

    procedure ct_e_ref (e     : in out t_atribut;
                        refer : in     t_atribut) is
    -- E:
    --     REF
    begin
        e := (linia    => refer.linia,
              columna  => refer.columna,
              a        => exp,
              exp_idt  => ID_NUL,
              exp_idb  => refer.ref_idb,
              exp_tsub => refer.ref_tsub,
              exp_mde  => eres,
              exp_res  => VAR_NUL,
              exp_d    => 0);
        
        case refer.ref_tip is
            when rvar   => e.exp_mde := evar;
            when rconst => e.exp_mde := econst;
            when others => raise err;         -- Referència no permesa
        end case;
        
        e.exp_idt  := refer.ref_idt;
        
        exception
            when err => me_t_ref (refer.linia,
                                  refer.columna);
    end ct_e_ref;


    procedure ct_op_rel (e0     : in out t_atribut;
                         e1, e2 : in     t_atribut;
                         error  : in out boolean) is
    -- E:
    --     E op_major E
    --  |  E op_majorigual E
    --  |  E op_menor E
    --  |  E op_menorigual E
    --  |  E op_igual E
    --  |  E op_diferent E
    begin
        e0 := (linia    => e1.linia,
               columna  => e1.columna,
               a        => exp,
               exp_idt  => ID_NUL,
               exp_idb  => ID_NUL,
               exp_tsub => tsbool,
               exp_mde  => eres,
               exp_res  => VAR_NUL,
               exp_d    => 0);
                 
        if e1.exp_tsub /= e2.exp_tsub then
            -- Tipus subjacents no compatibles
            me_no_compatibles (e1.linia,
                               e1.columna);
            raise err;
        end if;

        if e1.exp_tsub > tsent then
            -- El tipus subjacent no és escalar
            me_no_escalar (e1.linia,
                           e1.columna);
            raise err;
        end if;

        if e2.exp_tsub > tsent then
            -- El tipus subjacent no és escalar
            me_no_escalar (e2.linia,
                           e2.columna);
            raise err;
        end if;

        if e1.exp_idt /= ID_NUL and e2.exp_idt /= ID_NUL then
            if e1.exp_idt /= e2.exp_idt then
                -- Tipus dels operands no compatibles
                me_oper_no_comp (e1.linia,
                                 e1.columna);
                raise err;
            end if;
        end if;

        if e1.exp_mde = econst and e2.exp_mde = econst then
            e0.exp_mde := econst;
        end if;
        
        exception
            when err => error := true;
    end ct_op_rel;


    procedure ct_op_arit (e0     : in out t_atribut;
                          e1, e2 : in     t_atribut;
                          error  : in out boolean) is
    -- E:
    --     E op_suma E
    --  |  E op_resta E    
    --  |  E op_multiplicacio E
    --  |  E op_divisio E
    --  |  E op_mod E
    begin
        e0 := (linia    => e1.linia,
               columna  => e1.columna,
               a        => exp,
               exp_idt  => ID_NUL,
               exp_idb  => ID_NUL,
               exp_tsub => tsent,
               exp_mde  => eres,
               exp_res  => VAR_NUL,
               exp_d    => 0);
    
        if e1.exp_tsub /= tsent then
            -- El tipus subjacent no és enter
            me_no_enter1 (e1.linia,
                          e1.columna);
            raise err;
        end if;
        
        if e2.exp_tsub /= tsent then 
            -- El tipus subjacent no és enter
            me_no_enter2 (e2.linia,
                          e2.columna);
            raise err;
        end if;
        
        if e1.exp_idt /= ID_NUL and e2.exp_idt /= ID_NUL then
            if e1.exp_idt /= e2.exp_idt then
                -- Tipus dels operands no compqtibles
                me_oper_no_comp (e1.linia,
                                 e1.columna);
                raise err;
            end if;
        end if;
        
        if  e1.exp_idt = ID_NUL then
            e0.exp_idt := e2.exp_idt;
        else
            e0.exp_idt := e1.exp_idt;
        end if;
        
        if e1.exp_mde = econst and e2.exp_mde = econst then
            e0.exp_mde := econst;
        end if;
        
        exception
            when err => error := true;
    end ct_op_arit;
    

    procedure ct_op_bool (e0     : in out t_atribut;
                          e1, e2 : in     t_atribut;
                          error  : in out boolean) is
    -- E:
    --     E pc_and E
    --  |  E pc_or E
    begin
        e0 := (linia    => e1.linia,
               columna  => e1.columna,
               a        => exp,
               exp_idt  => ID_NUL,
               exp_idb  => ID_NUL,
               exp_tsub => tsbool,
               exp_mde  => eres,
               exp_res  => VAR_NUL,
               exp_d    => 0);
               
        if e1.exp_tsub /= tsbool then
            -- El tipus subjacent no és booleà
            me_no_bool1 (e1.linia,
                         e1.columna);
            raise err;
        end if;
        
        if e2.exp_tsub /= tsbool then
            -- El tipus subjacent no és booleà
            me_no_bool2 (e2.linia,
                         e2.columna);
            raise err;
        end if;
        
        if e1.exp_idt /= ID_NUL and e2.exp_idt /= ID_NUL then
            if e1.exp_idt /= e2.exp_idt then
                -- Tipus dels operands no compatibles
                me_oper_no_comp (e1.linia,
                                 e1.columna);
                raise err;
            end if;
        end if;
        
        if  e1.exp_idt = ID_NUL then
            e0.exp_idt := e2.exp_idt;
        else
            e0.exp_idt := e1.exp_idt;
        end if;
        
        if e1.exp_mde = econst and e2.exp_mde = econst then
            e0.exp_mde := econst;
        end if;
        
        exception
            when err => error := true;
    end ct_op_bool;


    procedure ct_op_not (e0    : in out t_atribut;
                         e1    : in     t_atribut;
                         error : in out boolean) is
    -- E:
    --     pc_not E
    begin
        e0 := (linia    => e1.linia,
               columna  => e1.columna,
               a        => exp,
               exp_idt  => ID_NUL,
               exp_idb  => ID_NUL,
               exp_tsub => tsbool,
               exp_mde  => eres,
               exp_res  => VAR_NUL,
               exp_d    => 0);
               
        if e1.exp_tsub /= tsbool then
            -- El tipus subjacent no és booleà
            raise err;
        end if;
        
        e0.exp_idt := e1.exp_idt;
        
        exception
            when err => me_no_bool (e1.linia,
                                    e1.columna);
                        error := true;
    end ct_op_not;


   procedure ct_sent_if is
    -- SENT_IF:
    --     P_SENT_IF SENTS pc_end pc_if s_punticoma
    begin
        null;
    end ct_sent_if;
    

    procedure ct_sent_if_else (marc  : in     t_atribut;
                               error : in out boolean) is
    -- SENT_IF:
    --     P_SENT_IF SENTS pc_else M_IF1 SENTS pc_end pc_if s_punticoma
    begin
        null;
    end ct_sent_if_else;


    procedure ct_p_sent_if (e     : in     t_atribut;
                            error : in out boolean) is
    -- P_SENT_IF:
    --     pc_if E pc_then
    begin    
        if e.exp_tsub /= tsbool then 
            -- Tipus de la condició no booleà
            raise err; 
        end if;
        
        exception
            when err => me_cond (e.linia, 
                                 e.columna);
                        error := true;
    end ct_p_sent_if;


    procedure ct_sent_while (p_sent : in     t_atribut;
                             error  : in out boolean) is
    -- SENT_WHILE:
    --     P_SENT_WHILE SENTS pc_end pc_loop s_punticoma
    begin
        null;
    end ct_sent_while;


    procedure ct_p_sent_while (p_sent : in out t_atribut;
                               marc   : in     t_atribut;
                               e      : in     t_atribut;
                               error  : in out boolean) is
    -- P_SENT_WHILE:
    --     pc_while M_WHILE E pc_loop
    begin
        if e.exp_tsub /= tsbool then 
            -- Tipus de la condició no booleà
            raise err;
        end if;
        
        exception
            when err => me_cond (e.linia, 
                                 e.columna);
                        error := true;
    end ct_p_sent_while;


    procedure ct_sent_assig (refer : in     t_atribut;
                             e     : in     t_atribut;
                             error : in out boolean) is
    -- SENT_ASSIG:
    --     REF op_assignacio E s_punticoma
        d : descr;
    begin

        if e.exp_tsub >= tsarr then
            -- El tipus subjacent no es un tipus escalar
            me_exp_no_esc (e.linia, 
                           e.columna);
            raise err;
        end if;
        
        if e.exp_tsub /= refer.ref_tsub then
            -- Tipus subjacents no compatibles
            me_no_compatibles (e.linia, 
                               e.columna);
            raise err;
        end if;
        
        if e.exp_idt /= ID_NUL and refer.ref_idt /= ID_NUL then
            if e.exp_idt /= refer.ref_idt then
                -- Tipus no compatibles
                me_assig_no_comp (e.linia, 
                                  e.columna);
                raise err;
            end if;
        end if;
        
        d := cons (ts, refer.ref_idb);
        if d.td /= dvar then
            -- El tipus no correspon a una variable
            me_no_variable (refer.linia, 
                            refer.columna, 
                            refer.ref_idb);
            raise err;
        end if;

        exception
            when err => error := true;
    end ct_sent_assig;


    procedure ct_sent_crid (refer : in     t_atribut;
                            error : in out boolean) is
    -- SENT_CRID:
    --     REF s_punticoma
        indxarg : indexarg;
    begin
        if refer.ref_tip = rproc then
            indxarg := primerarg (ts, refer.ref_idb);
            if esvalidarg (indxarg) then
                -- Procediment declarat amb arguments i cridat sense
                me_param (refer.linia, 
                          refer.columna, 
                          refer.ref_idb);
                raise err;
            end if;            
        else
            if refer.ref_tip /= rprocparam then
                -- No és un procediment
                me_no_proc (refer.linia, 
                            refer.columna, 
                            refer.ref_idb); 
                raise err;
            end if;
        end if;
        
        exception
            when err => error := true;
    end ct_sent_crid;

end p_semantica.ctipus;

