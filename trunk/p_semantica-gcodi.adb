with p_semantica.gcodi.gcodi3a, ada.text_io;
use  p_semantica.gcodi.gcodi3a, ada.text_io;

package body p_semantica.gcodi is

    function nova_var(iden : in     id_nom) return num_var is
    begin
        nv := nv + 1;
        put_line("nv: " & nv'img);
        put_line("nova var, t_var(nv).id: " & t_var(nv).id'img);
        put_line("iden: " & iden'img);
        t_var(nv) := (tv   => t_v_iden,
                      np   => cim(p_procs),
                      ocup => 4,
                      desp => 0,
                      id   => iden);
        put_line("nova var, t_var(nv).id: " & t_var(nv).id'img);
        return nv;
    end nova_var;

    function nova_const (v : in     valor) return num_var is
    begin
        nv := nv + 1;
        put_line("nv: " & nv'img);
        put_line("nova const, t_var(nv).id: " & t_var(nv).id'img);
        put_line("v: " & v'img);
        t_var(nv) := (tv   => t_v_const,
                      val  => v,
                      id   => ID_NUL,
                      np   => cim(p_procs),
                      ocup => 4,
                      desp => 0);
        return nv;
    end nova_const;

    function nova_etiq return t_etiqueta is
    begin
        ne := ne + 1;
        return ne;
    end nova_etiq;

    procedure ins_var(var : in t_variable) is
    begin
        put_line("ins_var, nv: " & nv'img);
        t_var(nv) := var;
    end ins_var;

    procedure ins_proc(proc : in t_procediment) is
    begin
        t_proc(np) := proc;
    end ins_proc;

    procedure prepara_gcodi (nom : in     string)is
    begin
        put_line("prepara_gcodi, nv: " & nv'img);
        put_line("t_var(8).id: " & t_var(8).id'img);
        buida(p_procs);
        buida(p_params);
        buida(p_etiq);
        prepara_fitxers_codi(nom);
    end prepara_gcodi;

    procedure conclou_gcodi is
        npr:num_proc;
        ocup_v:despl;
    begin
        -- Calculam el desplaçament de les variables
        put_line("conclou_codi1, t_var(8).id: " & t_var(8).id'img);
        for p in 1..np loop
            t_proc(p).ocup_vl := 0;
        end loop;
        
        for v in nprimera_varusu..nv loop
            if t_var(v).desp = 0 then
                npr    := t_var(v).np;
                ocup_v := t_var(v).ocup;
                
                t_var(v).desp       := despl(t_proc(npr).ocup_vl);
                t_proc(npr).ocup_vl := integer(t_proc(npr).ocup_vl) +
                                       integer(ocup_v);
                t_var(v).desp := (t_var(v).desp+4) * (-1);
            else
                t_var(v).desp := (t_var(v).desp*(-4)) + 8;
            end if;
        end loop;
        put_line("conclou_codi2, t_var(8).id: " & t_var(8).id'img);
        conclou_fitxer_codi;
    end conclou_gcodi;

    procedure gc_prog is
    -- PROG:
    --     DECL_PROG
    begin
        null;
    end gc_prog;
    

    procedure gc_decl_prog is
    -- DECL_PROG:
    --     pc_procedure ENCAP pc_is
    --       DECLS
    --     pc_begin M_DECL_PROG
    --       SENTS
    --     pc_end identificador s_punticoma
        nproc : num_proc;
    begin
        nproc := cim(p_procs);
        desempilar(p_procs);
        gen_ins_rtn(nproc);
    end gc_decl_prog;
    

    procedure gc_m_decl_prog is
    -- M_DECL_PROG:
        et : t_etiqueta;
        np : num_proc;
    begin
        et := nova_etiq;
        gen_ins_etiq(et);
        np := cim(p_procs);
        t_proc(np).id_int := et;
        gen_ins_prmb(np);
    end gc_m_decl_prog;
    

    procedure gc_encap_id (iden : in     t_atribut) is
    -- ENCAP:
    --     identificador
        d : descr;
    begin
        d := cons (ts,iden.id);
        empilar(p_procs, d.np);
        t_proc(d.np).id := iden.id;
        t_proc(d.np).prof := consprof(ts);
        t_proc(d.np).n_args := 0;
    end gc_encap_id;


    procedure gc_encap_p_encap (p_encap : in     t_atribut) is
    -- ENCAP:
    --     P_ENCAP s_partancat
        ia     : indexarg;
        d, dt  : descr;
        idarg  : id_nom;
        nvar   : num_var;
        n_args : despl;
    begin
        t_proc(np).id   := p_encap.id;
        t_proc(np).prof := consprof(ts);
        n_args  := 0;
        ia := primerarg(ts, p_encap.id);
        while esvalidarg(ia) loop
            n_args := n_args + 1;
            consarg(ts, ia, idarg, d);
            if d.td = dvar then
                nvar := d.nv;
                dt := cons (ts, d.tv);
            else
                nvar := d.na;
                dt   := cons (ts, d.ta);
            end if;
            t_var(nvar) := (tv    => t_v_iden,
                            np    => cim(p_procs),
                            ocup  => dt.dt.ocup,
                            desp  => -n_args,
                            id    => idarg);
            ia := succarg(ts, ia);
        end loop;
        t_proc(np).n_args := integer(n_args);
    end gc_encap_p_encap;


    procedure gc_p_encap_id (iden : in     t_atribut) is
    -- P_ENCAP:
    --     identificador s_parobert ARGUMENT
        d: descr;
    begin
        d := cons (ts, iden.id);
        empilar(p_procs, d.np);
        t_proc(d.np).id := iden.id;
        t_proc(d.np).prof := consprof(ts);
        t_proc(d.np).n_args := 1;
    end gc_p_encap_id;


    procedure gc_p_encap_rec is
    -- P_ENCAP:
    --     P_ENCAP s_punticoma ARGUMENT
    begin
        t_proc(cim(p_procs)).n_args := t_proc(cim(p_procs)).n_args + 1;
    end gc_p_encap_rec;
    

    procedure gc_argument is
    -- ARGUMENT:
    --     identificador s_dospunts MODE identificador
    begin
        null;
    end gc_argument;
    
    
    procedure gc_mode_in is
    -- MODE:
    --     pc_in
    begin
        null;
    end gc_mode_in;
    

    procedure gc_mode_out is
    -- MODE:
    --     pc_out
    begin
        null;
    end gc_mode_out;
    

    procedure gc_mode_in_out is
    -- MODE:
    --     pc_in pc_out
    begin
        null;
    end gc_mode_in_out;
    

    procedure gc_decl_var (iden : in     t_atribut) is
    -- DECL_VAR:
    --     identificador C_DECL_VAR
        d, dt : descr;
        nvar  : num_var;
    begin
        d := cons (ts, iden.id);
        dt := cons (ts, d.tv);
        nvar := d.nv;
        put_line("gc_decl_var, nvar: " & nvar'img);
        put_line("gc_decl_var, iden.id: " & iden.id'img);
        t_var(nvar) :=(tv   => t_v_iden,
                       np   => cim(p_procs),
                       ocup => dt.dt.ocup,
                       desp => 0,
                       id   => iden.id);
    end gc_decl_var;


    procedure gc_c_decl_var is
    -- C_DECL_VAR:
    --     s_dospunts identificador s_punticoma
    begin
        null;
    end gc_c_decl_var;
    

    procedure gc_c_decl_var_rec (iden : in     t_atribut) is
    -- C_DECL_VAR:
    --     s_coma identificador C_DECL_VAR
        d, dt : descr;
        nvar  : num_var;
    begin
        d  := cons (ts, iden.id);
        dt := cons (ts, d.tv);
        nvar := d.nv;
        t_var(nvar) := (tv   => t_v_iden,
                        np   => cim(p_procs),
                        ocup => dt.dt.ocup,
                        desp => 0,
                        id   => iden.id);
    end gc_c_decl_var_rec;


    procedure gc_decl_const (iden  : in     t_atribut;
                             liter : in     t_atribut) is
    -- DECL_CONST:
    --     identificador s_dospunts pc_constant identificador 
    --     op_assignacio literal s_punticoma
        n : num_var;
        d : descr;
    begin
        d := cons (ts, iden.id);
        n := d.nc;
        t_var(n) := (tv   => t_v_const,
                     val  => d.vc,
                     id   => iden.id,
                     np   => cim(p_procs),
                     ocup => 4,
                     desp => 0);
    end gc_decl_const;


    procedure gc_decl_record is
    -- DECL_RECORD:
    --     P_RECORD pc_end pc_record s_punticoma
    begin
        null;
    end gc_decl_record;
    

    procedure gc_p_record_rec is
    -- P_RECORD:
    --     P_RECORD identificador s_dospunts identificador s_punticoma
    begin
        null;
    end gc_p_record_rec;
    

    procedure gc_p_record is
    -- P_RECORD:
    --     pc_type identificador pc_is pc_record
    begin
        null;
    end gc_p_record;
    

    procedure gc_decl_array is
    -- DECL_ARRAY:
    --     P_ARRAY s_partancat pc_of identificador s_punticoma
    begin
        null;
    end gc_decl_array;
    

    procedure gc_p_array is
    -- P_ARRAY:
    --     pc_type identificador pc_is pc_array s_parobert identificador
    begin
        null;
    end gc_p_array;
    

    procedure gc_p_array_rec is
    -- P_ARRAY:
    --     P_ARRAY s_coma identificador
    begin
        null;
    end gc_p_array_rec;
    

    procedure gc_decl_subrang is
    -- DECL_SUBRANG:
    --     pc_type identificador pc_is pc_new identificador 
    --     pc_range LIM s_rang LIM s_punticoma
    begin
        null;
    end gc_decl_subrang;
    

    procedure gc_lim_id is
    -- LIM:
    --     identificador
    begin
        null;
    end gc_lim_id;
    
    procedure gc_lim_lit is
    -- LIM:
    --     literal
    begin
        null;
    end gc_lim_lit;
    

    procedure gc_ref_id (ref  : in out t_atribut;
                         iden : in     t_atribut) is
    -- REF:
    --     identificador
        d : descr;
    begin
        d := cons (ts, iden.id);
        
        case d.td is
            when dvar   => ref.ref_b := d.nv;
            when dconst => ref.ref_b := d.nc;
            when dproc  => ref.ref_p := d.np;
            when others => null;
        end case;
        
        ref.ref_d := 0;
    end gc_ref_id;


    procedure gc_ref_rec (ref0 : in out t_atribut;
                          ref1 : in     t_atribut;
                          iden : in     t_atribut) is
    -- REF:
    --     REF s_punt identificador
        dc, dt   : descr;
        c, t, tc : num_var;
    begin
        dc := conscamp(ts, ref1.ref_idt, iden.id);
        
        case ref1.ref_d is
            when VAR_NUL => t  := nova_var(ID_NUL);
                            c  := nova_const(valor(dc.dsp));
                            gen_ins_copia(t, c);
            when others  => t  := nova_var(ID_NUL);
                            tc := nova_const(valor(dc.dsp));
                            gen_ins_suma(t, ref1.ref_d, tc);
        end case;
        
        ref0.ref_b := ref1.ref_b;
        ref0.ref_d := t;
    end gc_ref_rec;


    procedure gc_ref_prmb_rind (ref       : in out t_atribut;
                                prmb_rind : in     t_atribut) is
    -- REF:
    --     PRMB_RIND s_partancat
        dt, da : descr;
        t1, t2, t3, n1, n2 : num_var;
    begin
        if prmb_rind.prmb_tr = rproc then
            ref.ref_p := prmb_rind.prmb_p;
        else
            da := cons (ts, prmb_rind.prmb_idt);
            dt := cons (ts, da.dt.tcomp);
            t1 := nova_var(ID_NUL);
            n1 := nova_const(valor(dt.dt.ocup));
            gen_ins_prod(t1, prmb_rind.prmb_d, n1);
            t2 := nova_var(ID_NUL);
            put_line("prmb.rind.prmb_idt: " & prmb_rind.prmb_idt'img);
            put_line("--------passam per aqui");
            put_line("da.dt.b: " & da.dt.b'img);
            --imprimir(ts);
            --t3:= nova_var(1000); --DEBUG Para que de Constraint error

            put_line(valor(da.dt.b)'img);
            n2 := nova_const(valor(da.dt.b));
            gen_ins_resta(t2, t1, n2);
            ref.ref_b := prmb_rind.prmb_r;
            
            case prmb_rind.prmb_db is
                when VAR_NUL => ref.ref_d := t2;
                when others  => t3:=nova_var(ID_NUL);
                                gen_ins_suma(t3, prmb_rind.prmb_db, t2);
                                ref.ref_d := t3;
            end case;
        end if;
    end gc_ref_prmb_rind;
    

    procedure gc_prmb_rind (prmb_rind : in out t_atribut;
                            ref       : in     t_atribut;
                            e         : in     t_atribut) is
    -- PRMB_RIND:
    --     REF s_parobert E
    begin
        if ref.ref_tip = rproc then
            empilar(p_params, (param_res => e.exp_res, 
                              param_d   => e.exp_d));
            prmb_rind.prmb_p := ref.ref_p;
        else
            prmb_rind.prmb_r  := ref.ref_b;
            prmb_rind.prmb_db := ref.ref_d;
            prmb_rind.prmb_d  := e.exp_res;
      end if;
    end gc_prmb_rind;
    

    procedure gc_prmb_rind_rec (prmb_rind0 : in out t_atribut;
                                prmb_rind1 : in     t_atribut;
                                e          : in     t_atribut) is
    -- PRMB_RIND:
    --     PRMB_RIND s_coma E
        di        : descr;
        iid       : id_nom;
        t1, t2, n : num_var;
    begin
        if prmb_rind1.prmb_tr = rproc then
            empilar(p_params, (param_res => e.exp_res, 
                               param_d   => e.exp_d));
            prmb_rind0.prmb_p := prmb_rind1.prmb_p;
        else
            prmb_rind0.prmb_r := prmb_rind1.prmb_r;
            consindex(ts, prmb_rind1.prmb_idxar, iid, di);
            t1 := nova_var(ID_NUL);
            n  := nova_const(di.dt.lsup - di.dt.linf + 1);
            gen_ins_prod(t1,prmb_rind1.prmb_d,n);
            t2 := nova_var(ID_NUL);
            gen_ins_suma(t2, t1,e.exp_res);
            prmb_rind0.prmb_d  := t2;
            prmb_rind0.prmb_db := prmb_rind1.prmb_db;
      end if;
    end gc_prmb_rind_rec;
    

    procedure gc_e_menys_unitari (e0 : in out t_atribut;
                                  e1 : in     t_atribut) is
    -- E:
    --     op_resta E %prec menys_unitari
    
        t : num_var;
    begin
        if e1.exp_mde = econst then
            e0.exp_mde := econst;
            e0.exp_res := e1.exp_res;
            t_var(e0.exp_res).val := -t_var(e0.exp_res).val;
        else
            t := nova_var(ID_NUL);
            gen_ins_neg(t, e1.exp_res);
            e0.exp_res := t;
        end if;
        
        e0.exp_d := e1.exp_d;
    end gc_e_menys_unitari;
    

    procedure gc_e_par (e0 : in out t_atribut;
                        e1 : in     t_atribut) is
    -- E:
    --     s_parobert E s_partancat
    begin
        e0.exp_mde := e1.exp_mde;
        e0.exp_res := e1.exp_res;
        e0.exp_d   := e1.exp_d;
    end gc_e_par;
    

    procedure gc_e_lit (e0 : in out t_atribut;
                        e1 : in     t_atribut) is
    -- E:
    --     literal
        t : num_var;
    begin
        if e1.a = lit_str then
            t := nova_var(ID_NUL);
            t_var(t) := (tv    => t_v_lit_str, 
                         id    => ID_NUL, 
                         np    => cim(p_procs), 
                         ocup  => 4,
                         desp  => despl(e1.str), 
                         str => e1.str);
        else
            t := nova_const(valor_lit(e1));
            e0.exp_d := 0;
        end if;
        
        e0.exp_res := t;
    end gc_e_lit;
    

    procedure gc_e_ref (e0 : in out t_atribut;
                        e1 : in     t_atribut) is
    -- E:
    --     REF
    begin
        e0.exp_res := e1.ref_b;
        e0.exp_d   := e1.ref_d;
    end gc_e_ref;
    

    procedure gc_op_major (e0 : in out t_atribut;
                           e1 : in     t_atribut;
                           e2 : in     t_atribut) is
    -- E:
    --     E op_major E
        t1, t2, t3 : num_var;
        et1, et2   : t_etiqueta;
    begin
        if e1.exp_d = 0 then
            t1 := e1.exp_res;
        else
            t1 := nova_var(ID_NUL);
            gen_ins_consind(t1, e1.exp_res, e1.exp_d);
        end if;
        
        if e2.exp_d = 0 then
            t2 := e2.exp_res;
        else
            t2 := nova_var(ID_NUL);
            gen_ins_consind(t2, e2.exp_res, e2.exp_d);
        end if;
        
        et1 := nova_etiq;
        et2 := nova_etiq;
        t3  := nova_var(ID_NUL);
        gen_ins_if_gt(t1, t2, et1);
        gen_ins_copia(t3, nfals);
        gen_ins_goto(et2);
        gen_ins_etiq(et1);
        gen_ins_copia(t3,ncert);
        gen_ins_etiq(et2);
        e0.exp_res:=t3;
        e0.exp_d:=0;
    end gc_op_major;
    

    procedure gc_op_major_igual (e0 : in out t_atribut;
                                 e1 : in     t_atribut;
                                 e2 : in     t_atribut) is
    -- E:
    --     E op_majorigual E
        t1, t2, t3 : num_var;
        et1, et2   : t_etiqueta;
    begin
        if e1.exp_d = 0 then
            t1 := e1.exp_res;
        else
            t1 := nova_var(ID_NUL);
            gen_ins_consind(t1, e1.exp_res, e1.exp_d);
        end if;
        
        if e2.exp_d = 0 then
            t2 := e2.exp_res;
        else
            t2 := nova_var(ID_NUL);
            gen_ins_consind(t2, e2.exp_res, e2.exp_d);
        end if;
        
        et1 := nova_etiq;
        et2 := nova_etiq;
        t3  := nova_var(ID_NUL);
        gen_ins_if_geq(t1, t2, et1);
        gen_ins_copia(t3, nfals);
        gen_ins_goto(et2);
        gen_ins_etiq(et1);
        gen_ins_copia(t3, ncert);
        gen_ins_etiq(et2);
        e0.exp_res := t3;
        e0.exp_d   := 0;
    end gc_op_major_igual;
    

    procedure gc_op_menor (e0 : in out t_atribut;
                           e1 : in     t_atribut;
                           e2 : in     t_atribut) is
    -- E:
    --     E op_menor E
        t1, t2, t3 : num_var;
        et1, et2   : t_etiqueta;
    begin
        if e1.exp_d = 0 then
            t1 := e1.exp_res;
        else
            t1 := nova_var(ID_NUL);
            gen_ins_consind(t1, e1.exp_res, e1.exp_d);
        end if;
        
        if e2.exp_d = 0 then
            t2 := e2.exp_res;
        else
            t2 := nova_var(ID_NUL);
            gen_ins_consind(t2, e2.exp_res, e2.exp_d);
        end if;
        
        et1 := nova_etiq;
        et2 := nova_etiq;
        t3  := nova_var(ID_NUL);
        gen_ins_if_lt(t1, t2, et1);
        gen_ins_copia(t3, nfals);
        gen_ins_goto(et2);
        gen_ins_etiq(et1);
        gen_ins_copia(t3, ncert);
        gen_ins_etiq(et2);
        e0.exp_res := t3;
        e0.exp_d   := 0;
    end gc_op_menor;
    

    procedure gc_op_menor_igual (e0 : in out t_atribut;
                                 e1 : in     t_atribut;
                                 e2 : in     t_atribut) is
    -- E:
    --     E op_menorigual E
        t1, t2, t3 : num_var;
        et1, et2   : t_etiqueta;
    begin
        if e1.exp_d = 0 then
            t1 := e1.exp_res;
        else
            t1 := nova_var(ID_NUL);
            gen_ins_consind(t1, e1.exp_res, e1.exp_d);
        end if;
        
        if e2.exp_d = 0 then
            t2 := e2.exp_res;
        else
            t2 := nova_var(ID_NUL);
            gen_ins_consind(t2, e2.exp_res, e2.exp_d);
        end if;
        
        et1 := nova_etiq;
        et2 := nova_etiq;
        t3  := nova_var(ID_NUL);
        gen_ins_if_leq(t1, t2, et1);
        gen_ins_copia(t3, nfals);
        gen_ins_goto(et2);
        gen_ins_etiq(et1);
        gen_ins_copia(t3, ncert);
        gen_ins_etiq(et2);
        e0.exp_res := t3;
        e0.exp_d   := 0;
    end gc_op_menor_igual;
    

    procedure gc_op_igual (e0 : in out t_atribut;
                           e1 : in     t_atribut;
                           e2 : in     t_atribut) is
    -- E:
    --     E op_igual E
        t1, t2, t3 : num_var;
        et1, et2   : t_etiqueta;
    begin
        if e1.exp_d = 0 then
            t1 := e1.exp_res;
        else
            t1 := nova_var(ID_NUL);
            gen_ins_consind(t1, e1.exp_res, e1.exp_d);
        end if;
        
        if e2.exp_d = 0 then
            t2 := e2.exp_res;
        else
            t2 := nova_var(ID_NUL);
            gen_ins_consind(t2, e2.exp_res, e2.exp_d);
        end if;
        
        et1 := nova_etiq;
        et2 := nova_etiq;
        t3  := nova_var(ID_NUL);
        gen_ins_if_eq(t1, t2, et1);
        gen_ins_copia(t3, nfals);
        gen_ins_goto(et2);
        gen_ins_etiq(et1);
        gen_ins_copia(t3, ncert);
        gen_ins_etiq(et2);
        e0.exp_res := t3;
        e0.exp_d   := 0;
    end gc_op_igual;
    

    procedure gc_op_diferent (e0 : in out t_atribut;
                              e1 : in     t_atribut;
                              e2 : in     t_atribut) is
    -- E:
    --     E op_diferent E
        t1, t2, t3 : num_var;
        et1, et2   : t_etiqueta;
    begin
        if e1.exp_d = 0 then
            t1 := e1.exp_res;
        else
            t1 := nova_var(ID_NUL);
            gen_ins_consind(t1, e1.exp_res, e1.exp_d);
        end if;
        
        if e2.exp_d = 0 then
            t2 := e2.exp_res;
        else
            t2 := nova_var(ID_NUL);
            gen_ins_consind(t2, e2.exp_res, e2.exp_d);
        end if;
        
        et1 := nova_etiq;
        et2 := nova_etiq;
        t3  := nova_var(ID_NUL);
        gen_ins_if_neq(t1, t2, et1);
        gen_ins_copia(t3, nfals);
        gen_ins_goto(et2);
        gen_ins_etiq(et1);
        gen_ins_copia(t3, ncert);
        gen_ins_etiq(et2);
        e0.exp_res := t3;
        e0.exp_d   := 0;
    end gc_op_diferent;
    

    procedure gc_op_suma (e0 : in out t_atribut;
                          e1 : in     t_atribut;
                          e2 : in     t_atribut) is
    -- E:
    --     E op_suma E
        t1, t2, t3 : num_var;
    begin
        if e1.exp_d = 0 then
            t1 := e1.exp_res;
        else
            t1 := nova_var(ID_NUL);
            gen_ins_consind(t1, e1.exp_res, e1.exp_d);
        end if;
        
        if e2.exp_d = 0 then
            t2 := e2.exp_res;
        else
            t2 := nova_var(ID_NUL);
            gen_ins_consind(t2, e2.exp_res, e2.exp_d);
        end if;
       
        t3 := nova_var(ID_NUL);
        gen_ins_suma(t3, t1, t2);
        e0.exp_res := t3;
        e0.exp_d := 0;
    end gc_op_suma;
    

    procedure gc_op_resta (e0 : in out t_atribut;
                           e1 : in     t_atribut;
                           e2 : in     t_atribut) is
    -- E:
    --     E op_resta E
        t1, t2, t3 : num_var;
    begin
        if e1.exp_d = 0 then
            t1 := e1.exp_res;
        else
            t1 := nova_var(ID_NUL);
            gen_ins_consind(t1, e1.exp_res, e1.exp_d);
        end if;
        
        if e2.exp_d = 0 then
            t2 := e2.exp_res;
        else
            t2 := nova_var(ID_NUL);
            gen_ins_consind(t2, e2.exp_res, e2.exp_d);
        end if;
      
        t3 := nova_var(ID_NUL);
        gen_ins_resta(t3, t1, t2);
        e0.exp_res := t3;
        e0.exp_d := 0;
    end gc_op_resta;
    

    procedure gc_op_mult (e0 : in out t_atribut;
                          e1 : in     t_atribut;
                          e2 : in     t_atribut) is
    -- E:
    --     E op_multiplicacio E
        t1, t2, t3 : num_var;
    begin
        if e1.exp_d = 0 then
            t1 := e1.exp_res;
        else
            t1 := nova_var(ID_NUL);
            gen_ins_consind(t1, e1.exp_res, e1.exp_d);
        end if;
        
        if e2.exp_d = 0 then
            t2 := e2.exp_res;
        else
            t2 := nova_var(ID_NUL);
            gen_ins_consind(t2, e2.exp_res, e2.exp_d);
        end if;
        
        t3 := nova_var(ID_NUL);
        gen_ins_prod(t3, t1, t2);
        e0.exp_res := t3;
        e0.exp_d := 0;
    end gc_op_mult;
    

    procedure gc_op_div (e0 : in out t_atribut;
                         e1 : in     t_atribut;
                         e2 : in     t_atribut) is
    -- E:
    --     E op_divisio E
        t1, t2, t3 : num_var;
    begin
        if e1.exp_d = 0 then
            t1 := e1.exp_res;
        else
            t1 := nova_var(ID_NUL);
            gen_ins_consind(t1, e1.exp_res, e1.exp_d);
        end if;
       
        if e2.exp_d = 0 then
            t2 := e2.exp_res;
        else
            t2 := nova_var(ID_NUL);
            gen_ins_consind(t2, e2.exp_res, e2.exp_d);
        end if;
       
        t3 := nova_var(ID_NUL);
        gen_ins_divisio(t3, t1, t2);
        e0.exp_res := t3;
        e0.exp_d := 0;
    end gc_op_div;
    

    procedure gc_op_and (e0 : in out t_atribut;
                         e1 : in     t_atribut;
                         e2 : in     t_atribut) is
    -- E:
    --     E pc_and E
       t1, t2, t3 : num_var;
   begin
       if e1.exp_d = 0 then
           t1 := e1.exp_res;
       else
           t1 := nova_var(ID_NUL);
           gen_ins_consind(t1, e1.exp_res, e1.exp_d);
       end if;
       
       if e2.exp_d = 0 then
           t2 := e2.exp_res;
       else
           t2 := nova_var(ID_NUL);
           gen_ins_consind(t2,e2.exp_res,e2.exp_d);
       end if;
       
       t3 := nova_var(ID_NUL);
       gen_ins_and(t3, t1, t2);
       e0.exp_res := t3;
       e0.exp_d := 0;
    end gc_op_and;
    

    procedure gc_op_or (e0 : in out t_atribut;
                        e1 : in     t_atribut;
                        e2 : in     t_atribut) is
    -- E:
    --     E pc_or E
        t1, t2,t3: num_var;
    begin
        if e1.exp_d = 0 then
            t1 := e1.exp_res;
        else
            t1 := nova_var(ID_NUL);
            gen_ins_consind(t1, e1.exp_res, e1.exp_d);
        end if;
        
        if e2.exp_d = 0 then
            t2 := e2.exp_res;
        else
            t2 := nova_var(ID_NUL);
            gen_ins_consind(t2, e2.exp_res, e2.exp_d);
        end if;
        
        t3 := nova_var(ID_NUL);
        gen_ins_or(t3, t1, t2);
        e0.exp_res := t3;
        e0.exp_d := 0;
    end gc_op_or;
    

    procedure gc_op_not (e0 : in out t_atribut;
                         e1 : in     t_atribut) is
    -- E:
    --     pc_not E
        t1, t2 : num_var;
    begin
        if e1.exp_d = 0 then
            t1 := e1.exp_res;
        else
            t1 := nova_var(ID_NUL);
            gen_ins_consind(t1,e1.exp_res,e1.exp_d);
        end if;
        
        t2 := nova_var(ID_NUL);
        gen_ins_not(t2,t1);
        e0.exp_res := t2;
        e0.exp_d := 0;
    end gc_op_not;
    

    procedure gc_op_mod (e0 : in out t_atribut;
                         e1 : in     t_atribut;
                         e2 : in     t_atribut) is
    -- E:
    --     E pc_mod E
        t1, t2, t3 : num_var;
    begin
        if e1.exp_d = 0 then
            t1 := e1.exp_res;
        else
            t1 := nova_var(ID_NUL);
            gen_ins_consind(t1, e1.exp_res, e1.exp_d);
        end if;
        
        if e2.exp_d = 0 then
            t2 := e2.exp_res;
        else
            t2 := nova_var(ID_NUL);
            gen_ins_consind(t2, e2.exp_res, e2.exp_d);
        end if;
        
        t3 := nova_var(ID_NUL);
        gen_ins_modul(t3, t1, t2);
        e0.exp_res := t3;
        e0.exp_d := 0;
    end gc_op_mod;
    

    procedure gc_sent_if is
    -- SENT_IF:
    --     P_SENT_IF SENTS pc_end pc_if s_punticoma
    begin
        gen_ins_etiq(cim(p_etiq));
        desempilar(p_etiq);
    end gc_sent_if;
    

    procedure gc_sent_if_else (marc : in     t_atribut) is
    -- SENT_IF:
    --     P_SENT_IF SENTS pc_else M_IF1 SENTS pc_end pc_if s_punticoma
    begin
        gen_ins_etiq(marc.mcond_ef);
        desempilar(p_etiq);
    end gc_sent_if_else;


    procedure gc_p_sent_if (e : in     t_atribut) is
    -- P_SENT_IF:
    --     pc_if E pc_then
       ec, ef : t_etiqueta;
   begin
      ec := nova_etiq;
      ef := nova_etiq;
      empilar(p_etiq, ef);
      -- Avaluacio de l'expressio
      gen_ins_if_eq(e.exp_res, nfals, ef);
      -- Creacio d'tiquetes inici de condicio certa
      gen_ins_etiq(ec);
    end gc_p_sent_if;
    

    procedure gc_m_if (marc : in out t_atribut) is
    -- M_IF:
        ef : t_etiqueta;
    begin
        marc := (a        => mcond, 
                 linia    => 0, 
                 columna  => 0, 
                 mcond_ei => ET_NUL,
                 mcond_ef => ET_NUL);
                 
        ef := nova_etiq;
        gen_ins_goto(ef);
        marc.mcond_ef:= ef;
        gen_ins_etiq(cim(p_etiq));
    end gc_m_if;
    

    procedure gc_sent_while (p_sent : in     t_atribut) is
    -- SENT_WHILE:
    --     P_SENT_WHILE SENTS pc_end pc_loop s_punticoma
    begin
        gen_ins_goto(p_sent.mcond_ei);
        gen_ins_etiq(cim(p_etiq));
        desempilar(p_etiq);
    end gc_sent_while;
    

    procedure gc_p_sent_while (p_sent : in out t_atribut;
                               marc   : in     t_atribut;
                               e      : in     t_atribut) is
    -- P_SENT_WHILE:
    --     pc_while M_WHILE E pc_loop
        ec, ef : t_etiqueta;
    begin
      p_sent := (a        => mcond, 
                linia    => 0, 
                columna  => 0, 
                mcond_ei => ET_NUL,
                mcond_ef => ET_NUL);
                
      -- Creació de les etiquetes
      ec := nova_etiq;
      ef := nova_etiq;
      p_sent.mcond_ei := marc.mcond_ei;
      empilar(p_etiq, ef);
      
      -- Avaluació de l'expressió
      gen_ins_if_eq(e.exp_res, nfals, ef);
      gen_ins_etiq(ec);
    end gc_p_sent_while;
    

    procedure gc_m_while (marc : in out t_atribut) is
    -- M_WHILE:
        ei : t_etiqueta;
    begin
        marc := (a        => mcond, 
                 linia    => 0, 
                 columna  => 0, 
                 mcond_ei => ET_NUL,
                 mcond_ef => ET_NUL);
                 
        ei := nova_etiq;
        gen_ins_etiq(ei);
        marc.mcond_ei := ei;
    end gc_m_while;
    

    procedure gc_sent_assig (ref : in     t_atribut;
                             e   : in     t_atribut) is
    -- SENT_ASSIG:
    --     REF op_assignacio E s_punticoma
        t : num_var;
    begin
        if e.exp_d = 0 then
            t := e.exp_res;
        else
            t := nova_var(ID_NUL);
            gen_ins_consind(t, e.exp_res, e.exp_d);
        end if;
        
        case ref.ref_d is
            when 0      => gen_ins_copia (ref.ref_b, t);
            when others => gen_ins_copiaind (ref.ref_b, ref.ref_d, t);
        end case;
    end gc_sent_assig;
    

    procedure gc_sent_crid (ref : in     t_atribut) is
    -- SENT_CRID:
    --     REF s_punticoma
        param : t_param;
        d : descr;
    begin
        while not es_buida(p_params) loop
            param := cim (p_params);
            desempilar(p_params);
            
            case param.param_d is
               when 0      => gen_ins_params (param.param_res);
               when others => gen_ins_paramc (param.param_res,
                                              param.param_d);
            end case;
        end loop;
        
        gen_ins_call(ref.ref_p);
    end gc_sent_crid;

end p_semantica.gcodi;
