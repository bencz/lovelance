with decls.p_taula_noms, decls.dec_generals, decls.p_descripcio, ada.text_io,
     p_semantica.missatges, p_semantica.gcodi, p_semantica.ctipus;
     
use decls.dec_generals, decls.p_descripcio, ada.text_io, 
    p_semantica.missatges, p_semantica.gcodi, p_semantica.ctipus;

package body p_semantica is

    -- Procediments d'inicialitzaci� i finalitzaci�
    procedure prepara_analisi(nom : in     string) is
    begin
        tbuida (ts);
        nv := 0;
        np := 0;
        ne := 0;
        error := false;
        tbuida (tn);
        prepara_predefinits;
        inicia_missatges;
        prepara_gcodi (nom);
    end prepara_analisi;
        

    procedure conclou_analisi (e :    out boolean) is
    begin
        finalitza_missatges;
        conclou_gcodi;
        e := error;
    end conclou_analisi;
    

    procedure prepara_predefinits is
        idbool, idchar, idint, idstr : id_nom;
        -- Inserta BOOLEAN
        procedure inserta_boolean (id:    out id_nom) is
            d : descr;
        begin
           posa(tn, "boolean", id);
           d := (td => dtipus, 
                 dt => (ts   => tsbool, 
                        ocup => 4, 
                        linf => -1, 
                        lsup => 0));                        
           posa (ts, id, d, error);
        end inserta_boolean;
        
        -- Inserta TRUE
        procedure inserta_true (idbool : in     id_nom) is
            d  : descr;
            id : id_nom;
        begin
            posa (tn, "true", id);
            nv := nv + 1;
            d := (td => dconst, 
                  tc => idbool, 
                  vc => -1,
                  nc => nv);
            posa (ts, id, d, error);
            ins_var((tv   => t_v_const, 
                     val  => -1, 
                     id   => id, 
                     np   => PROC_NUL,
                     ocup => 4, 
                     desp => 0));
            ncert := nv;
        end inserta_true;
        
        -- Inserta FALSE
        procedure inserta_false (idbool : in     id_nom) is
            d  : descr;
            id : id_nom;
        begin
            posa (tn, "false", id);
            nv := nv + 1;
            d := (td => dconst, 
                  tc => idbool, 
                  vc => 0,
                  nc => nv);
            posa (ts, id, d, error);
            ins_var((tv   => t_v_const,
                          val  => 0, 
                          id   => id, 
                          np   => PROC_NUL,
                          ocup => 4, 
                          desp => 0));
            nfals := nv;
        end inserta_false;
        
        -- Inserta CHARACTER
        procedure inserta_character (id :    out id_nom) is
            d : descr;
        begin
            posa(tn, "character", id);
            d := (td => dtipus, 
                  dt => (ts   => tscar, 
                         ocup => 4,
                         linf => character'pos(character'first),
                         lsup => character'pos(character'last)));
            posa (ts, id, d, error);
        end inserta_character;
        
        -- Inserta PUTC
        procedure inserta_putc (id : in     id_nom) is
            d      : descr;
            idputc : id_nom;
        begin
            posa (tn, "putc", idputc);
            np := np + 1;        
            d := (td => dproc, 
                  np => np);
            posa (ts, idputc, d, error);
            nv := nv + 1;
            d := (td => darg, 
                  ta => id, 
                  na => nv);
            posaarg (ts, idputc, id, d, error);
            ne := ne + 1;
            ins_proc((id      => idputc, 
                      ocup_vl => 0, 
                      n_args  => 1,
                      prof    => consprof(ts), 
                      id_int  => ne));
        end inserta_putc;
        
        -- Inserta GETC
        procedure inserta_getc (id : in     id_nom) is
            d               : descr;
            idgetc, idparam : id_nom;
        begin
            posa (tn, "getc", idgetc);
            np := np + 1;
            d := (td => dproc, 
                  np => np);
            posa (ts, idgetc, d, error);
            posa (tn,"character", idparam);
            nv := nv + 1;
            d := (td => dvar, 
                  tv => id, 
                  nv => nv);
            posaarg (ts, idgetc, idparam, d, error);
            ne := ne + 1;
            ins_proc((id      => idgetc, 
                      ocup_vl => 0, 
                      n_args  => 1,
                      prof    => consprof(ts), 
                      id_int  => ne));
        end inserta_getc;
        
        -- Inserta INTEGER
        procedure inserta_integer (id :    out id_nom) is
            d : descr;
        begin
            posa (tn, "integer", id);
            d := (td => dtipus, 
                  dt => (ts   => tsent, 
                         ocup => 4,
                         linf => valor(integer'first),
                         lsup => valor(integer'last)));
            posa (ts, id, d, error);
        end inserta_integer;
        
        -- Inserta GETI
        procedure inserta_geti (id : in     id_nom) is
            d               : descr;
            idgeti, idparam : id_nom;
        begin
            posa (tn, "geti", idgeti);
            np := np + 1;
            d := (td => dproc, 
                  np => np);
            posa (ts, idgeti, d, error);
            posa (tn, "integer", idparam);
            nv := nv + 1;
            d := (td => dvar, 
                  tv => id, 
                  nv => nv);
            posaarg (ts, idgeti, idparam, d, error);
            ne := ne + 1;
            ins_proc((id      => idgeti, 
                      ocup_vl => 0, 
                      n_args  => 1,
                      prof    => consprof(ts), 
                      id_int  => ne));
        end inserta_geti;
        
        -- Inserta PUTI
        procedure inserta_puti (id : in     id_nom) is
            d      : descr;
            idputi : id_nom;
        begin
            posa (tn, "puti", idputi);
            np := np + 1;
            d := (td => dproc, 
                  np => np);
            posa (ts, idputi, d, error);
            nv := nv + 1;
            d := (td => darg, 
                  ta => id, 
                  na => nv);
            posaarg (ts, idputi, id, d, error);
            ne := ne + 1;
            ins_proc((id      => idputi, 
                      ocup_vl => 0, 
                      n_args  => 1,
                      prof    => consprof(ts), 
                      id_int  => ne));
        end inserta_puti;
        
        -- Inserta NEW_LINE
        procedure inserta_newline is
            d  : descr;
            id : id_nom;
        begin
            posa (tn, "new_line", id);
            np := np + 1;
            d := (td => dproc, 
                  np => np);
            posa (ts, id, d, error);
            ne := ne + 1;
            ins_proc((id     => id, 
                     ocup_vl => 0, 
                     n_args  => 0,
                     prof    => consprof(ts), 
                     id_int  => ne));
        end inserta_newline;
        
        -- Inserta STRING
        procedure inserta_string (idstring :    out id_nom) is
            d: descr;
            error: boolean;
        begin
            posa(tn, "string", idstring);
            d := (td => dtipus, 
                  dt => (ts   => tsstr, 
                         ocup => 4));
            posa (ts, idstring, d, error);
        end inserta_string;
        
        -- Inserta PUTS
        procedure inserta_puts (id : in     id_nom) is
            d      : descr;
            idputs : id_nom;
        begin
            posa (tn, "puts", idputs);
            np := np + 1;
            d := (td => dproc, 
                  np => np);
            posa (ts, idputs, d, error);
            nv := nv + 1;
            d := (td => darg, 
                  ta => id, 
                  na => nv);
            posaarg (ts, idputs, id, d, error);
            ne := ne + 1;
            ins_proc((id      => idputs, 
                      ocup_vl => 0, 
                      n_args  => 1,
                      prof    => consprof(ts), 
                      id_int  => ne));
        end inserta_puts;

    begin
        inserta_boolean (idbool);
        inserta_true (idbool);
        inserta_false (idbool);
        inserta_character (idchar);
        inserta_putc (idchar);
        inserta_getc (idchar);
        inserta_integer (idint);
        inserta_geti (idint);
        inserta_puti (idint);
        inserta_newline;
        inserta_string (idstr);
        inserta_puts (idstr);
        nprimera_varusu := nv + 1;
        nprimer_procusu := np + 1;
    end prepara_predefinits;
    

    -- Rutines per crear els atributs
    procedure calc_num (atr     :    out t_atribut;
                        str     : in     string;
                        lin,col : in     natural) is
    begin
        atr := (a       => num,
                n       => integer'value(str),
                linia   => lin,
                columna => col);
    end calc_num;
    

    procedure calc_car (atr     :    out t_atribut;
                        str     : in     string;
                        lin,col : in     natural) is
    begin
        atr := (a       => car,
                c       => character(str(str'first + 1)),
                linia   => lin,
                columna => col);
    end calc_car;
    

    procedure calc_str (atr     :    out t_atribut;
                        str     : in     string;
                        lin,col : in     natural) is
        pos_lit : id_str;
    begin
        posa(tn, str(str'first + 1..str'last - 1), pos_lit);
        atr := (a       => lit_str,
                str     => pos_lit,
                linia   => lin,
                columna => col);
    end calc_str;
    

    procedure calc_id (atr     :    out t_atribut;
                       str     : in     string;
                       lin,col : in     natural) is
        pos_id : id_nom;
    begin
        posa(tn, str, pos_id);
        atr := (a       => id,
                id      => pos_id,
                linia   => lin,
                columna => col);
    end calc_id;
    

    procedure calc_atom (atr :    out t_atribut;
                         lin : in     natural;
                         col : in     natural) is
    begin
        atr := (a       => atom,
                linia   => lin,
                columna => col);
    end calc_atom;
    

    -- Rutines sem�ntiques
    procedure rs_prog (decl_prog : in     t_atribut) is
    begin
        ct_prog (decl_prog, error);
        if not error then
            gc_prog;
        end if;
    end rs_prog;
    

    procedure rs_decl_prog (decl_prog : in out t_atribut;
                            encap     : in     t_atribut;
                            ident     : in     t_atribut) is
    begin
        ct_decl_prog (decl_prog, encap, ident, error);
        if not error then
            gc_decl_prog;
        end if;
    end rs_decl_prog;
    

    procedure rs_m_decl_prog is
    begin
        if not error then
            gc_m_decl_prog;
        end if;
    end rs_m_decl_prog;
    

    procedure rs_encap_id (encap : in out t_atribut;
                           iden  : in     t_atribut) is
    begin
        ct_encap_id (encap, iden, error);
        if not error then
            gc_encap_id (iden);
        end if;
    end rs_encap_id;
    

    procedure rs_encap_p_encap (encap   : in out t_atribut;
                                p_encap : in     t_atribut) is
    begin
        ct_encap_p_encap (encap, p_encap);
        if not error then
            gc_encap_p_encap(p_encap);
        end if;
    end rs_encap_p_encap;
    

    procedure rs_p_encap_id (p_encap : in out t_atribut;
                             iden    : in     t_atribut;
                             argum   : in     t_atribut) is
    begin
        ct_p_encap_id (p_encap, iden, argum, error);
        if not error then
            gc_p_encap_id(iden);
        end if;
    end rs_p_encap_id;
    

    procedure rs_p_encap_rec (p_encap0 : in out t_atribut;
                                   p_encap1 : in     t_atribut;
                                   argum    : in     t_atribut) is
    begin
        ct_p_encap_rec (p_encap0, p_encap1, argum, error);
        if not error then
            gc_p_encap_rec;
        end if;
    end rs_p_encap_rec;
    

    procedure rs_argument (argum : in out t_atribut;
                           iden0 : in     t_atribut;
                           mode  : in     t_atribut;
                           iden1 : in     t_atribut) is
    begin
        ct_argument (argum, iden0, mode, iden1);
        if not error then
            gc_argument;
        end if;
    end rs_argument;
    

    procedure rs_mode_in (mode : in out t_atribut) is
    begin
        ct_mode_in (mode);
        if not error then
            gc_mode_in;
        end if;
    end rs_mode_in;
    

    procedure rs_mode_out (mode : in out t_atribut) is
    begin
        ct_mode_out (mode);
        if not error then
            gc_mode_out;
        end if;
    end rs_mode_out;
    

    procedure rs_mode_in_out (mode : in out t_atribut) is
    begin
        ct_mode_in_out (mode);
        if not error then
            gc_mode_in_out;
        end if;
    end rs_mode_in_out;
    

    procedure rs_decl_var (decl_var   : in out t_atribut;
                           iden       : in     t_atribut;
                           c_decl_var : in     t_atribut) is
    begin
        ct_decl_var (decl_var, iden, c_decl_var, error);
        if not error then
            gc_decl_var (iden);
        end if;
    end rs_decl_var;
    

    procedure rs_c_decl_var (c_decl_var : in out t_atribut;
                             iden       : in     t_atribut) is
    begin
        ct_c_decl_var (c_decl_var, iden, error);
        if not error then
            gc_c_decl_var;
        end if;
    end rs_c_decl_var;
    
    
    procedure rs_c_decl_var_rec (c_decl_var0 : in out t_atribut;
                                 c_decl_var1 : in     t_atribut;
                                 iden        : in     t_atribut) is
    begin
        ct_c_decl_var_rec (c_decl_var0, c_decl_var1, iden, error);
        if not error then
            gc_c_decl_var_rec (iden);
        end if;
    end rs_c_decl_var_rec;
    

    procedure rs_decl_const (iden0 : in     t_atribut;
                             iden1 : in     t_atribut;
                             liter : in     t_atribut) is
    begin
        ct_decl_const (iden0, iden1, liter, error);
        if not error then
            gc_decl_const (iden0, liter);
        end if;
    end rs_decl_const;
    

    procedure rs_decl_record (p_record : in     t_atribut) is
    begin
        ct_decl_record (p_record);
        if not error then
            gc_decl_record;
        end if;
    end rs_decl_record;
    
    procedure rs_p_record_rec (p_record0 : in out t_atribut;
                                    p_record1 : in     t_atribut;
                                    iden0     : in     t_atribut;
                                    iden1     : in     t_atribut) is
    begin
        ct_p_record_rec(p_record0, p_record1, iden0, iden1, error);
        if not error then
            gc_p_record_rec;
        end if;
    end rs_p_record_rec;
    

    procedure rs_p_record (p_record : in out t_atribut;
                           iden0    : in     t_atribut;
                           iden1    : in     t_atribut;
                           iden2    : in     t_atribut) is
    begin
        ct_p_record (p_record, iden0, iden1, iden2, error);
        if not error then
            gc_p_record;
        end if;
    end rs_p_record;
    

    procedure rs_decl_array (p_array : in     t_atribut;
                             iden    : in     t_atribut) is
    begin
        ct_decl_array (p_array, iden, error);
        if not error then
            gc_decl_array;
        end if;
    end rs_decl_array;
    

    procedure rs_p_array (p_array : in out t_atribut;
                          iden0   : in     t_atribut;
                          iden1   : in     t_atribut) is
    begin
        ct_p_array (p_array, iden0, iden1, error);
        if not error then
            gc_p_array;
        end if;
    end rs_p_array;
    

    procedure rs_p_array_rec (p_array0 : in out t_atribut;
                                   p_array1 : in     t_atribut;
                                   iden     : in     t_atribut) is
    begin
        ct_p_array_rec (p_array0, p_array1, iden, error);
        if not error then
            gc_p_array_rec;
        end if;
    end rs_p_array_rec;
    

    procedure rs_decl_subrang (iden0 : in     t_atribut;
                               iden1 : in     t_atribut;
                               lim0  : in     t_atribut;
                               lim1  : in     t_atribut) is
    begin
        ct_decl_subrang (iden0, iden1, lim0, lim1, error);
        if not error then
            gc_decl_subrang;
        end if;
    end rs_decl_subrang;
    

    procedure rs_lim_id (lim   : in out t_atribut;
                         ident : in     t_atribut) is
    begin
        ct_lim_id (lim, ident, error);
        if not error then
            gc_lim_id;
        end if;
    end rs_lim_id;
    

    procedure rs_lim_lit (lim : in out t_atribut;
                          lit : in     t_atribut) is
    begin
        ct_lim_lit (lim, lit, error);
        if not error then
            gc_lim_lit;
        end if;
    end rs_lim_lit;
    

    procedure rs_ref_id (refer : in out t_atribut;
                         iden  : in     t_atribut) is
    begin
        ct_ref_id (refer, iden, error);
        if not error then
            gc_ref_id (refer, iden);
        end if;
    end rs_ref_id;
    

    procedure rs_ref_rec (refer0 : in out t_atribut;
                          refer1 : in     t_atribut;
                          iden   : in     t_atribut) is
    begin
        ct_ref_rec (refer0, refer1, iden, error);
        if not error then
            gc_ref_rec (refer0, refer1, iden);
        end if;
    end rs_ref_rec;
    

    procedure rs_ref_prmb_rind (refer     : in out t_atribut;
                                prmb_rind : in     t_atribut) is
    begin
        ct_ref_prmb_rind (refer, prmb_rind, error);
        if not error then
            gc_ref_prmb_rind (refer, prmb_rind);
        end if;
    end rs_ref_prmb_rind;
    

    procedure rs_prmb_rind (prmb_rind : in out t_atribut;
                            refer     : in     t_atribut;
                            e         : in     t_atribut) is
    begin
        ct_prmb_rind (prmb_rind, refer, e, error);
        if not error then
            gc_prmb_rind (prmb_rind, refer, e);
        end if;
    end rs_prmb_rind;
    

    procedure rs_prmb_rind_rec (prmb_rind0 : in out t_atribut;
                                prmb_rind1 : in     t_atribut;
                                e          : in     t_atribut) is
    begin
        ct_prmb_rind_rec (prmb_rind0, prmb_rind1, e, error);
        if not error then
            gc_prmb_rind_rec (prmb_rind0, prmb_rind1, e);
        end if;
    end rs_prmb_rind_rec;
    

    procedure rs_e_menys_unitari (e0 : in out t_atribut;
                                  e1 : in     t_atribut) is
    begin
        ct_e_menys_unitari (e0, e1, error);
        if not error then
            gc_e_menys_unitari (e0, e1);
        end if;
    end rs_e_menys_unitari;
    

    procedure rs_e_par (e0 : in out t_atribut;
                        e1 : in     t_atribut) is
    begin
        ct_e_par (e0, e1);
        if not error then
            gc_e_par (e0, e1);
        end if;
    end rs_e_par;
    

    procedure rs_e_lit (e0 : in out t_atribut;
                        e1 : in     t_atribut) is
    begin
        ct_e_lit (e0, e1);
        if not error then
            gc_e_lit (e0, e1);
        end if;
    end rs_e_lit;
    

    procedure rs_e_ref (e     : in out t_atribut;
                        refer : in     t_atribut) is
    begin
        ct_e_ref (e, refer);
        if not error then
            gc_e_ref (e, refer);
        end if;
    end rs_e_ref;
    

    procedure rs_op_major (e0     : in out t_atribut;
                           e1, e2 : in     t_atribut) is
    begin
        ct_op_rel (e0, e1, e2, error);
        if not error then
            gc_op_major (e0, e1, e2);
        end if;
    end rs_op_major;
    

    procedure rs_op_major_igual (e0     : in out t_atribut;
                                 e1, e2 : in     t_atribut) is
    begin
        ct_op_rel (e0, e1, e2, error);
        if not error then
            gc_op_major_igual (e0, e1, e2);
        end if;
    end rs_op_major_igual;
    

    procedure rs_op_menor (e0     : in out t_atribut;
                           e1, e2 : in     t_atribut) is
    begin
        ct_op_rel (e0, e1, e2, error);
        if not error then
            gc_op_menor (e0, e1, e2);
        end if;
    end rs_op_menor;
    

    procedure rs_op_menor_igual (e0     : in out t_atribut;
                                 e1, e2 : in     t_atribut) is
    begin
        ct_op_rel (e0, e1, e2, error);
        if not error then
            gc_op_menor_igual (e0, e1, e2);
        end if;
    end rs_op_menor_igual;
    

    procedure rs_op_igual (e0     : in out t_atribut;
                           e1, e2 : in     t_atribut) is
    begin
        ct_op_rel (e0, e1, e2, error);
        if not error then
            gc_op_igual (e0, e1, e2);
        end if;
    end rs_op_igual;
    

    procedure rs_op_diferent (e0     : in out t_atribut;
                              e1, e2 : in     t_atribut) is
    begin
        ct_op_rel (e0, e1, e2, error);
        if not error then
            gc_op_diferent (e0, e1, e2);
        end if;
    end rs_op_diferent;
    

    procedure rs_op_suma (e0     : in out t_atribut;
                          e1, e2 : in     t_atribut) is
    begin
        ct_op_arit (e0, e1, e2, error);
        if not error then
            gc_op_suma (e0, e1, e2);
        end if;
    end rs_op_suma;
    

    procedure rs_op_resta (e0     : in out t_atribut;
                           e1, e2 : in     t_atribut) is
    begin
        ct_op_arit (e0, e1, e2, error);
        if not error then
            gc_op_resta (e0, e1, e2);
        end if;
    end rs_op_resta;
    

    procedure rs_op_mult (e0     : in out t_atribut;
                          e1, e2 : in     t_atribut) is
    begin
        ct_op_arit (e0, e1, e2, error);
        if not error then
            gc_op_mult (e0, e1, e2);
        end if;
    end rs_op_mult;
    

    procedure rs_op_div (e0     : in out t_atribut;
                         e1, e2 : in     t_atribut) is
    begin
        ct_op_arit (e0, e1, e2, error);
        if not error then
            gc_op_div (e0, e1, e2);
        end if;
    end rs_op_div;
    

    procedure rs_op_and (e0     : in out t_atribut;
                         e1, e2 : in     t_atribut) is
    begin
        ct_op_bool (e0, e1, e2, error);
        if not error then
            gc_op_and (e0, e1, e2);
        end if;
    end rs_op_and;
    

    procedure rs_op_or (e0     : in out t_atribut;
                        e1, e2 : in     t_atribut) is
    begin
        ct_op_bool (e0, e1, e2, error);
        if not error then
            gc_op_or (e0, e1, e2);
        end if;
    end rs_op_or;
    

    procedure rs_op_not (e0 : in out t_atribut;
                         e1 : in     t_atribut) is
    begin
        ct_op_not (e0, e1, error);
        if not error then
            gc_op_not (e0, e1);
        end if;
    end rs_op_not;
    

    procedure rs_op_mod (e0     : in out t_atribut;
                         e1, e2 : in     t_atribut) is
    begin
        ct_op_arit (e0, e1, e2, error);
        if not error then
            gc_op_mod (e0, e1, e2);
        end if;
    end rs_op_mod;
    

    procedure rs_sent_if is
    begin
        if not error then
            gc_sent_if;
        end if;
    end rs_sent_if;
    

    procedure rs_sent_if_else (marc : in     t_atribut) is
    begin
        if not error then
            gc_sent_if_else (marc);
        end if;
    end rs_sent_if_else;
     
     
    procedure rs_p_sent_if (e : in     t_atribut) is
    begin
        ct_p_sent_if (e, error);
        if not error then
            gc_p_sent_if (e);
        end if;
    end rs_p_sent_if;
    

    procedure rs_m_if (marc : in out t_atribut) is
    begin
        if not error then
            gc_m_if (marc);
        end if;
    end rs_m_if;
    

    procedure rs_sent_while (p_sent : in     t_atribut) is
    begin
        if not error then
            gc_sent_while (p_sent);
        end if;
    end rs_sent_while;
    

    procedure rs_p_sent_while (p_sent : in out t_atribut;
                               marc   : in     t_atribut;
                               e      : in     t_atribut) is
    begin
        ct_p_sent_while (p_sent, marc, e, error);
        if not error then
            gc_p_sent_while (p_sent, marc, e);
        end if;
    end rs_p_sent_while;
    

    procedure rs_m_while (marc : in out t_atribut) is
    begin
        if not error then
            gc_m_while (marc);
        end if;
    end rs_m_while;
    

    procedure rs_sent_assig (refer : in     t_atribut;
                             e     : in     t_atribut) is
    begin
        ct_sent_assig (refer, e, error);
        if not error then
            gc_sent_assig (refer, e);
        end if;
    end rs_sent_assig;
    

    procedure rs_sent_crid (refer : in     t_atribut) is
    begin
        ct_sent_crid (refer, error);
        if not error then
            gc_sent_crid (refer);
        end if;
    end rs_sent_crid;
    
end p_semantica;