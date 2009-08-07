with decls.dec_generals,    decls.p_descripcio,
     decls.p_taula_simbols, decls.p_taula_noms,
     decls.g_codi;
     
use decls.dec_generals,    decls.p_descripcio,
    decls.p_taula_simbols, decls.g_codi;

package decls.p_atributs is
    
    type enum_atribut is (num, lit_str, car, id,  var, limit, exp,
                          ref, prmb,    rec, arr, arg, mode,  mcond,
                          atom);

    type t_atribut (a: enum_atribut := atom) is
        record
            linia   : natural;
            columna : natural;

            case a is
                when num     => n          : integer;
                when lit_str => str        : id_str;
                when car     => c          : character;
                when id      => id         : id_nom;
                when var     => var_idt    : id_nom;
                when limit   => limit_idt  : id_nom;
                                limit_tsub : tipus_subjacent;
                                limit_val  : valor;
                when exp     => exp_idt    : id_nom;
                                exp_tsub   : tipus_subjacent;
                                exp_mde    : mde_exp;
                                exp_idb    : id_nom;
                                exp_res	   : num_var;
                                exp_d	   : num_var;
                when ref     => ref_idb    : id_nom;
                                ref_tip    : t_ref;
                                ref_idt    : id_nom;
                                ref_tsub   : tipus_subjacent;
                                ref_mde    : mde_ref;
                                ref_b      : num_var;
                                ref_d      : num_var;
                                ref_p      : num_proc;
                when prmb    => prmb_tr    : t_ref;
                                prmb_idt   : id_nom;
                                prmb_idb   : id_nom;
                                prmb_idxar : indexarr;
                                prmb_idxag : indexarg;
                                prmb_p     : num_proc;
                                prmb_r     : num_var;
                                prmb_d     : num_var;
                                prmb_db    : num_var;
                when rec     => rec_idr    : id_nom;
                                rec_ocup   : despl;
                when arr     => arr_ida    : id_nom;
                                arr_ncomp  : comps_array;
                                arr_b      : despl;
                when arg     => arg_idarg  : id_nom;
                                arg_idt    : id_nom;
                                arg_mde    : t_mode;
                when mode    => mode_mde   : t_mode;
                when mcond   => mcond_ei   : t_etiqueta;
                                mcond_ef   : t_etiqueta;
                when atom    => null;
            end case;
        end record;

    function valor_lit (lit : t_atribut) return valor;
    function tsub_lit  (lit : t_atribut) return tipus_subjacent;

end decls.p_atributs;
