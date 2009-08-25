with decls.dec_generals, decls.p_descripcio, decls.p_taula_simbols;
use  decls.dec_generals, decls.p_descripcio, decls.p_taula_simbols;

package decls.d_codi is

    type t_etiqueta     is new integer range 0..integer'last;
    type tipus_variable is (t_v_iden, t_v_arg, t_v_const, t_v_lit_str);
    
    type t_variable (tv : tipus_variable := t_v_iden) is
        record
            id   : id_nom;
            np   : num_proc;
            ocup : despl;
            desp : despl;
            
            case tv is
                when t_v_iden    => null;
                when t_v_arg     => d_arg : despl;
                when t_v_const   => val   : valor;
                when t_v_lit_str => str   : id_str;
            end case;
        end record;
    
    type t_procediment is
        record
            id      : id_nom;
            prof    : nambits;
            id_int  : t_etiqueta;
            n_args  : integer;
            ocup_vl : integer;
        end record;
    
    type t_param is
        record
            param_res : num_var;
            param_d   : num_var;
        end record;

    ET_NUL   : constant t_etiqueta := 0;
    
end decls.d_codi;
