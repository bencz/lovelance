with decls.dec_generals, decls.p_i3a,
     ada.sequential_io,  ada.text_io,
     decls.g_codi;
use  decls.dec_generals, decls.p_i3a,
     decls.g_codi,       ada.text_io;

package p_semantica.gcodi.gcodi3a is

    procedure prepara_fitxers_codi (nom : in string);
    procedure lectura_fitxer_c3a (instr : out i3a);
    procedure conclou_fitxer_codi;

    procedure gen_text_i3a (f3as  : in     ada.text_io.file_type;
                            instr : in     i3a);

    procedure gen_ins_copia (opd, opf1 : in     num_var);
    procedure gen_ins_consind (opd, opf1, opf2 : in     num_var);
    procedure gen_ins_copiaind (opd, opf1, opf2 : in     num_var);
    procedure gen_ins_neg (opd, opf1 : in     num_var);
    procedure gen_ins_suma (opd, opf1, opf2 : in     num_var);
    procedure gen_ins_resta (opd, opf1, opf2 : in     num_var);
    procedure gen_ins_prod (opd, opf1, opf2 : in     num_var);
    procedure gen_ins_divisio (opd, opf1, opf2 : in     num_var);
    procedure gen_ins_modul (opd, opf1, opf2 : in     num_var);
    procedure gen_ins_and (opd, opf1, opf2 : in     num_var);
    procedure gen_ins_or (opd, opf1, opf2 : in     num_var);
    procedure gen_ins_not (opd, opf1 : in     num_var);
    procedure gen_ins_etiq (ope : in     t_etiqueta);
    procedure gen_ins_goto (ope : in     t_etiqueta);
    procedure gen_ins_if_lt (opf1, opf2 : in     num_var;
                             ope        : in     t_etiqueta);
    procedure gen_ins_if_leq (opf1, opf2 : in     num_var;
                              ope        : in     t_etiqueta);
    procedure gen_ins_if_eq (opf1, opf2 : in     num_var;
                             ope        : in     t_etiqueta);
    procedure gen_ins_if_neq (opf1, opf2 : in     num_var;
                              ope        : in     t_etiqueta);
    procedure gen_ins_if_geq (opf1, opf2 : in     num_var;
                              ope        : in     t_etiqueta);
    procedure gen_ins_if_gt (opf1, opf2 : in     num_var;
                             ope        : in     t_etiqueta);
    procedure gen_ins_call (opp : in     num_proc);
    procedure gen_ins_rtn (opp : in     num_proc);
    procedure gen_ins_prmb (opp : in     num_proc);
    procedure gen_ins_params (opf1 : in     num_var);
    procedure gen_ins_paramc (opf1, opf2 : in     num_var);

private

    package p_f_c3a_binari is new ada.sequential_io (i3a);
    use p_f_c3a_binari;

    fi3as: ada.text_io.file_type;
    fi3ab: p_f_c3a_binari.file_type;

end p_semantica.gcodi.gcodi3a;
