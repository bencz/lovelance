with decls.dec_generals, decls.g_codi;
use  decls.dec_generals, decls.g_codi;

package decls.p_i3a is

    type rep_instr is (ins_copia,    ins_neg,     ins_suma,
                       ins_prod,     ins_resta,   ins_modul,
                       ins_consind,  ins_etiq,    ins_goto,
                       ins_params,   ins_divisio, ins_copiaind,
                       ins_paramc,   ins_call,    ins_return,
                       ins_preambul, ins_iflt,    ins_ifleq,
                       ins_ifeq,     ins_ifneq,   ins_ifgeq,
                       ins_ifgt,     ins_or,      ins_and,
                       ins_not);

    type i3a is
        record
            tins   : rep_instr;
            opproc : num_proc;
            opf1, opf2, opdesti : num_var;
            opetiq : t_etiqueta;
        end record;

end decls.p_i3a;
