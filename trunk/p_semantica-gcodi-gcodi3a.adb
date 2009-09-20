with decls.dec_generals, decls.d_codi, 
     decls.p_f_c3a_binari, ada.text_io;
use  decls.dec_generals, decls.d_codi, 
     decls.p_f_c3a_binari, ada.text_io;

package body p_semantica.gcodi.gcodi3a is

    procedure prepara_fitxers_codi(nom : in string) is
    begin
        create(fi3as, out_file, nom & ".c3as");
        create(fi3ab, out_file, nom & ".c3a");
    end prepara_fitxers_codi;

    procedure lectura_fitxer_c3a(instr : out i3a) is
    begin
        read(fi3ab, instr);
    end lectura_fitxer_c3a;

    procedure conclou_fitxer_codi is
    begin
        close(fi3as);
        close(fi3ab);
    end conclou_fitxer_codi;

    function esborra_primer_car(str: string) return string is
    begin
        return str(str'first+1..str'last);
    end esborra_primer_car;

    procedure escriu_variable (f3as : in     ada.text_io.file_type;
                               var  : in     num_var) is
    begin
        case t_var(var).tv is
            when t_v_iden     =>  --put_line("t_var(var).id: " & t_var(var).id'img);
                                  --put_line("id: " & id'img);
                                  --put_line("var: " & var'img);
--                                  put_line("i - var - id - t_var(i).id");
--                                  for i in num_var loop
--                                      if t_var(i).id /= 0 then
--                                          put(i'img);
--                                          put(" - ");
--                                          put(var'img);
--                                          put(" - ");
--                                          put(id'img);
--                                          put(" - ");
--                                          put(t_var(i).id'img); 
--                                          new_line;
--                                      end if;
--                                  end loop;

                                  if t_var(var).id = ID_NUL then
                                      put_line("es temporal");
                                      -- Es una variable temporal
                                      if var = VAR_NUL then
                                          put(f3as, "VAR_NUL");
                                      else
                                          put(f3as, "tmp_" &
                                          esborra_primer_car(var'img));
                                      end if;
                                  else
                                      put(f3as, consulta(tn, t_var(var).id));
                                      put(f3as, esborra_primer_car(var'img));
                                  end if;
            when t_v_const => if t_var(var).val < 0 then
                                  put(f3as, t_var(var).val'img);
                              else   
                                  put(f3as,
                                      esborra_primer_car(t_var(var).val'img));
                              end if;
            when others    => put(f3as, esborra_primer_car(var'img));
        end case;
    end escriu_variable;

    procedure escriu_etiqueta (f3as : in     ada.text_io.file_type;
                               et   : in     t_etiqueta) is
    begin
        put(f3as, "et_" & esborra_primer_car(et'img));
    end escriu_etiqueta;

    procedure escriu_procediment (f3as : in     ada.text_io.file_type;
                                  np   : in     num_proc) is
    begin
        put(f3as, "_" & consulta(tn, t_proc(np).id));
        if t_proc(np).id_int /= 0 then
            put(f3as, "(");
            escriu_etiqueta(f3as, t_proc(np).id_int);
            put(f3as, ")");
        end if;
    end escriu_procediment;
    
    procedure gen_text_i3a (f3as  : in     ada.text_io.file_type;
                            instr : in     i3a) is
    begin
        if instr.tins /= ins_etiq then
            put(f3as, CAR_TAB);
        end if;
        
        case instr.tins is
            when ins_copia    => escriu_variable(f3as, instr.opdesti);
                                 put(f3as, " := ");
                                 escriu_variable(f3as, instr.opf1);
            when ins_neg      => escriu_variable(f3as, instr.opdesti);
                                 put(f3as, " := -");
                                 escriu_variable(f3as, instr.opf1);
            when ins_not      => escriu_variable(f3as, instr.opdesti);
                                 put(f3as, " := not ");
                                 escriu_variable(f3as, instr.opf1);
            when ins_consind  => escriu_variable(f3as, instr.opdesti);
                                 put(f3as, " := ");
                                 escriu_variable(f3as, instr.opf1);
                                 put(f3as, "[");
                                 escriu_variable(f3as, instr.opf2);
                                 put(f3as, "]");
            when ins_prod     => escriu_variable(f3as, instr.opdesti);
                                 put(f3as, " := ");
                                 escriu_variable(f3as, instr.opf1);
                                 put(f3as, " * ");
                                 escriu_variable(f3as, instr.opf2);
            when ins_divisio  => escriu_variable(f3as, instr.opdesti);
                                 put(f3as, " := ");
                                 escriu_variable(f3as, instr.opf1);
                                 put(f3as, " / ");
                                 escriu_variable(f3as, instr.opf2);
            when ins_modul    => escriu_variable(f3as, instr.opdesti);
                                 put(f3as, " := ");
                                 escriu_variable(f3as, instr.opf1);
                                 put(f3as, " mod ");
                                 escriu_variable(f3as, instr.opf2);
            when ins_suma     => escriu_variable(f3as, instr.opdesti);
                                 put(f3as, " := ");
                                 escriu_variable(f3as, instr.opf1);
                                 put(f3as, " + ");
                                 escriu_variable(f3as, instr.opf2);
            when ins_resta    => escriu_variable(f3as, instr.opdesti);
                                 put(f3as, " := ");
                                 escriu_variable(f3as, instr.opf1);
                                 put(f3as, " - ");
                                 escriu_variable(f3as, instr.opf2);
            when ins_and      => escriu_variable(f3as, instr.opdesti);
                                 put(f3as, " := ");
                                 escriu_variable(f3as, instr.opf1);
                                 put(f3as, " and ");
                                 escriu_variable(f3as, instr.opf2);
            when ins_or       => escriu_variable(f3as, instr.opdesti);
                                 put(f3as, " := ");
                                 escriu_variable(f3as, instr.opf1);
                                 put(f3as, " or ");
                                 escriu_variable(f3as, instr.opf2);
            when ins_copiaind => escriu_variable(f3as, instr.opdesti);
                                 put(f3as, "[");
                                 escriu_variable(f3as, instr.opf1);
                                 put(f3as, "]");
                                 put(f3as, " := ");
                                 escriu_variable(f3as, instr.opf2);
            when ins_etiq     => escriu_etiqueta(f3as, instr.opetiq);
                                 put(f3as, ":" & CAR_TAB & "skip");
            when ins_goto     => put(f3as,"goto ");
                                 escriu_etiqueta(f3as, instr.opetiq);
            when ins_ifeq     => put(f3as, "if ");
                                 escriu_variable(f3as, instr.opf1);
                                 put(f3as, " = ");
                                 escriu_variable(f3as, instr.opf2);
                                 put(f3as, " goto ");
                                 escriu_etiqueta(f3as, instr.opetiq);
            when ins_ifneq    => put(f3as, "if ");
                                 escriu_variable(f3as, instr.opf1);
                                 put(f3as, " /= ");
                                 escriu_variable(f3as, instr.opf2);
                                 put(f3as, " goto ");
                                 escriu_etiqueta(f3as, instr.opetiq);
            when ins_iflt     => put(f3as, "if ");
                                 escriu_variable(f3as, instr.opf1);
                                 put(f3as, " < ");
                                 escriu_variable(f3as, instr.opf2);
                                 put(f3as, " goto ");
                                 escriu_etiqueta(f3as, instr.opetiq);
            when ins_ifleq    => put(f3as, "if ");
                                 escriu_variable(f3as, instr.opf1);
                                 put(f3as, " <= ");
                                 escriu_variable(f3as, instr.opf2);
                                 put(f3as, " goto ");
                                 escriu_etiqueta(f3as, instr.opetiq);
            when ins_ifgt     => put(f3as, "if ");
                                 escriu_variable(f3as, instr.opf1);
                                 put(f3as, " > ");
                                 escriu_variable(f3as, instr.opf2);
                                 put(f3as, " goto ");
                                 escriu_etiqueta(f3as, instr.opetiq);
            when ins_ifgeq    => put(f3as, "if ");
                                 escriu_variable(f3as, instr.opf1);
                                 put(f3as, " >= ");
                                 escriu_variable(f3as, instr.opf2);
                                 put(f3as, " goto ");
                                 escriu_etiqueta(f3as, instr.opetiq);
            when ins_preambul => put(f3as,"preamb ");
                                 escriu_procediment(f3as, instr.opproc);
            when ins_call     => put(f3as,"call ");
                                 escriu_procediment(f3as, instr.opproc);
            when ins_return   => put(f3as,"rtn ");
                                 escriu_procediment(f3as, instr.opproc);
            when ins_params   => put(f3as,"params ");
                                 escriu_variable(f3as, instr.opf1);
            when ins_paramc   => put(f3as,"paramc ");
                                 escriu_variable(f3as, instr.opf1);
                                 put(f3as," ");
                                 escriu_variable(f3as, instr.opf2);
        end case;
        new_line(f3as);
    end gen_text_i3a;

    procedure gen_i3a (instr : in     i3a) is
    begin
        write(fi3ab, instr);
        gen_text_i3a (fi3as, instr);
    end gen_i3a;
    
    procedure gen_ins_copia (opd, opf1 : in     num_var) is
    begin
        gen_i3a((tins    => ins_copia,
                 opdesti => opd,
                 opf1    => opf1,
                 opf2    => VAR_NUL,
                 opproc  => PROC_NUL,
                 opetiq  => 0));
    end gen_ins_copia;
    
    procedure gen_ins_consind (opd, opf1, opf2 : in     num_var) is
    begin
        gen_i3a((tins    => ins_consind,
                 opdesti => opd,
                 opf1    => opf1,
                 opf2    => opf2,
                 opproc  => PROC_NUL,
                 opetiq  => 0));
    end gen_ins_consind;
    
    procedure gen_ins_copiaind (opd, opf1, opf2 : in     num_var) is
    begin
        gen_i3a((tins    => ins_copiaind,
                 opdesti => opd,
                 opf1    => opf1,
                 opf2    => opf2,
                 opproc  => PROC_NUL,
                 opetiq  => 0));
    end gen_ins_copiaind;
    
    procedure gen_ins_neg (opd, opf1 : in     num_var) is
    begin
        gen_i3a((tins    => ins_neg,
                 opdesti => opd,
                 opf1    => opf1,
                 opf2    => VAR_NUL,
                 opproc  => PROC_NUL,
                 opetiq  => 0));
    end gen_ins_neg;
    
    procedure gen_ins_suma (opd, opf1, opf2 : in     num_var) is
    begin
        gen_i3a((tins    => ins_suma,
                 opdesti => opd,
                 opf1    => opf1,
                 opf2    => opf2,
                 opproc  => PROC_NUL,
                 opetiq  => 0));
    end gen_ins_suma;
    
    procedure gen_ins_resta (opd, opf1, opf2 : in     num_var) is
    begin
        gen_i3a((tins    => ins_resta,
                 opdesti => opd,
                 opf1    => opf1,
                 opf2    => opf2,
                 opproc  => PROC_NUL,
                 opetiq  => 0));
    end gen_ins_resta;
    
    procedure gen_ins_prod (opd, opf1, opf2 : in     num_var) is
    begin
        gen_i3a((tins    => ins_prod,
                 opdesti => opd,
                 opf1    => opf1,
                 opf2    => opf2,
                 opproc  => PROC_NUL,
                 opetiq  => 0));
    end gen_ins_prod;
    
    procedure gen_ins_divisio (opd, opf1, opf2 : in     num_var) is
    begin
        gen_i3a((tins    => ins_divisio,
                 opdesti => opd,
                 opf1    => opf1,
                 opf2    => opf2,
                 opproc  => PROC_NUL,
                 opetiq  => 0));
    end gen_ins_divisio;
    
    procedure gen_ins_modul (opd, opf1, opf2 : in     num_var) is
    begin
        gen_i3a((tins    => ins_modul,
                 opdesti => opd,
                 opf1    => opf1,
                 opf2    => opf2,
                 opproc  => PROC_NUL,
                 opetiq  => 0));
    end gen_ins_modul;
    
    procedure gen_ins_and (opd, opf1, opf2 : in     num_var) is
    begin
        gen_i3a((tins    => ins_and,
                 opdesti => opd,
                 opf1    => opf1,
                 opf2    => opf2,
                 opproc  => PROC_NUL,
                 opetiq  => 0));
    end gen_ins_and;
    
    procedure gen_ins_or (opd, opf1, opf2 : in     num_var) is
    begin
        gen_i3a((tins    => ins_or,
                 opdesti => opd,
                 opf1    => opf1,
                 opf2    => opf2,
                 opproc  => PROC_NUL,
                 opetiq  => 0));
    end gen_ins_or;
    
    procedure gen_ins_not (opd, opf1 : in     num_var) is
    begin
        gen_i3a((tins    => ins_not,
                 opdesti => opd,
                 opf1    => opf1,
                 opf2    => VAR_NUL,
                 opproc  => PROC_NUL,
                 opetiq  => 0));
    end gen_ins_not;
    
    procedure gen_ins_etiq (ope : in     t_etiqueta) is
    begin
        gen_i3a((tins    => ins_etiq,
                 opdesti => VAR_NUL,
                 opf1    => VAR_NUL,
                 opf2    => VAR_NUL,
                 opproc  => PROC_NUL,
                 opetiq  => ope));
    end gen_ins_etiq;
    
    procedure gen_ins_goto (ope : in     t_etiqueta) is
    begin
        gen_i3a((tins    => ins_goto,
                 opdesti => VAR_NUL,
                 opf1    => VAR_NUL,
                 opf2    => VAR_NUL,
                 opproc  => PROC_NUL,
                 opetiq  => ope));
    end gen_ins_goto;
    
    procedure gen_ins_if_lt (opf1, opf2 : in     num_var;
                             ope        : in     t_etiqueta) is
    begin
        gen_i3a((tins    => ins_iflt,
                 opdesti => VAR_NUL,
                 opf1    => opf1,
                 opf2    => opf2,
                 opproc  => PROC_NUL,
                 opetiq  => ope));
    end gen_ins_if_lt;
    
    procedure gen_ins_if_leq (opf1, opf2 : in     num_var;
                              ope        : in     t_etiqueta) is
    begin
        gen_i3a((tins    => ins_ifleq,
                 opdesti => VAR_NUL,
                 opf1    => opf1,
                 opf2    => opf2,
                 opproc  => PROC_NUL,
                 opetiq  => ope));
    end gen_ins_if_leq;
    
    procedure gen_ins_if_eq (opf1, opf2 : in     num_var;
                             ope        : in     t_etiqueta) is
    begin
        gen_i3a((tins    => ins_ifeq,
                 opdesti => VAR_NUL,
                 opf1    => opf1,
                 opf2    => opf2,
                 opproc  => PROC_NUL,
                 opetiq  => ope));
    end gen_ins_if_eq;
    
    procedure gen_ins_if_neq (opf1, opf2 : in     num_var;
                              ope        : in     t_etiqueta) is
    begin
        gen_i3a((tins    => ins_ifneq,
                 opdesti => VAR_NUL,
                 opf1    => opf1,
                 opf2    => opf2,
                 opproc  => PROC_NUL,
                 opetiq  => ope));
    end gen_ins_if_neq;
    
    procedure gen_ins_if_geq (opf1, opf2 : in     num_var;
                              ope        : in     t_etiqueta) is
    begin
        gen_i3a((tins    => ins_ifgeq,
                 opdesti => VAR_NUL,
                 opf1    => opf1,
                 opf2    => opf2,
                 opproc  => PROC_NUL,
                 opetiq  => ope));
    end gen_ins_if_geq;
    
    procedure gen_ins_if_gt (opf1, opf2 : in     num_var;
                             ope        : in     t_etiqueta) is
    begin
        gen_i3a((tins    => ins_ifgt,
                 opdesti => VAR_NUL,
                 opf1    => opf1,
                 opf2    => opf2,
                 opproc  => PROC_NUL,
                 opetiq  => ope));
    end gen_ins_if_gt;
    
    procedure gen_ins_call (opp : in     num_proc) is
    begin
        gen_i3a((tins    => ins_call,
                 opdesti => VAR_NUL,
                 opf1    => VAR_NUL,
                 opf2    => VAR_NUL,
                 opproc  => opp,
                 opetiq  => 0));
    end gen_ins_call;
    
    procedure gen_ins_rtn (opp : in     num_proc) is
    begin
        gen_i3a((tins    => ins_return,
                 opdesti => VAR_NUL,
                 opf1    => VAR_NUL,
                 opf2    => VAR_NUL,
                 opproc  => opp,
                 opetiq  => 0));
    end gen_ins_rtn;
    
    procedure gen_ins_prmb (opp : in     num_proc) is
    begin
        gen_i3a((tins    => ins_preambul,
                 opdesti => VAR_NUL,
                 opf1    => VAR_NUL,
                 opf2    => VAR_NUL,
                 opproc  => opp,
                 opetiq  => 0));
    end gen_ins_prmb;
    
    procedure gen_ins_params (opf1 : in     num_var) is
    begin
        gen_i3a((tins    => ins_params,
                 opdesti => VAR_NUL,
                 opf1    => opf1,
                 opf2    => VAR_NUL,
                 opproc  => PROC_NUL,
                 opetiq  => 0));
    end gen_ins_params;
    
    procedure gen_ins_paramc (opf1, opf2 : in     num_var) is
    begin
        gen_i3a((tins    => ins_paramc,
                 opdesti => VAR_NUL,
                 opf1    => opf1,
                 opf2    => opf2,
                 opproc  => PROC_NUL,
                 opetiq  => 0));
    end gen_ins_paramc;
    
    
end p_semantica.gcodi.gcodi3a;
