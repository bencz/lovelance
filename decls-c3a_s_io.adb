package body decls.c3a_b_io is

    procedure obrir (nom: in string)
    begin
        put_line("decls.c3a_b_io - obrir");
        create(f_c3as, out_file, nom&".c3as");
    end prepara_gcodi_fitxer;

    procedure tancar is
    begin
        close(f_c3as);
    end prepara_gcodi_fitxer;

    procedure posainstr (ins: in i3a)
    begin
        if ins.i /= ins_etiq then
            put(f3as, car_tabulador);
        end if;
        case ins.i is
            when ins_copia =>
                escriu_variable(f3as, ins.opdesti);
                put(f3as, " := ");
                escriu_variable(f3as, ins.opf1);
            when ins_neg =>
                escriu_variable(f3as, ins.opdesti);
                put(f3as, " := - ");
                escriu_variable(f3as, ins.opf1);
            when ins_not =>
                escriu_variable(f3as, ins.opdesti);
                put(f3as, " := not ");
                escriu_variable(f3as, ins.opf1);
            when ins_consind =>
                escriu_variable(f3as, ins.opdesti);
                put(f3as, " := ");
                escriu_variable(f3as, ins.opf1);
                put(f3as, "[");
                escriu_variable(f3as, ins.opf2);
                put(f3as, "]");
            when ins_prod =>
                escriu_variable(f3as, ins.opdesti);
                put(f3as, " := ");
                escriu_variable(f3as, ins.opf1);
                put(f3as, " * ");
                escriu_variable(f3as, ins.opf2);
            when ins_divisio =>
                escriu_variable(f3as, ins.opdesti);
                put(f3as, " := ");
                escriu_variable(f3as, ins.opf1);
                put(f3as, " / ");
                escriu_variable(f3as, ins.opf2);
            when ins_modul =>
                escriu_variable(f3as, ins.opdesti);
                put(f3as, " := ");
                escriu_variable(f3as, ins.opf1);
                put(f3as, " mod ");
                escriu_variable(f3as, ins.opf2);
            when ins_suma =>
                escriu_variable(f3as, ins.opdesti);
                put(f3as, " := ");
                escriu_variable(f3as, ins.opf1);
                put(f3as, " + ");
                escriu_variable(f3as, ins.opf2);
            when ins_resta =>
                escriu_variable(f3as, ins.opdesti);
                put(f3as, " := ");
                escriu_variable(f3as, ins.opf1);
                put(f3as, " - ");
                escriu_variable(f3as, ins.opf2);
            when ins_and =>
                escriu_variable(f3as, ins.opdesti);
                put(f3as, " := ");
                escriu_variable(f3as, ins.opf1);
                put(f3as, " and ");
                escriu_variable(f3as, ins.opf2);
            when ins_or =>
                escriu_variable(f3as, ins.opdesti);
                put(f3as, " := ");
                escriu_variable(f3as, ins.opf1);
                put(f3as, " or ");
                escriu_variable(f3as, ins.opf2);
            when ins_copiaind =>
                escriu_variable(f3as, ins.opdesti);
                put(f3as, "[");
                escriu_variable(f3as, ins.opf1);
                put(f3as, "]");
                put(f3as, " := ");
                escriu_variable(f3as, ins.opf2);
            when ins_etiq =>
                escriu_etiqueta(f3as, ins.opetiq);
                put(f3as, ":"&car_tabulador&"skip");
            when ins_goto=>
                put(f3as,"goto ");
                escriu_etiqueta(f3as, ins.opetiq);
            when ins_ifeq =>
                put(f3as, "if ");
                escriu_variable(f3as, ins.opf1);
                put(f3as, " = ");
                escriu_variable(f3as, ins.opf2);
                put(f3as, " goto ");
                escriu_etiqueta(f3as, ins.opetiq);
            when ins_ifne =>
                put(f3as, "if ");
                escriu_variable(f3as, ins.opf1);
                put(f3as, " /= ");
                escriu_variable(f3as, ins.opf2);
                put(f3as, " goto ");
                escriu_etiqueta(f3as, ins.opetiq);
            when ins_iflt =>
                put(f3as, "if ");
                escriu_variable(f3as, ins.opf1);
                put(f3as, " < ");
                escriu_variable(f3as, ins.opf2);
                put(f3as, " goto ");
                escriu_etiqueta(f3as, ins.opetiq);
            when ins_ifle =>
                put(f3as, "if ");
                escriu_variable(f3as, ins.opf1);
                put(f3as, " <= ");
                escriu_variable(f3as, ins.opf2);
                put(f3as, " goto ");
                escriu_etiqueta(f3as, ins.opetiq);
            when ins_ifgt =>
                put(f3as, "if ");
                escriu_variable(f3as, ins.opf1);
                put(f3as, " > ");
                escriu_variable(f3as, ins.opf2);
                put(f3as, " goto ");
                escriu_etiqueta(f3as, ins.opetiq);
            when ins_ifge =>
                put(f3as, "if ");
                escriu_variable(f3as, ins.opf1);
                put(f3as, " >= ");
                escriu_variable(f3as, ins.opf2);
                put(f3as, " goto ");
                escriu_etiqueta(f3as, ins.opetiq);
            when ins_preambul =>
                put(f3as,"preamb ");
                escriu_procediment(f3as, ins.opproc);
            when ins_call =>
                put(f3as,"call ");
                escriu_procediment(f3as, ins.opproc);
            when ins_return=>
                put(f3as,"rtn ");
                escriu_procediment(f3as, ins.opproc);
            when ins_params =>
                put(f3as,"params ");
                escriu_variable(f3as, ins.opf1);
            when ins_paramc =>
                put(f3as,"paramc ");
                escriu_variable(f3as, ins.opf1);
                put(f3as," ");
                escriu_variable(f3as, ins.opf2);
        end case;
        new_line(f3as);
    end posainstr;

end decls.c3a_b_io;
