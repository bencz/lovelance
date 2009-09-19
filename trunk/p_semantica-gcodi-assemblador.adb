with decls.p_i3a, p_semantica.gcodi.gcodi3a,
     ada.text_io, decls.p_f_c3a_binari;

use  decls.p_i3a, p_semantica.gcodi.gcodi3a,
     ada.text_io, decls.p_f_c3a_binari;
    
package body p_semantica.gcodi.assemblador is

    procedure crea_fitxer_assemblador (nom : in     string) is
    begin
        create(fass, out_file, nom & ".s");
    end crea_fitxer_assemblador;

    procedure tanca_fitxer_assemblador is
    begin
        close(fass);
    end tanca_fitxer_assemblador;

    function elim_car_inicial (str : string) return string is
    begin
        if str(str'first) = '-' then
            return str;
        else
            return str(str'first + 1..str'last);
        end if;
    end elim_car_inicial;

    procedure escriu_seccio_data is
    begin
        put_line(fass, ".section .data");
        
        for i in 1..nv loop
            if t_var(i).tv = t_v_lit_str then
                put_line(fass, "    S" & 
                         elim_car_inicial(i'img) & 
                         ": .asciz """ & 
                         consulta(tn, t_var(i).str) & 
                         """");
            end if;
        end loop;
    end escriu_seccio_data;

    procedure escriu_seccio_bss is
    begin
        put_line(fass, ".section .bss");
        put_line(fass, CAR_TAB & ".comm DISP,100");
    end escriu_seccio_bss;


    procedure escriu_inici_seccio_instruccions is
    begin
        put_line(fass, ".section .text");
        put_line(fass, CAR_TAB & ".globl  _" & 
                 consulta(tn, t_proc(nprimer_procusu).id));
        new_line(fass);
    end escriu_inici_seccio_instruccions;

    procedure escriu_registre (r : in registre) is
    begin
        case r.a is
            when immediat        => if r.x >= 0 then
                                       put(fass, "$" &
                                           elim_car_inicial(r.x'img));
                                    else
                                      put(fass,"$" & r.x'img);
                                    end if;
                                    
            when absolut         => put(fass, r.x'img);
            when directe         => put(fass, "%" & r.r'img);
            when indirecte       => put(fass, "(%" & r.r'img&")");
            when indexat         => put(fass, r.di'img &
                                        "(%"&r.ri'img & ")");
                                        
            when immediat_disp   => put(fass, "$DISP");
            when immediat_string => put(fass, "$S" &
                                        elim_car_inicial(r.s'img));
        end case;
    end escriu_registre;


    procedure escriu_insass (i : in ins_ass) is
        j: num_proc;
    begin
        case i.ti is
            when movl  |
                 leal  |
                 addl  |
                 subl  |
                 imull |
                 xorl  |
                 orl   |
                 andl   => put (fass, "    " & i.ti'img & " ");
                           escriu_registre(i.src);
                           put (fass, ", ");
                           escriu_registre(i.dst);
                           
            when cmpl   => put(fass, "    " & i.ti'img & " ");
                           escriu_registre(i.opd1);
                           put(fass, ", ");
                           escriu_registre(i.opd2);
                           
            when idivl |
                 pushl |
                 popl  |
                 notl   => put(fass, "    " & i.ti'img & " ");
                           escriu_registre(i.oprd);
                           
            when jmp |
                 je  |
                 jne |
                 jl  |
                 jle |
                 jge |
                 jg     => put (fass, "    " & i.ti'img & " et_" & 
                                elim_car_inicial(i.dest'img));
                                
            when call   => put(fass, "    " & i.ti'img & " _" & 
                           consulta(tn, t_proc(i.dest_proc).id));
                           
            when sarl   => put (fass, "    " & i.ti'img & " $" & 
                                elim_car_inicial(i.despl'img) & ",");
                           escriu_registre(i.opd);
                           
            when ret    => put(fass, "    " & i.ti'img & " ");
            
            when etiq   => j:=1;
                           while (j <= np) and then 
                                 (t_proc(j).id_int /= i.dest) loop
                               j:= j+1;
                           end loop;
                           
                           if (j <= np) then
                               put(fass, "_" &
                                   consulta(tn, t_proc(j).id) &
                                   ": nop");
                           else
                               put(fass,"  et_" &
                                   elim_car_inicial(i.dest'img) &
                                   ": nop");
                           end if;
        end case;
      
        new_line(fass);
    end escriu_insass;


    procedure gen_comentari (ins3a : in i3a) is
    begin
        new_line(fass);
        put(fass, "# ");
        gen_text_i3a(fass, ins3a);
    end gen_comentari;


    procedure gen_movl (src, dst : in  registre) is
    begin
        escriu_insass((ti  => movl,
                       src => src,
                       dst => dst));
    end gen_movl;

    procedure gen_addl (src, dst : in  registre) is
    begin
        escriu_insass((ti  => addl,
                       src => src,
                       dst => dst));
    end gen_addl;

    procedure gen_leal (src, dst : in  registre) is
    begin
        escriu_insass((ti  => leal,
                       src => src,
                       dst => dst));
    end gen_leal;

    procedure gen_pushl (oprd : in registre) is
    begin
        escriu_insass((ti   => pushl,
                       oprd => oprd));
    end gen_pushl;

    procedure gen_popl (oprd : in registre) is
    begin
        escriu_insass((ti   => popl,
                       oprd => oprd));
    end gen_popl;

    procedure gen_xorl (src, dst : in registre) is
    begin
        escriu_insass((ti  => xorl,
                       src => src,
                       dst => dst));
    end gen_xorl;

    procedure gen_orl (src, dst : in  registre) is
    begin
        escriu_insass((ti  => orl,
                       src => src,
                       dst => dst));
    end gen_orl;

    procedure gen_andl (src, dst : in  registre) is
    begin
       escriu_insass((ti  => andl,
                      src => src,
                      dst => dst));
    end gen_andl;

    procedure gen_notl (oprd : in  registre) is
    begin
        escriu_insass((ti   => notl,
                       oprd => oprd));
    end gen_notl;

    procedure gen_subl (src, dst : in  registre) is
    begin
        escriu_insass((ti  => subl,
                       src => src,
                       dst => dst));
    end gen_subl;

    procedure gen_sarl (despl : in     integer;
                        opd   : in     registre) is
    begin
        escriu_insass((ti    => sarl,
                       despl => despl,
                       opd   => opd));
    end gen_sarl;

    procedure gen_idivl (oprd : in     registre) is
    begin
        escriu_insass((ti   => idivl,
                       oprd => oprd));
    end gen_idivl;

    procedure gen_imull (src, dst : in     registre) is
    begin
        escriu_insass((ti  => imull,
                       src => src,
                       dst => dst));
    end gen_imull;

    procedure gen_jmp (dest : in     t_etiqueta) is
    begin
        escriu_insass((ti   => jmp,
                       dest => dest));
    end gen_jmp;

    procedure gen_cmpl (opd1, opd2: in     registre) is
    begin
        escriu_insass((ti   => cmpl,
                       opd1 => opd1,
                       opd2 => opd2));
    end gen_cmpl;

    procedure gen_jge (dest : in     t_etiqueta) is
    begin
        escriu_insass((ti   => jge,
                       dest => dest));
    end gen_jge;

    procedure gen_jg (dest : in     t_etiqueta) is
    begin
        escriu_insass((ti   => jg,
                       dest => dest));
    end gen_jg;

    procedure gen_jl (dest : in     t_etiqueta) is
    begin
        escriu_insass((ti   => jl,
                       dest => dest));
    end gen_jl;

    procedure gen_jle(dest : in     t_etiqueta) is
    begin
        escriu_insass((ti   => jle,
                       dest => dest));
    end gen_jle;

    procedure gen_jne (dest : in     t_etiqueta) is
    begin
        escriu_insass((ti   => jne,
                       dest => dest));
    end gen_jne;

    procedure gen_je (dest: in     t_etiqueta) is
    begin
        escriu_insass((ti   => je,
                       dest => dest));
    end gen_je;

    procedure gen_call (dest : in     num_proc) is
    begin
        escriu_insass((ti        => call,
                     dest_proc => dest));
    end gen_call;

    procedure gen_ret is
    begin
        escriu_insass((ti => ret));
    end gen_ret;

    procedure gen_etiq (dest : in     t_etiqueta) is
        i: num_proc;
    begin
        escriu_insass((ti => etiq,
                       dest => dest));
        i:=1;
        
        while i <= np and then t_proc(i).id_int /= dest loop
            i :=  i + 1;
        end loop;
        
        if i <= np and i /= proc_actual then
            proc_actual := i;
        end if;
    end gen_etiq;

    procedure gen_load (origen : in     num_var;
                        dest   : in     tipus_registre) is
        d_var     : despl;
        prof_codi : nambits;
        prof_var  : nambits;
        prof_real : integer;
    begin
        if t_var(origen).tv = t_v_const then
            gen_movl((a => immediat, x => integer(t_var(origen).val)),
                     (a => directe, r => dest));
        else
            prof_codi := t_proc(proc_actual).prof;
            d_var     := t_var(origen).desp;
            prof_var  := t_proc(t_var(origen).np).prof;
            prof_real := integer(prof_var) * 4;
            
            if prof_codi = prof_var and d_var < 0 then
                -- Var Local
                gen_movl((a  => indexat,
                          di => d_var,
                          ri => ebp),
                         (a => directe,
                          r => dest));
            elsif prof_codi = prof_var and d_var > 0 then
                -- Paràmetre Local
                gen_movl((a  => indexat,
                          di => d_var,
                          ri => ebp),
                         (a => directe,
                          r => esi));
                          
                gen_movl ((a => indirecte,
                           r => esi),
                          (a => directe,
                           r => dest));
            elsif prof_codi > prof_var and d_var < 0 then
                -- Var Global
                gen_movl ((a => immediat_disp),
                          (a => directe,
                           r => esi));
                           
                gen_addl ((a => immediat,
                           x => prof_real),
                          (a => directe,
                           r => esi));
                           
                gen_movl ((a => indirecte,
                           r => esi),
                          (a => directe,
                           r => esi));
                           
                gen_movl ((a  => indexat,
                           di => d_var,
                           ri => esi),
                          (a => directe,
                           r => dest));
            elsif prof_codi > prof_var and d_var > 0 then
                -- Paràmetre global
                gen_movl ((a => immediat_disp),
                          (a => directe,
                           r => esi));
                           
                gen_addl ((a => immediat,
                           x => prof_real),
                          (a => directe,
                           r => esi));
                           
                gen_movl ((a => indirecte,
                           r => esi),
                          (a => directe,
                           r => esi));
                           
                gen_movl ((a  => indexat,
                           di => d_var,
                           ri => esi),
                          (a => directe,
                           r => esi));
                           
                gen_movl ((a => indirecte,
                           r => esi),
                          (a => directe,
                           r => dest));
            end if;
        end if;
    end gen_load;

    procedure gen_store (src: in tipus_registre; dest: in num_var) is
        d_var     : despl;
        prof_codi : nambits;
        prof_var  : nambits;
        prof_real : integer;
    begin
        d_var     := t_var(dest).desp;
        prof_var  := t_proc(t_var(dest).np).prof;
        prof_codi := t_proc(proc_actual).prof;
        prof_real := integer(prof_var) * 4;
        
        if prof_codi = prof_var and d_var < 0 then
            -- Var Local
            gen_movl ((a => directe,
                       r => src),
                      (a  => indexat,
                       di => d_var,
                       ri => ebp));
        elsif prof_codi = prof_var and d_var > 0 then
            -- Paràmetre Local
            gen_movl ((a  => indexat,
                       di => d_var,
                       ri => ebp),
                      (a => directe,
                       r => edi));
                  
            gen_movl ((a => directe,
                       r => src),
                      (a => indirecte,
                       r => edi));
        elsif prof_codi > prof_var and d_var < 0 then
            -- Var Global
            gen_movl ((a => immediat_disp),
                      (a => directe,
                       r => esi));
                      
            gen_addl ((a => immediat,
                       x => prof_real),
                      (a => directe,
                       r => esi));
                       
            gen_movl ((a => indirecte,
                       r => esi),
                      (a => directe,
                       r => edi));
                       
            gen_movl ((a => directe,
                       r => src),
                      (a  => indexat,
                       di => d_var,
                       ri => edi));
        elsif prof_codi > prof_var and d_var > 0 then
            -- Paràmetre global
            gen_movl ((a => immediat_disp),
                      (a => directe,
                       r => esi));
                       
            gen_addl ((a => immediat,
                       x => prof_real),
                      (a => directe,
                       r => esi));
                       
            gen_movl ((a => indirecte,
                       r => esi),
                      (a => directe,
                       r => esi));
                       
            gen_movl ((a  => indexat,
                       di => d_var,
                       ri => esi),
                      (a => directe,
                       r => edi));
                       
            gen_movl ((a => directe,
                       r => src),
                      (a => indirecte,
                       r => edi));
        end if;
    end gen_store;

    procedure gen_loadadr (src  : in     num_var;
                           dest : in     tipus_registre) is
        d_var     : despl;
        prof_codi : nambits;
        prof_var  : nambits;
        prof_real : integer;
    begin
        if t_var(src).tv = t_v_const then
            gen_movl ((a => immediat,
                       x => integer(t_var(src).val)),
                      (a => directe,
                       r => dest));
                       
            gen_store(dest, src);
        end if;
        
        if t_var(src).tv = t_v_lit_str then
            gen_movl((a => immediat_string,
                      s => src),
                     (a => directe,
                      r => dest));
        else
            d_var     := t_var(src).desp;
            prof_var  := t_proc(t_var(src).np).prof;
            prof_codi := t_proc(proc_actual).prof;
            prof_real := integer(prof_var) * 4;
            
            if prof_codi = prof_var and d_var < 0 then
                -- Var Local
                gen_leal ((a  => indexat,
                           di => d_var,
                           ri => ebp),
                          (a => directe,
                           r => dest));
            elsif prof_codi = prof_var and d_var > 0 then
                -- Paràmetre Local
                gen_movl ((a  => indexat,
                           di => d_var,
                           ri => ebp),
                          (a => directe,
                           r => dest));
            elsif prof_codi > prof_var and d_var < 0 then
                -- Var Global
                gen_movl ((a => immediat_disp),
                          (a => directe,
                           r => esi));
                           
                gen_addl ((a => immediat,
                           x => prof_real),
                          (a => directe,
                           r => esi));
                           
                gen_movl ((a => indirecte,
                           r => esi),
                          (a => directe,
                           r => esi));
                           
                gen_leal ((a  => indexat,
                           di => d_var,
                           ri => esi),
                          (a => directe,
                           r => dest));
            elsif prof_codi > prof_var and d_var > 0 then
                -- Paràmetre global
                gen_movl ((a => immediat_disp),
                          (a => directe,
                           r => esi));
                           
                gen_addl ((a => immediat,
                           x => prof_real),
                          (a => directe,
                           r => esi));
                           
                gen_movl ((a => indirecte,
                           r => esi),
                          (a => directe,
                           r => esi));
                           
                gen_movl ((a  => indexat,
                           di => d_var,
                           ri => esi),
                          (a => directe,
                           r => dest));
            end if;
        end if;
    end gen_loadadr;

    procedure gen_salt (ins3a : in     i3a) is
    begin
        gen_load (ins3a.opf1, eax);
        gen_load (ins3a.opf2, ebx);
        gen_cmpl ((a => directe,
                   r => ebx),
                  (a => directe,
                   r => eax));
        
        case (ins3a.tins) is
            when ins_iflt  => gen_jl (ins3a.opetiq);
            when ins_ifleq => gen_jle (ins3a.opetiq);
            when ins_ifeq  => gen_je (ins3a.opetiq);
            when ins_ifneq => gen_jne (ins3a.opetiq);
            when ins_ifgeq => gen_jge (ins3a.opetiq);
            when ins_ifgt  => gen_jg (ins3a.opetiq);
            when others    => null;
        end case;
    end gen_salt;

    procedure tradueix(ins3a: in i3a) is
    begin
        gen_comentari(ins3a);
        
        case (ins3a.tins) is
            when ins_copia    => gen_load (ins3a.opf1, eax);
                                 gen_store (eax, ins3a.opdesti);
                              
            when ins_neg      => gen_load(ins3a.opf1, eax);
                                 gen_xorl((a => directe,
                                           r => ebx),
                                          (a => directe,
                                           r => ebx));
                                        
                                 gen_subl((a => directe,
                                           r => eax),
                                          (a => directe,
                                           r => ebx));
                                          
                                 gen_store(ebx, ins3a.opdesti);
                              
            when ins_suma     => gen_load(ins3a.opf1, eax);
                                 gen_load(ins3a.opf2, ebx);
                                 gen_addl((a => directe,
                                           r => eax),
                                          (a => directe,
                                           r => ebx));
                                 gen_store(ebx,ins3a.opdesti);
                              
            when ins_resta    => gen_load(ins3a.opf1, eax);
                                 gen_load(ins3a.opf2, ebx);
                                 gen_subl((a => directe,
                                           r => ebx),
                                          (a => directe,
                                           r => eax));
                                 gen_store(eax, ins3a.opdesti);
                              
            when ins_prod     => gen_load(ins3a.opf1, eax);
                                 gen_load(ins3a.opf2, ebx);
                                 gen_imull((a => directe,
                                            r => eax),
                                           (a => directe,
                                            r => ebx));
                                 gen_store(ebx, ins3a.opdesti);
                               
            when ins_divisio  => gen_load(ins3a.opf1, eax);
                                 gen_load(ins3a.opf2, ebx);
                                 gen_movl((a => directe,
                                           r => eax),
                                          (a => directe,
                                           r => edx));
                                 gen_sarl(31, (a => directe,
                                               r => edx));
                                 gen_idivl((a => directe,
                                            r => ebx));
                                 gen_store(eax, ins3a.opdesti);
                                
            when ins_modul    => gen_load(ins3a.opf1, eax);
                                 gen_load(ins3a.opf2, ebx);
                                 gen_movl((a => directe,
                                           r => eax),
                                          (a => directe,
                                           r => edx));
                                 gen_sarl(31, (a => directe,
                                               r => edx));
                                 gen_idivl((a => directe,
                                            r => ebx));
                                gen_store(edx, ins3a.opdesti);
                                
            when ins_consind  => gen_loadadr(ins3a.opf1, esi);
                                 gen_load(ins3a.opf2, eax);
                                 gen_addl((a => directe,
                                           r => eax),
                                          (a => directe,
                                           r => esi));
                                 gen_movl((a => indirecte,
                                           r => esi),
                                          (a => directe,
                                           r => eax));
                                 gen_store(eax, ins3a.opdesti);
                                
            when ins_copiaind => gen_loadadr(ins3a.opdesti, edi);
                                 gen_load(ins3a.opf1, eax);
                                 gen_addl((a => directe,
                                           r => eax),
                                          (a => directe,
                                           r => edi));
                                 gen_load(ins3a.opf2, eax);
                                 gen_movl((a => directe,
                                           r => eax),
                                          (a => indirecte,
                                           r => edi));
                                           
            when ins_etiq     => gen_etiq(ins3a.opetiq);
            
            when ins_goto     => gen_jmp(ins3a.opetiq);
            
            when ins_params   => gen_loadadr(ins3a.opf1, eax);
                                 gen_pushl((a => directe,
                                            r => eax));
                                            
            when ins_paramc   => gen_loadadr(ins3a.opf1, eax);
                                 gen_load(ins3a.opf2, ebx);
                                 gen_addl((a => directe,
                                           r => ebx),
                                          (a => directe,
                                           r => eax));
                                           
                                 gen_pushl((a => directe,
                                            r => eax));
                                            
            when ins_call     => gen_call(ins3a.opproc);
                                 gen_addl((a => immediat,
                                           x => t_proc(ins3a.opproc).n_args
                                                * 4),
                                          (a => directe,
                                           r => esp));
                                           
            when ins_return   => gen_movl((a => directe,
                                           r => ebp),
                                          (a => directe,
                                           r => esp));
                                           
                                 gen_popl((a => directe,
                                           r => ebp));
                                           
                                 gen_movl((a => immediat_disp),
                                          (a => directe,
                                           r => edi));
                                           
                                 gen_popl((a  => indexat,
                                           di => despl(t_proc(ins3a.opproc).prof
                                                 *4),
                                           ri => edi));
                                 gen_ret;
                                 
            when ins_preambul => gen_movl((a => immediat_disp),
                                          (a => directe,
                                           r => esi));
                                           
                                 gen_pushl((a  => indexat,
                                            di => despl(t_proc(ins3a.opproc).prof)
                                                  *4,
                                           ri => esi));
                                           
                                 gen_pushl((a => directe,
                                            r => ebp));
                                            
                                 gen_movl((a => directe,
                                           r => esp),
                                          (a => directe,
                                           r => ebp));
                                           
                                 gen_movl((a => directe,
                                           r => ebp),
                                          (a  => indexat,
                                           di => despl(t_proc(ins3a.opproc).prof)
                                                 *4,
                                           ri => esi));
                                           
                                 gen_subl((a => immediat,
                                           x => t_proc(ins3a.opproc).ocup_vl),
                                          (a => directe,
                                           r => esp));
            when ins_iflt  |
                 ins_ifleq |
                 ins_ifeq  |
                 ins_ifneq |
                 ins_ifgeq |
                 ins_ifgt     => gen_salt(ins3a);
                 
            when ins_or       => gen_load(ins3a.opf1, eax);
                                 gen_load(ins3a.opf2, ebx);
                                 gen_orl((a => directe,
                                          r => eax),
                                         (a => directe,
                                          r => ebx));
                                 gen_store(ebx, ins3a.opdesti);
                                 
            when ins_and      => gen_load(ins3a.opf1, eax);
                                 gen_load(ins3a.opf2, ebx);
                                 gen_andl((a => directe,
                                           r => eax),
                                          (a => directe,
                                           r => ebx));
                                 gen_store(ebx, ins3a.opdesti);
                                 
            when ins_not      => gen_load(ins3a.opf1, eax);
                                 gen_notl((a => directe,
                                           r => eax));
                                 gen_store(eax, ins3a.opdesti);
        end case;
    end tradueix;


    procedure genera_assemblador (nom : in     string) is
        ic3a  : i3a;
        fi3ab : decls.p_f_c3a_binari.file_type;
    begin
        proc_actual := np;
        open(fi3ab, in_file, nom & ".c3a");

        crea_fitxer_assemblador(nom);
        escriu_seccio_data;
        escriu_seccio_bss;
        escriu_inici_seccio_instruccions;

        while not end_of_file(fi3ab) loop
            read(fi3ab, ic3a);
            tradueix(ic3a);
        end loop;
      
        tanca_fitxer_assemblador;
        close(fi3ab);
    end genera_assemblador;


    procedure genera_fitxer_compilacio (nom : in     string) is
    begin
        create(fbat, out_file, nom & ".bat");
        put_line (fbat, "gcc -c stdio/stdio.s");
        put_line (fbat, "gcc -c " & nom & ".s");
        put_line (fbat, "ld -o " & nom & ".exe -e _" &
                  consulta(tn, t_proc(nprimer_procusu).id) &
                  ' ' & nom & ".o stdio.o a\*.a");
        close(fbat);
    end genera_fitxer_compilacio;

end p_semantica.gcodi.assemblador;

