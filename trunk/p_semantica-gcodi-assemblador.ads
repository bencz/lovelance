with decls.dec_generals, decls.p_i3a, 
     ada.sequential_io, ada.text_io; 

use decls.dec_generals, decls.p_i3a, ada.text_io;

package p_semantica.gcodi.assemblador is

   procedure genera_assemblador (nom : in     string);
   procedure genera_fitxer_compilacio (nom : in     string);

private
   fass, fbat  : file_type;
   proc_actual : num_proc;

   type tipus_insass is (movl, addl, leal, pushl, popl,  xorl, orl,  andl, 
                         notl, subl, sarl, idivl, imull, jmp,  cmpl, jge, 
                         jg,   jl,   jle,  jne,   je,    call, ret,  etiq);

   type tipus_registre is (eax, ebx, ecx, edx, esi, edi, ebp, esp);

   type tipus_adressament is (immediat, 	     
                              immediat_disp,   
                              immediat_string,
                              absolut,		
                              indexat,
                              indirecte,
                              directe);

   type registre (a: tipus_adressament := directe) is record
      case a is
         when immediat | 
              absolut         => x  : integer;
         when directe | 
              indirecte       => r  : tipus_registre;
         when indexat         => di : despl;
                                 ri : tipus_registre;
         when immediat_disp   => null;
         when immediat_string => s  : num_var;
      end case;
   end record;

   type ins_ass (ti : tipus_insass := movl) is record
      case ti is
         when movl  | 
              leal  | 
              addl  | 
              subl  | 
              imull | 
              xorl  | 
              orl   | 
              andl    => src, dst : registre;
              
         when cmpl    => opd1, opd2 : registre;
         
         when pushl | 
              popl  | 
              idivl | 
              notl    => oprd : registre;
              
         when etiq | 
              jmp  | 
              je   | 
              jne  | 
              jl   | 
              jle  | 
              jge  | 
              jg      => dest : t_etiqueta;
              
         when call    => dest_proc : num_proc;
         
         when sarl    => despl : integer;
                         opd   : registre;
                      
         when ret     => null;
      end case;
   end record;

   package p_f_c3a_binari is new ada.sequential_io (i3a);
   use p_f_c3a_binari;

   fi3ab: p_f_c3a_binari.file_type;
   

end p_semantica.gcodi.assemblador;

