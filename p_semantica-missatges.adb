package body p_semantica.missatges is

    procedure inicia_missatges is
    begin
        create (f_missatges, out_file, "errors.log");
    end inicia_missatges;
    

    procedure finalitza_missatges is
    begin
        close(f_missatges);
    end finalitza_missatges;

    
    procedure me_posicio (lin : in     natural; 
                          col : in     natural) is
    begin
        put (f_missatges, "Linia: " & Integer'Image(lin) & " columna: " 
             & Integer'Image(col) & ". Error: ");
    end me_posicio;


    -- Programa principal amb arguments
    procedure me_args_princ (lin : in     natural;
                             col : in     natural) is
    begin
        me_posicio (lin, col);
        put_line (f_missatges, "el programa principal no pot tenir arguments");
    end me_args_princ;

    
    -- Identificadors diferents
    procedure me_difer_id (lin : in     natural; 
                           col : in     natural; 
                           id1 : in     id_nom; 
                           id2 : in     id_nom) is
    begin
        me_posicio (lin, col);
        put_line (f_missatges, "identificadors diferents: '" & 
                  consulta (tn, id1) & "' no coincideix amb '" & 
                  consulta (tn, id2) & "'.");
    end me_difer_id;


    -- Identificador existent                             
    procedure me_id_exist (lin : in     natural; 
                           col : in     natural; 
                           id  : in     id_nom) is
    begin
        me_posicio (lin, col);
        put_line (f_missatges, "identificador existent '" & 
                  consulta (tn, id) & "'.");
    end me_id_exist;
  

    -- No existeix el tipus    
    procedure me_no_tipus (lin : in     natural; 
                           col : in     natural; 
                           id  : in     id_nom) is
    begin
        me_posicio (lin, col);
        put_line (f_missatges, "'" & consulta (tn, id) & 
                  "' no correspon a un tipus.");
    end me_no_tipus;
    

    -- Argument existent
    procedure me_arg_exist (lin : in     natural; 
                            col : in     natural; 
                            id  : in     id_nom;
                            idp : in     id_nom) is
    begin
        me_posicio (lin, col);
        put_line (f_missatges, "argument existent '" & consulta (tn, id) 
                  & "' al procediment '" & consulta (tn, idp) & "'.");
    end me_arg_exist;
    

    -- No es un tipus escalar
    procedure me_no_escalar (lin : in     natural; 
                             col : in     natural; 
                             id  : in     id_nom) is
    begin
        me_posicio (lin, col);
        put_line (f_missatges, "'" & consulta (tn, id) & 
                  "' no correspon a un tipus escalar.");
    end me_no_escalar;
   

    -- Tipus subjacent no compatible
    procedure me_t_sub (lin : in     natural; 
                        col : in     natural; 
                        id1 : in     id_nom; 
                        id2 : in     t_atribut) is
    begin
        me_posicio (lin, col);
        put_line (f_missatges, "el tipus subjacent de '" & consulta (tn, id1) 
                  & "' no es compatible amb '" & 
                  valor'image (valor_lit (id2)) & "'.");

    end me_t_sub;
    

    -- Valor fora dels limits
    procedure me_fora_limits (lin  : in     natural; 
                              col  : in     natural; 
                              id1  : in     id_nom; 
                              id2  : in     t_atribut; 
                              linf : in     valor; 
                              lsup : in     valor) is
    begin
        me_posicio (lin, col);
        put_line (f_missatges, "el valor '" & valor'image (valor_lit (id2)) & 
                  "'no es troba dins els limits permesos de '" & 
                  consulta (tn, id1) & "': " & valor'image (linf) & 
                  ".." & valor'image (lsup) & ".");
    end me_fora_limits;
    
    
    -- El camp ja existeix   
    procedure me_camp_existent (lin : in     natural; 
                                col : in     natural; 
                                idr  : in     id_nom;
                                idc  : in     id_nom) is
    begin
        me_posicio (lin, col);
        put_line (f_missatges, "camp existent '" & consulta (tn, idc) & 
                  "' al record '" & consulta (tn, idr) & "'.");
    end me_camp_existent;                            
    

    -- Tipus existent
    procedure me_tipus_existent (lin : in    natural;
                                 col : in    natural;
                                 id  : in    id_nom) is
    begin
        me_posicio (lin, col);
        put_line (f_missatges, "tipus existent '" & 
                  consulta (tn, id) & "'.");
    end me_tipus_existent;
    

    -- Tipus del limit no compatible
    procedure me_tsub_no_comp (lin : in     natural; 
                               col : in     natural; 
                               id1 : in     id_nom; 
                               l   : in     valor) is
    begin
        me_posicio(lin, col);
        put_line (f_missatges, "el tipus del limit '" & valor'image (l) & 
                  "' no es compatible amb el de'" &consulta (tn, id1) & "'.");
    end me_tsub_no_comp;

    
    -- Valor fora dels limits
    procedure me_fora_limits (lin  : in     natural; 
                              col  : in     natural; 
                              id1  : in     id_nom; 
                              v    : in     valor;
                              linf : in     valor; 
                              lsup : in     valor) is 
    begin
        me_posicio (lin, col);
        put_line (f_missatges, "el valor del limit'" & valor'image (v) & 
                  "'no es troba dins els limits permesos de '" & 
                  consulta (tn, id1) & "': " & valor'image (linf) & 
                  ".." & valor'image (lsup) & ".");
    end me_fora_limits;
    

    -- Tipus del limit no compatible
    procedure me_limit_no_comp (lin : in     natural; 
                                col : in     natural; 
                                id1 : in     id_nom; 
                                l   : in     valor) is
    begin
        me_posicio(lin, col);
        put_line (f_missatges, "el tipus del limit '" & valor'image (l) & 
                  "' no es compatible amb el de'" & consulta (tn, id1) & "'.");
    end me_limit_no_comp;
    

    -- Els limits estan invetits
    procedure me_limits_invertits (lin  : in     natural; 
                                  col  : in     natural;
                                  linf : in     valor; 
                                  lsup : in     valor) is
    begin
        me_posicio (lin, col);
        put_line (f_missatges, "el valor del limit inferior '" & 
                  valor'image (linf) & "'es major que el valor " &
                  "limit superior'" & valor'image (lsup) & ".");
    end me_limits_invertits;
    

    -- No es una constant
    procedure me_no_constant (lin : in     natural; 
                             col : in     natural; 
                             id : in     id_nom) is
    begin
        me_posicio (lin, col);
        put_line (f_missatges, "'" & consulta (tn, id) & 
                  "' no correspon a una constant.");
    end me_no_constant;


    -- El limit no es un tipus escalar
    procedure me_noescalar (lin : in     natural; 
                            col : in     natural) is
    begin
        me_posicio (lin, col);
        put_line (f_missatges, "el tipus del limit no " &
                  "correspon a un tipus escalar.");
    end me_noescalar;


    -- Identificador no existent
    procedure me_id_inexist (lin : in     natural; 
                             col : in     natural; 
                             id  : in     id_nom) is
    begin
        me_posicio (lin, col);
        put_line (f_missatges, "identificador '" & 
                  consulta (tn, id) & "' no existent.");
    end me_id_inexist;    
    

    -- No es un tipus record
    procedure me_no_record (lin : in     natural; 
                            col : in     natural; 
                            id  : in     id_nom) is
    begin
        me_posicio (lin, col);
        put_line (f_missatges, "'" & consulta (tn, id) & 
                  "' no correspon a un tipus record.");
    end me_no_record;


    -- El camp no existeix
    procedure me_no_camp (lin : in     natural; 
                          col : in     natural; 
                          id1 : in     id_nom; 
                          id2 : in     id_nom) is
    begin
        me_posicio (lin, col);
        put_line (f_missatges, "el record '" & consulta (tn, id1) & 
                  "' no te el camp '" & consulta (tn, id2) & "'.");
    end me_no_camp;


    -- Falten indexos a l'array
    procedure me_falten_index (lin : in     natural; 
                               col : in     natural; 
                               id  : in     id_nom) is
    begin
        me_posicio (lin, col);
        put_line (f_missatges, "s'esperaven mes indexos a l'array '" &
                  consulta (tn, id) & "'.");
    end me_falten_index;

    
    -- Falten parametres al procediment
    procedure me_falten_param (lin : in     natural; 
                               col : in     natural; 
                               id  : in     id_nom) is
    begin
        me_posicio (lin, col);
        put_line (f_missatges, "s'esperaven mes parametres al procediment '" &
                  consulta (tn, id) & "'.");
    end me_falten_param;

    
    -- El tipus no es un array
    procedure me_no_array (lin : in     natural; 
                           col : in     natural; 
                           id  : in     id_nom) is
    begin
        me_posicio (lin, col);
        put_line (f_missatges, "'" & consulta (tn, id) & 
                  "' no correspon a un tipus array.");
    end me_no_array;
    

    -- Tipus no compatibles
    procedure me_tipus_no_comp (lin : in     natural; 
                                col : in     natural; 
                                id1 : in     id_nom; 
                                id2 : in     id_nom) is
    begin
        me_posicio (lin, col);
        if id1 /= ID_NUL then
            put_line (f_missatges, "el tipus de '" & consulta (tn, id1) & 
                      "' no es compatible amb '" & consulta (tn, id2) & "'.");
        else
            put_line (f_missatges, "el tipus de '" & consulta (tn, id2) & 
                      "' no es compatible amb el tipus de l'expressio.");
      end if;

    end me_tipus_no_comp;
    

    -- Procediment cridat amb arguments i declarat sense
    procedure me_no_param (lin : in     natural; 
                          col : in     natural; 
                          id  : in     id_nom) is
    begin
        me_posicio (lin, col);
        put_line (f_missatges, "el procediment '" & consulta (tn, id) & 
                  "' estava declarat sense arguments " &
                  "i es crida amb arguments.");
    end me_no_param;
    

    -- Mode de parametres incorrecte
    procedure me_mode_arg (lin : in     natural; 
                           col : in     natural; 
                           id1 : in     id_nom; 
                           id2 : in     id_nom) is
    begin
        me_posicio (lin, col);
        put_line (f_missatges, "el procediment '" & consulta (tn, id1) & 
                  "' te el parametre '" & consulta (tn, id2) & 
                  "' out o in out i no reb una variable.");
    end me_mode_arg;

    
    -- Massa indexos a l'array
    procedure me_massa_index (lin : in     natural; 
                              col : in     natural; 
                              id  : in     id_nom) is
    begin
        me_posicio (lin, col);
        put_line (f_missatges, "hi ha mes indexos a l'array '" & 
                  consulta (tn, id) & "' que els prevists a la declaracio.");
    end me_massa_index;

    
    -- Massa parametres al procediment
    procedure me_massa_param (lin : in     natural; 
                              col : in     natural; 
                              id  : in     id_nom) is
    begin
        me_posicio (lin, col);
        put_line (f_missatges, "el procediment '" & 
                  consulta (tn, id) & "' te massa parametres.");
    end me_massa_param;


    -- El tipus subjacent no es enter
    procedure me_no_enter (lin : in     natural; 
                           col : in     natural) is
    begin
      me_posicio (lin, col);
      put_line (f_missatges, "el tipus subjacent de l'operand " &
                "de l'expressio no correspon a un tipus enter.");    
    end me_no_enter;
    

    -- Referencia no permesa
    procedure me_t_ref (lin : in     natural;
                        col : in     natural) is
    begin
      me_posicio (lin, col);
      put_line (f_missatges, "referencia no permesa.");    
    end me_t_ref;

        
    -- Tipus subjacent no compatibles
    procedure me_no_compatibles (lin : in     natural; 
                                 col : in     natural; 
                                 id1 : in     id_nom; 
                                 id2 : in     id_nom) is
    begin
        me_posicio (lin, col);
        if id1 /= ID_NUL then
            put_line (f_missatges, "el tipus subjacent de '" 
                      & consulta (tn, id1) & "' no es compatible amb '" 
                      & consulta (tn, id2) & "'.");
        else
            put_line (f_missatges, "el tipus subjacent de '" & 
                      consulta (tn, id2) & "' no es compatible amb "
                      & "el tipus subjacent de l'expressio.");
        end if;
    end me_no_compatibles;
    

    -- El tipus subjacent no es escalar
    procedure me_no_escalar (lin : in     natural; 
                             col : in     natural) is
    begin
        me_posicio (lin, col);
        put_line (f_missatges, "el tipus subjacent dels dos operands " &
                  "de l'expressio no correspon a un tipus escalar.");
    end me_no_escalar;

    
    -- Tipus dels operands no compatibles
    procedure me_oper_no_comp (lin : in     natural; 
                               col : in     natural) is
    begin
        me_posicio (lin, col);
        put_line (f_missatges, "el tipus dels dos operands " &
                  "de l'expressio no son compatibles.");
    end me_oper_no_comp;


    -- El tipus subjacent no es enter
    procedure me_no_enter1 (lin : in     natural; 
                            col : in     natural) is
    begin
        me_posicio (lin, col);
        put_line (f_missatges, "el tipus subjacent del primer operand " & 
                  "de l'expressio no correspon a un tipus enter.");
    end me_no_enter1;
                 

    -- El tipus subjacent no es enter
    procedure me_no_enter2 (lin : in     natural; 
                            col : in     natural) is
    begin
        me_posicio (lin, col);
        put_line (f_missatges, "el tipus subjacent del segon operand " &
                  "de l'expressio no correspon a un tipus enter.");
    end me_no_enter2;                         

    
    -- El tipus subjacent no es boolea
    procedure me_no_bool1 (lin : in     natural; 
                           col : in     natural) is
    begin
        me_posicio (lin, col);
        put_line (f_missatges, "el tipus subjacent del primer operand " &
                  "de l'expressio no correspon a un tipus boolea.");
    end me_no_bool1;
                      

    -- El tipus subjacent no es boolea
    procedure me_no_bool2 (lin : in     natural; 
                           col : in     natural) is
    begin
        me_posicio (lin, col);
        put_line (f_missatges, "el tipus subjacent del segon operand " &
                  "de l'expressio no correspon a un tipus boolea.");
    end me_no_bool2;


    -- El tipus subjacent no es boolea
    procedure me_no_bool (lin : in     natural; 
                          col : in     natural) is
    begin
        me_posicio (lin, col);
        put_line (f_missatges, "el tipus subjacent de l'operand " &
                  "de l'expressio no correspon a un tipus boolea.");
    end me_no_bool;
    

    -- Tipus de la condicio no boolea
    procedure me_cond (lin : in     natural; 
                       col : in     natural) is
    begin
        me_posicio (lin, col);
        put_line (f_missatges, "el tipus subjacent de la condicio " &
                  "no correspon a un tipus boolea.");
    end me_cond;
    
    
    -- El tipus subjacent no es un tipus escalar
    procedure me_exp_no_esc (lin : in     natural; 
                             col : in     natural) is
    begin
        me_posicio (lin, col);
        put_line (f_missatges, "el tipus subjacent de l'expressio " &
                  "no correspon a un tipus escalar.");
    end me_exp_no_esc;


    -- Tipus subjacents no compatibles
    procedure me_no_compatibles (lin : in     natural; 
                                 col : in     natural) is
    begin
        me_posicio (lin, col);
        put_line (f_missatges, "incompatibilitat de " &
                  "tipus subjacent dels operands.");
    end me_no_compatibles;

    
    -- El tipus no correspon a una variable
    procedure me_no_variable (lin : in     natural; 
                              col : in     natural; 
                              id  : in     id_nom) is
    begin
        me_posicio (lin, col);
        put_line (f_missatges, "el tipus de '" & consulta (tn, id) & 
                  "' no correspon a una variable.");
    end me_no_variable;
    

    -- Procediment declarat amb arguments i cridat sense
    procedure me_param (lin : in     natural; 
                        col : in     natural; 
                        id  : in     id_nom) is
    begin
        me_posicio (lin, col);
        put_line (f_missatges, "el procediment '" & consulta (tn, id) & 
                  "' estava declarat amb arguments i es crida sense.");
    end me_param;
    

    -- No es un procediment
    procedure me_no_proc (lin : in     natural; 
                          col : in     natural; 
                          id  : in     id_nom) is
    begin
        me_posicio (lin, col);
        put_line (f_missatges, "'" & consulta (tn, id) & 
                  "' no es un procediment.");
    end me_no_proc;
    
    -- Tipus no compatibles
    procedure me_assig_no_comp (lin : in     natural; 
                                col : in     natural) is
    begin
        me_posicio (lin, col);
        put_line (f_missatges, "incompatibilitat de tipus en l'assignacio.");
    end me_assig_no_comp;
    
    
end p_semantica.missatges;
