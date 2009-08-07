with ada.text_io, decls.dec_generals, decls.p_descripcio;
use  ada.text_io, decls.dec_generals, decls.p_descripcio;

package p_semantica.missatges is
    procedure inicia_missatges;
    
    procedure finalitza_missatges;
    

    procedure me_args_princ (lin : in     natural;
                             col : in     natural);
                             
    -- Identificadors diferents
    procedure me_difer_id (lin : in     natural;
                           col : in     natural; 
                           id1 : in     id_nom; 
                           id2 : in     id_nom);
                             
    -- Identificador existent
    procedure me_id_exist (lin : in     natural;
                           col : in     natural; 
                           id  : in     id_nom);
               
    -- No existeix el tipus
    procedure me_no_tipus (lin : in     natural;
                           col : in     natural; 
                           id  : in     id_nom);
                
    -- Argument existent
    procedure me_arg_exist (lin : in     natural;
                            col : in     natural; 
                            id  : in     id_nom;
                            idp : in     id_nom);
                            
    -- No és un tipus escalar
    procedure me_no_escalar (lin : in     natural; 
                             col : in     natural; 
                             id  : in     id_nom);
                             
    -- Tipus subjacent no compatible
    procedure me_t_sub (lin : in     natural; 
                        col : in     natural; 
                        id1 : in     id_nom; 
                        id2 : in     t_atribut);
    
    -- Valor fora dels límits
    procedure me_fora_limits (lin  : in     natural; 
                              col  : in     natural; 
                              id1  : in     id_nom; 
                              id2  : in     t_atribut; 
                              linf : in     valor; 
                              lsup : in     valor);                            
                              
    -- El camp ja existeix
    procedure me_camp_existent (lin : in     natural; 
                                col : in     natural; 
                                idr : in     id_nom;
                                idc : in     id_nom);
                                  
    -- Tipus existent
    procedure me_tipus_existent (lin : in    natural;
                                 col : in    natural;
                                 id  : in    id_nom);

    -- Tipus del límit no compatible
    procedure me_tsub_no_comp (lin : in     natural; 
                               col : in     natural; 
                               id1 : in     id_nom; 
                               l   : in     valor);

    -- Valor fora dels límits
    procedure me_fora_limits (lin  : in     natural;
                              col  : in     natural; 
                              id1  : in     id_nom; 
                              v    : in     valor;
                              linf : in     valor; 
                              lsup : in     valor);
    
    -- Tipus del límit no compatible                          
    procedure me_limit_no_comp (lin : in     natural; 
                                col : in     natural; 
                                id1 : in     id_nom; 
                                l   : in     valor);
                                
    -- Els límits estàn invertits
    procedure me_limits_invertits (lin  : in     natural; 
                                  col  : in     natural;
                                  linf : in     valor; 
                                  lsup : in     valor);
    
    -- No és una constant            
    procedure me_no_constant (lin : in     natural; 
                              col : in     natural; 
                              id : in     id_nom);                             

    -- El límit no és un tipus escalar
    procedure me_noescalar (lin : in     natural; 
                            col : in     natural);

    -- Identificador no existent
    procedure me_id_inexist (lin : in     natural; 
                            col : in     natural; 
                            id  : in     id_nom);

    -- No és un tipus record
    procedure me_no_record (lin : in     natural; 
                            col : in     natural; 
                            id  : in     id_nom);
            
    -- El camp no existeix                
    procedure me_no_camp (lin : in     natural; 
                          col : in     natural; 
                          id1 : in     id_nom; 
                          id2 : in     id_nom);
                          
    -- Falten índexos a l'array
    procedure me_falten_index (lin : in     natural; 
                               col : in     natural; 
                               id  : in     id_nom);
    
    -- Falten paràmetres al procediment                        
    procedure me_falten_param (lin : in     natural; 
                               col : in     natural; 
                               id  : in     id_nom);
                               
    -- El tipus no és un array                           
    procedure me_no_array (lin : in     natural; 
                           col : in     natural; 
                           id  : in     id_nom);

    -- Tipus no compatibles                      
    procedure me_tipus_no_comp (lin : in     natural; 
                                col : in     natural; 
                                id1 : in     id_nom; 
                                id2 : in     id_nom);
                               
    -- Procediment cridat amb arguments i declarat sense
    procedure me_no_param (lin : in     natural; 
                          col : in     natural; 
                          id  : in     id_nom);
    
    -- Mode de paràmetres incorrecte                      
    procedure me_mode_arg (lin : in     natural; 
                           col : in     natural; 
                           id1 : in     id_nom; 
                           id2 : in     id_nom);
                      
    -- Massa índexos a l'array          
    procedure me_massa_index (lin : in     natural; 
                             col : in     natural; 
                             id  : in     id_nom);
    
    -- Massa paràmetres al procediment                         
    procedure me_massa_param (lin : in     natural; 
                              col : in     natural; 
                              id  : in     id_nom);
        
    -- El tipus subjacent no és enter
    procedure me_no_enter (lin : in     natural; 
                           col : in     natural);
                           
    -- Referència no permesa
    procedure me_t_ref (lin : in     natural;
                        col : in     natural);
                        
    -- Tipus subjacents no compatibles
    procedure me_no_compatibles (lin : in     natural; 
                                 col : in     natural; 
                                 id1 : in     id_nom; 
                                 id2 : in     id_nom);
                          
    -- El tipus subjacent no és escalar       
    procedure me_no_escalar (lin : in     natural; 
                             col : in     natural);
                             
    -- Tipus dels operands no compatibles
    procedure me_oper_no_comp (lin : in     natural; 
                               col : in     natural);
    
    -- El tipus subjacent no és enter
    procedure me_no_enter1 (lin : in     natural; 
                            col : in     natural);
    
    -- El tipus subjacent no és enter                       
    procedure me_no_enter2 (lin : in     natural; 
                            col : in     natural);

    -- El tipus subjacent no és booleà                   
    procedure me_no_bool1 (lin : in     natural; 
                           col : in     natural);

    -- El tipus subjacent no és booleà                            
    procedure me_no_bool2 (lin : in     natural; 
                           col : in     natural);

    -- El tipus subjacent no és booleà                           
    procedure me_no_bool (lin : in     natural; 
                          col : in     natural);
                          
    -- Tipus de la condició no booleà
    procedure me_cond (lin : in     natural; 
                       col : in     natural);
                       
    -- El tipus subjacent no es un tipus escalar
    procedure me_exp_no_esc (lin : in     natural; 
                             col : in     natural);
    
    -- Tipus subjacents no compatibles
    procedure me_no_compatibles (lin : in     natural; 
                                 col : in     natural);

    -- El tipus no correspon a una variable
    procedure me_no_variable (lin : in     natural; 
                              col : in     natural; 
                              id  : in     id_nom);
                              
    -- Procediment declarat amb arguments i cridat sense
    procedure me_param (lin : in     natural; 
                        col : in     natural; 
                        id  : in     id_nom);

    -- No és un procediment
    procedure me_no_proc (lin : in     natural; 
                          col : in     natural; 
                          id  : in     id_nom);
    
    -- Tipus no compatibles                                 
    procedure me_assig_no_comp (lin : in     natural; 
                                col : in     natural);
                          
private
    f_missatges: file_type;

end p_semantica.missatges;
