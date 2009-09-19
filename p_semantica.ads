with decls.p_atributs, decls.p_taula_noms, decls.p_taula_simbols,
     decls.dec_generals, decls.d_codi;
     
use decls.p_atributs, decls.p_taula_noms, decls.p_taula_simbols,
     decls.dec_generals, decls.d_codi;

package p_semantica is

    --procediments d'inicialitzacio i finalitzacio
    procedure prepara_analisi (nom : in     string);
    procedure conclou_analisi (e   :    out boolean);
    procedure prepara_predefinits;

    --rutines per crear els atributs
    procedure calc_num (atr      :    out t_atribut;
                        str      : in     string;
                        lin, col : in     natural);
                        
    procedure calc_car (atr      :    out t_atribut;
                        str      : in     string;
                        lin, col : in     natural);
                        
    procedure calc_str (atr      :    out t_atribut;
                        str      : in     string;
                        lin, col : in     natural);
                        
    procedure calc_id (atr      :    out t_atribut;
                       str      : in     string;
                       lin, col : in     natural);
                       
    procedure calc_atom (atr :    out t_atribut;
                         lin : in     natural;
                         col : in     natural);

    --rutines semantiques
    procedure rs_prog (decl_prog : in     t_atribut);
    
    procedure rs_decl_prog (decl_prog : in out t_atribut;
                            encap     : in     t_atribut;
                            ident     : in     t_atribut);
                            
    procedure rs_m_decl_prog;
    
    procedure rs_encap_id (encap : in out t_atribut;
                           iden  : in     t_atribut);
                           
    procedure rs_encap_p_encap (encap   : in out t_atribut;
                                p_encap : in     t_atribut);
                                
    procedure rs_p_encap_id (p_encap : in out t_atribut;
                             iden    : in     t_atribut;
                             argum   : in     t_atribut);
                             
    procedure rs_p_encap_rec (p_encap0 : in out t_atribut;
                                   p_encap1 : in     t_atribut;
                                   argum    : in     t_atribut);
                                   
    procedure rs_argument (argum : in out t_atribut;
                           iden0 : in     t_atribut;
                           mode  : in     t_atribut;
                           iden1 : in     t_atribut);
                           
    procedure rs_mode_in (mode : in out t_atribut);
    
    procedure rs_mode_out (mode : in out t_atribut);
    
    procedure rs_mode_in_out (mode : in out t_atribut);
    
    procedure rs_decl_var (decl_var   : in out t_atribut;
                           iden       : in     t_atribut;
                           c_decl_var : in     t_atribut);
                           
    procedure rs_c_decl_var (c_decl_var : in out t_atribut;
                             iden       : in     t_atribut);
                             
    procedure rs_c_decl_var_rec (c_decl_var0 : in out t_atribut;
                                 c_decl_var1 : in     t_atribut;
                                 iden        : in     t_atribut);
                                      
    procedure rs_decl_const (iden0 : in     t_atribut;
                             iden1 : in     t_atribut;
                             liter : in     t_atribut);
                             
    procedure rs_decl_record (p_record : in     t_atribut);
    
    procedure rs_p_record_rec (p_record0 : in out t_atribut;
                                    p_record1 : in     t_atribut;
                                    iden0     : in     t_atribut;
                                    iden1     : in     t_atribut);
                                    
    procedure rs_p_record (p_record : in out t_atribut;
                           iden0    : in     t_atribut;
                           iden1    : in     t_atribut;
                           iden2    : in     t_atribut);
                           
    procedure rs_decl_array (p_array : in     t_atribut;
                             iden    : in     t_atribut);
                             
    procedure rs_p_array (p_array : in out t_atribut;
                          iden0   : in     t_atribut;
                          iden1   : in     t_atribut);
                          
    procedure rs_p_array_rec (p_array0 : in out t_atribut;
                                   p_array1 : in     t_atribut;
                                   iden     : in     t_atribut);
                                   
    procedure rs_decl_subrang (iden0 : in     t_atribut;
                               iden1 : in     t_atribut;
                               lim0  : in     t_atribut;
                               lim1  : in     t_atribut);
                               
    procedure rs_lim_id (lim   : in out t_atribut;
                         ident : in     t_atribut);
                         
    procedure rs_lim_lit (lim : in out t_atribut;
                          lit : in     t_atribut);
                          
    procedure rs_ref_id (refer : in out t_atribut;
                         iden  : in     t_atribut);
                         
    procedure rs_ref_rec (refer0 : in out t_atribut;
                          refer1 : in     t_atribut;
                          iden   : in     t_atribut);
                               
    procedure rs_ref_prmb_rind (refer     : in out t_atribut;
                                prmb_rind : in     t_atribut);
                                
    procedure rs_prmb_rind (prmb_rind : in out t_atribut;
                            refer     : in     t_atribut;
                            e         : in     t_atribut);
                            
    procedure rs_prmb_rind_rec (prmb_rind0 : in out t_atribut;
                                     prmb_rind1 : in     t_atribut;
                                     e          : in     t_atribut);
                                     
    procedure rs_e_menys_unitari (e0 : in out t_atribut;
                                  e1 : in     t_atribut);
                                  
    procedure rs_e_par (e0 : in out t_atribut;
                        e1 : in     t_atribut);
                        
    procedure rs_e_lit (e0 : in out t_atribut;
                        e1 : in     t_atribut);
                        
    procedure rs_e_ref (e     : in out t_atribut;
                        refer : in     t_atribut);
                        
    procedure rs_op_major (e0     : in out t_atribut;
                           e1, e2 : in     t_atribut);
                           
    procedure rs_op_major_igual (e0     : in out t_atribut;
                                 e1, e2 : in     t_atribut);
                                 
    procedure rs_op_menor (e0     : in out t_atribut;
                           e1, e2 : in     t_atribut);
                           
    procedure rs_op_menor_igual (e0     : in out t_atribut;
                                 e1, e2 : in     t_atribut);
                                 
    procedure rs_op_igual (e0     : in out t_atribut;
                           e1, e2 : in     t_atribut);
                           
    procedure rs_op_diferent (e0     : in out t_atribut;
                              e1, e2 : in     t_atribut);
                              
    procedure rs_op_suma (e0     : in out t_atribut;
                          e1, e2 : in     t_atribut);
                          
    procedure rs_op_resta (e0     : in out t_atribut;
                           e1, e2 : in     t_atribut);
                           
    procedure rs_op_mult (e0     : in out t_atribut;
                          e1, e2 : in     t_atribut);
                          
    procedure rs_op_div (e0     : in out t_atribut;
                         e1, e2 : in     t_atribut);
                         
    procedure rs_op_and (e0     : in out t_atribut;
                         e1, e2 : in     t_atribut);
                         
    procedure rs_op_or (e0     : in out t_atribut;
                        e1, e2 : in     t_atribut);
                        
    procedure rs_op_not (e0 : in out t_atribut;
                         e1 : in     t_atribut);
                         
    procedure rs_op_mod (e0     : in out t_atribut;
                         e1, e2 : in     t_atribut);
                         
    procedure rs_sent_if;
    
    procedure rs_sent_if_else (marc : in     t_atribut);
    
    procedure rs_p_sent_if (e : in     t_atribut);
    
    procedure rs_m_if (marc : in out t_atribut);
    
    procedure rs_sent_while (p_sent : in     t_atribut);
    
    procedure rs_p_sent_while (p_sent : in out t_atribut;
                               marc   : in     t_atribut;
                               e      : in     t_atribut);
                               
    procedure rs_m_while (marc : in out t_atribut);
    
    procedure rs_sent_assig (refer : in     t_atribut;
                             e     : in     t_atribut);
                             
    procedure rs_sent_crid (refer : in     t_atribut);

private
    tn    : t_taula_noms;
    ts    : t_taula_simbols;
    error : boolean;
    nv    : num_var;
    np    : num_proc;
    ne    : t_etiqueta;
    
    ncert: num_var;
    nfals: num_var;

    nprimera_varusu : num_var;
    nprimer_procusu : num_proc;
end p_semantica;
