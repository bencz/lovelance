with decls.p_taula_noms, decls.p_taula_simbols, decls.dec_generals, 
     decls.p_descripcio, decls.p_atributs;

use  decls.p_taula_noms, decls.p_taula_simbols, decls.dec_generals,
     decls.p_descripcio, decls.p_atributs;

package p_semantica.ctipus is

    procedure ct_prog (decl_prog : in     t_atribut;
                       error     : in out boolean);
                       
    procedure ct_decl_prog (decl_prog : in out t_atribut;
                            encap     : in     t_atribut;
                            ident     : in     t_atribut;
                            error     : in out boolean);
    
    procedure ct_encap_id (encap : in out t_atribut;
                           iden  : in     t_atribut;
                           error : in out boolean);
                           
    procedure ct_encap_p_encap (encap   : in out t_atribut;
                                p_encap : in     t_atribut);
                                
    procedure ct_p_encap_id (p_encap : in out t_atribut;
                             iden    : in     t_atribut;
                             argum   : in     t_atribut;
                             error   : in out boolean);
                             
    procedure ct_p_encap_rec (p_encap0 : in out t_atribut;
                              p_encap1 : in     t_atribut;
                              argum    : in     t_atribut;
                              error    : in out boolean);
                                   
    procedure ct_argument (argum : in out t_atribut;
                           iden0 : in     t_atribut;
                           mode  : in     t_atribut;
                           iden1 : in     t_atribut);
                           
    procedure ct_mode_in (mde  : in out t_atribut);
                          
    procedure ct_mode_out (mde  : in out t_atribut);
                           
    procedure ct_mode_in_out (mde  : in out t_atribut);
                              
    procedure ct_decl_var (decl_var   : in out t_atribut;
                           iden       : in     t_atribut;
                           c_decl_var : in     t_atribut;
                           error      : in out boolean);
                           
    procedure ct_c_decl_var (c_decl_var : in out t_atribut;
                             iden       : in     t_atribut;
                             error      : in out boolean);
                             
    procedure ct_c_decl_var_rec (c_decl_var0 : in out t_atribut;
                                 c_decl_var1 : in     t_atribut;
                                 iden        : in     t_atribut;
                                 error       : in out boolean);
                                      
    procedure ct_decl_const (iden0 : in     t_atribut;
                             iden1 : in     t_atribut;
                             liter : in     t_atribut;
                             error : in out boolean);
                             
    procedure ct_decl_record (p_record : in     t_atribut);
                              
    procedure ct_p_record_rec (p_record0 : in out t_atribut;
                               p_record1 : in     t_atribut;
                               iden0     : in     t_atribut;
                               iden1     : in     t_atribut;
                               error     : in out boolean);
                                    
    procedure ct_p_record (p_record : in out t_atribut;
                           iden0    : in     t_atribut;
                           iden1    : in     t_atribut;
                           iden2    : in     t_atribut;
                           error    : in out boolean);
                           
    procedure ct_decl_array (p_array : in     t_atribut;
                             iden    : in     t_atribut;
                             error   : in out boolean);
                             
    procedure ct_p_array (p_array : in out t_atribut;
                          iden0   : in     t_atribut;
                          iden1   : in     t_atribut;
                          error   : in out boolean);
                          
    procedure ct_p_array_rec (p_array0 : in out t_atribut;
                                   p_array1 : in     t_atribut;
                                   iden     : in     t_atribut;
                                   error    : in out boolean);
                                   
    procedure ct_decl_subrang (iden0 : in     t_atribut;
                               iden1 : in     t_atribut;
                               lim0  : in     t_atribut;
                               lim1  : in     t_atribut;
                               error : in out boolean);
                               
    procedure ct_lim_id (lim   : in out t_atribut;
                         ident : in     t_atribut;
                         error : in out boolean);
                         
    procedure ct_lim_lit (lim   : in out t_atribut;
                          lit   : in     t_atribut;
                          error : in out boolean);
                          
    procedure ct_ref_id (refer : in out t_atribut;
                         iden  : in     t_atribut;
                         error : in out boolean);
                         
    procedure ct_ref_rec (refer0 : in out t_atribut;
                          refer1 : in     t_atribut;
                          iden   : in     t_atribut;
                          error  : in out boolean);
                               
    procedure ct_ref_prmb_rind (refer     : in out t_atribut;
                                prmb_rind : in     t_atribut;
                                error     : in out boolean);
                                
    procedure ct_prmb_rind (prmb_rind : in out t_atribut;
                            refer     : in     t_atribut;
                            e         : in     t_atribut;
                            error     : in out boolean);
                            
    procedure ct_prmb_rind_rec (prmb_rind0 : in out t_atribut;
                                prmb_rind1 : in     t_atribut;
                                e          : in     t_atribut;
                                error      : in out boolean);
                                     
    procedure ct_e_menys_unitari (e0    : in out t_atribut;
                                  e1    : in     t_atribut;
                                  error : in out boolean);
                                  
    procedure ct_e_par (e0    : in out t_atribut;
                        e1    : in     t_atribut);
                        
    procedure ct_e_lit (e    : in out t_atribut;
                        lit  : in     t_atribut);
                        
    procedure ct_e_ref (e     : in out t_atribut;
                        refer : in     t_atribut);
                        
    procedure ct_op_rel (e0     : in out t_atribut;
                         e1, e2 : in     t_atribut;
                         error  : in out boolean);
                              
    procedure ct_op_arit (e0     : in out t_atribut;
                          e1, e2 : in     t_atribut;
                          error  : in out boolean);
                          
    procedure ct_op_bool (e0     : in out t_atribut;
                          e1, e2 : in     t_atribut;
                          error  : in out boolean);
                        
    procedure ct_op_not (e0    : in out t_atribut;
                         e1    : in     t_atribut;
                         error : in out boolean);
                         
    procedure ct_sent_if;
    
    procedure ct_sent_if_else (marc  : in     t_atribut;
                               error : in out boolean);

    procedure ct_p_sent_if (e     : in     t_atribut;
                            error : in out boolean);
                       
    procedure ct_sent_while (p_sent : in     t_atribut;
                             error  : in out boolean);
                             
    procedure ct_p_sent_while (p_sent : in out t_atribut;
                               marc   : in     t_atribut;
                               e      : in     t_atribut;
                               error  : in out boolean);
                          
    procedure ct_sent_assig (refer : in     t_atribut;
                             e     : in     t_atribut;
                             error : in out boolean);
                             
    procedure ct_sent_crid (refer : in     t_atribut;
                            error : in out boolean);

    err, e_previ : exception;

end p_semantica.ctipus;
