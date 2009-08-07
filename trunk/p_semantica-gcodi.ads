with decls.dec_generals, decls.g_codi, decls.p_pila;
use  decls.dec_generals, decls.g_codi;

package p_semantica.gcodi is

    function nova_var (iden : in     id_nom) return num_var;
    function nova_const (v : in     valor) return num_var;
    function nova_etiq return t_etiqueta;

    procedure ins_var (var : in t_variable);
    procedure ins_proc (proc : in t_procediment);

    procedure prepara_gcodi (nom : in     string);
    procedure conclou_gcodi;

    procedure gc_prog;
    
    procedure gc_decl_prog;
    
    procedure gc_m_decl_prog;
    
    procedure gc_encap_id (iden : in     t_atribut);
    
    procedure gc_encap_p_encap (p_encap : in     t_atribut);
    
    procedure gc_p_encap_id (iden : in     t_atribut);
    
    procedure gc_p_encap_rec;
    
    procedure gc_argument;
    
    procedure gc_mode_in;
    
    procedure gc_mode_out;
    
    procedure gc_mode_in_out;
    
    procedure gc_decl_var (iden : in     t_atribut);
    
    procedure gc_c_decl_var;
    
    procedure gc_c_decl_var_rec (iden : in     t_atribut);
    
    procedure gc_decl_const (iden  : in     t_atribut;
                             liter : in     t_atribut);
    
    procedure gc_decl_record;
    
    procedure gc_p_record_rec;
    
    procedure gc_p_record;
    
    procedure gc_decl_array;
    
    procedure gc_p_array;
    
    procedure gc_p_array_rec;
    
    procedure gc_decl_subrang;
    
    procedure gc_lim_id;
    
    procedure gc_lim_lit;
    
    procedure gc_ref_id (ref  : in out t_atribut;
                         iden : in     t_atribut);
                         
    procedure gc_ref_rec (ref0 : in out t_atribut;
                          ref1 : in     t_atribut;
                          iden : in     t_atribut);
                               
    procedure gc_ref_prmb_rind (ref       : in out t_atribut;
                                prmb_rind : in     t_atribut);
                                
    procedure gc_prmb_rind (prmb_rind : in out t_atribut;
                            ref       : in     t_atribut;
                            e         : in     t_atribut);
                            
    procedure gc_prmb_rind_rec (prmb_rind0 : in out t_atribut;
                                prmb_rind1 : in     t_atribut;
                                e          : in     t_atribut);
                                     
    procedure gc_e_menys_unitari (e0 : in out t_atribut;
                                  e1 : in     t_atribut);
                                  
    procedure gc_e_par (e0 : in out t_atribut;
                        e1 : in     t_atribut);
                        
    procedure gc_e_lit (e0 : in out t_atribut;
                        e1 : in     t_atribut);
                        
    procedure gc_e_ref (e0 : in out t_atribut;
                        e1 : in     t_atribut);
                        
    procedure gc_op_major (e0 : in out t_atribut;
                           e1 : in     t_atribut;
                           e2 : in     t_atribut);
                           
    procedure gc_op_major_igual (e0 : in out t_atribut;
                                 e1 : in     t_atribut;
                                 e2 : in     t_atribut);
                                 
    procedure gc_op_menor (e0 : in out t_atribut;
                           e1 : in     t_atribut;
                           e2 : in     t_atribut);
                           
    procedure gc_op_menor_igual (e0 : in out t_atribut;
                                 e1 : in     t_atribut;
                                 e2 : in     t_atribut);
                                 
    procedure gc_op_igual (e0 : in out t_atribut;
                           e1 : in     t_atribut;
                           e2 : in     t_atribut);
                           
    procedure gc_op_diferent (e0 : in out t_atribut;
                              e1 : in     t_atribut;
                              e2 : in     t_atribut);
                              
    procedure gc_op_suma (e0 : in out t_atribut;
                          e1 : in     t_atribut;
                          e2 : in     t_atribut);
                          
    procedure gc_op_resta (e0 : in out t_atribut;
                           e1 : in     t_atribut;
                           e2 : in     t_atribut);
                           
    procedure gc_op_mult (e0 : in out t_atribut;
                          e1 : in     t_atribut;
                          e2 : in     t_atribut);
                          
    procedure gc_op_div (e0 : in out t_atribut;
                         e1 : in     t_atribut;
                         e2 : in     t_atribut);
                         
    procedure gc_op_and (e0 : in out t_atribut;
                         e1 : in     t_atribut;
                         e2 : in     t_atribut);
                         
    procedure gc_op_or (e0 : in out t_atribut;
                        e1 : in     t_atribut;
                        e2 : in     t_atribut);
                        
    procedure gc_op_not (e0 : in out t_atribut;
                         e1 : in     t_atribut);
                         
    procedure gc_op_mod (e0 : in out t_atribut;
                         e1 : in     t_atribut;
                         e2 : in     t_atribut);
                         
    procedure gc_sent_if;
    
    procedure gc_sent_if_else (marc : in     t_atribut);
    
    procedure gc_p_sent_if (e : in     t_atribut);
    
    procedure gc_m_if (marc : in out t_atribut);
    
    procedure gc_sent_while (p_sent : in     t_atribut);
    
    procedure gc_p_sent_while (p_sent : in out t_atribut;
                               marc   : in     t_atribut;
                               e      : in     t_atribut);
                               
    procedure gc_m_while (marc : in out t_atribut);
    
    procedure gc_sent_assig (ref : in     t_atribut;
                             e   : in     t_atribut);
    
    procedure gc_sent_crid (ref : in     t_atribut);

private
    
    package p_pila_params is new decls.p_pila(t_param);
    package p_pila_procs  is new decls.p_pila(num_proc);
    package p_pila_etiq   is new decls.p_pila(t_etiqueta);
    
    use p_pila_params, p_pila_procs, p_pila_etiq;
    
    type t_taula_variables    is array (num_var)  of t_variable;
    type t_taula_procediments is array (num_proc) of t_procediment;

    t_var  : t_taula_variables;
    t_proc : t_taula_procediments;
    
    p_params : p_pila_params.t_pila;
    p_procs  : p_pila_procs.t_pila;
    p_etiq   : p_pila_etiq.t_pila;
end p_semantica.gcodi;
