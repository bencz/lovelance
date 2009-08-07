package p_token is
                       
      type token is (
            end_of_input,       error,            pc_and,            
            pc_array,           pc_begin,         pc_constant,       
            pc_else,            pc_end,           pc_if,             
            pc_in,              pc_is,            pc_loop,           
            pc_mod,             pc_new,           pc_not,            
            pc_null,            pc_of,            pc_or,             
            pc_out,             pc_procedure,     pc_range,          
            pc_record,          pc_then,          pc_type,           
            pc_while,           identificador,    literal,           
            s_parobert,         s_partancat,      s_dospunts,        
            s_coma,             s_punticoma,      s_punt,            
            s_rang,             op_suma,          op_resta,          
            op_multiplicacio,   op_divisio,       op_major,          
            op_majorigual,      op_menor,         op_menorigual,      
            op_igual,           op_diferent,      op_assignacio);                
end p_token;
