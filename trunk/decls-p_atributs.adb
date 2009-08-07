package body decls.p_atributs is

    function valor_lit (lit : t_atribut) return valor is
        error : exception;
    begin
        case lit.a is
            when num     => return valor(lit.n);
            when car     => return valor(character'pos(lit.c));
            when lit_str => return valor(lit.str);
            when others  => raise error;
        end case;
        
        exception
            when error   => null;
    end valor_lit;
    
    --posar el tipus tsbool? Afegir el tipus d'atribut tsstr?
    function tsub_lit  (lit : t_atribut) return tipus_subjacent is
    begin
        case lit.a is
            when num     => return tsent;
            when car     => return tscar;
            when lit_str => return tsstr;
            when others  => return tsnul;
        end case;
    end tsub_lit;

end decls.p_atributs;
