package body decls.p_pila is

    procedure buida (p :    out t_pila) is
    begin
        p.index := 0;
    end buida;
    
    procedure empilar (p  : in out t_pila;
                       it : in     item) is
    begin
        if es_plena(p) then
            raise e_pila_plena;
        end if;
    
        p.index := p.index + 1;
        p.elem(p.index) := it;
    end empilar;
    
    procedure desempilar (p: in out t_pila) is
    begin
        if es_buida(p) then
            raise e_pila_buida;
        end if;
        
        p.index := p.index - 1;
    end desempilar;
    
    function cim (p : in t_pila) return item is
    begin
        if es_buida(p) then
            raise e_pila_buida;
        end if;
        
        return p.elem(p.index);
    end cim;
    
    function es_plena (p : in     t_pila) return boolean is
    begin
        return p.index = MAX_PILA;
    end es_plena;
    
    function es_buida (p : in     t_pila) return boolean is
    begin
        return p.index = 0;
    end es_buida;
    
end decls.p_pila;
