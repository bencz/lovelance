with decls.dec_generals;
use  decls.dec_generals;

generic
    type item is private;

package decls.p_pila is
    type t_pila is limited private;
    
    procedure buida (p :    out t_pila);
    procedure empilar (p  : in out t_pila;
                       it : in     item);
    procedure desempilar (p: in out t_pila);
    function  cim (p : in     t_pila) return item;
    function  es_plena (p : in     t_pila) return boolean;
    function  es_buida (p : in     t_pila) return boolean;
private
    type a_pila is array (1..MAX_PILA) of item;
    
    type t_pila is
        record
            index : integer;
            elem  : a_pila;
        end record;
    
    e_pila_buida : exception;
    e_pila_plena : exception;
end decls.p_pila;
