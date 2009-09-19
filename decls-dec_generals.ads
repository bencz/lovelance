package decls.dec_generals is

    pragma pure;

    --longituds mitjanes d'identificadors i strings
    LONG_ID  : constant integer := 50;
    LONG_STR : constant integer := 200;

    MAX_IDENTIFICADORS : constant integer   := 255;  --aprox. a n. primers
    MAX_STRINGS        : constant integer   := 30;
    MAX_AMBITS         : constant integer   := 30;
    MAX_PROCS          : constant integer   := MAX_IDENTIFICADORS;
    MAX_VARS           : constant integer   := MAX_IDENTIFICADORS;
    DECS_AMBIT         : constant integer   := 1024; --mitjana d'ambits
    MAX_DESPLACAMENTS  : constant integer   := 100;
    MAX_PILA           : constant integer   := 200;
    CAR_NUL            : constant character := ascii.nul;
    CAR_TAB            : constant character := ascii.ht;

    type id_nom        is new integer range 0..MAX_IDENTIFICADORS;
    type id_str        is new integer range 0..MAX_STRINGS;
    type num_proc      is new integer range 0..MAX_PROCS;
    type num_var       is new integer range 0..MAX_VARS;
    type ndesplacament is new integer range 0..MAX_DESPLACAMENTS;
    type nambits       is new integer range 0..MAX_AMBITS;
    type comps_array   is new integer;
    type mde_exp       is (econst, evar, eres);
    type mde_ref       is (const, var);
    type t_mode        is (mdin, mdout, mdinout);
    type t_ref         is (rvar, rproc, rprocparam, rconst);

    
    subtype indexarr   is ndesplacament;
    subtype indexarg   is ndesplacament;
    
    ID_NUL   : constant id_nom   := 0;
    VAR_NUL  : constant num_var  := 0;
    PROC_NUL : constant num_proc := 0;

end decls.dec_generals;
