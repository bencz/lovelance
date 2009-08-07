with decls.dec_generals;
use  decls.dec_generals;

package decls.p_taula_noms is
    tcaracters_plena      : exception;
    tidentificadors_plena : exception;
    tstrings_plena        : exception;
    id_no_valid           : exception;
    

    type t_taula_noms is limited private;

    procedure tbuida (taula :    out t_taula_noms);

    procedure posa (taula : in out t_taula_noms;
                    nom   : in     string;
                    idn   :    out id_nom);

    procedure posa (taula : in out t_taula_noms;
                    str   : in     string;
                    ids   :    out id_str);

    function consulta (taula : in     t_taula_noms;
                       idn   : in     id_nom)
                       return string;

    function consulta (taula : in     t_taula_noms;
                       ids   : in     id_str)
                       return string;

    -- Funcions de debugging
    procedure imprimir (taula : in     t_taula_noms);

private

    LONG_TDISPERSIO : constant integer := MAX_IDENTIFICADORS;
    LONG_TCARACTERS : constant integer := MAX_IDENTIFICADORS * (LONG_ID + 1) +
                                          MAX_STRINGS * (LONG_STR + 1);

    type id_tdispersio is new integer range 0..LONG_TDISPERSIO;
    type id_tcaracters is new integer range 0..LONG_TCARACTERS;

    type t_identificador is
        record
            id: id_tcaracters;
            seg_id: id_nom;
        end record;

    type t_taula_dispersio is
        array (id_tdispersio) of id_nom;

    type t_taula_identificadors is
        array (id_nom) of t_identificador;
    
    type t_taula_strings is
        array (id_str) of id_tcaracters;

    subtype t_taula_caracters is
        string (1..integer(id_tcaracters'last));

    type t_taula_noms is
        record
            td : t_taula_dispersio;
            ti : t_taula_identificadors;
            ts : t_taula_strings;
            tc : t_taula_caracters;
            di : id_nom;
            ds : id_str;
            dc : id_tcaracters;
        end record;

    function igual(str : in     string;
                   tc  : in     t_taula_caracters;
                   itc : in     id_tcaracters) return boolean;
                   
    procedure inserir(str   : in     string;
                      taula : in out t_taula_noms);

end decls.p_taula_noms;
