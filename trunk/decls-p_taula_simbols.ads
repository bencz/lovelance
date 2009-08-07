with decls.dec_generals, decls.p_descripcio;
use  decls.dec_generals, decls.p_descripcio;

package decls.p_taula_simbols is

    error_surtbloc     : exception;
    error_consultacamp : exception;
    error_posaindex    : exception;
    error_succindex    : exception;

    type t_taula_simbols is limited private;

    procedure tbuida (ts :    out t_taula_simbols);

    procedure posa (ts : in out t_taula_simbols;
                    id : in     id_nom;
                    d  : in     descr;
                    e  :    out boolean);

    function cons (ts : in    t_taula_simbols;
                   id : in    id_nom) return descr;
                   
    function consprof(ts : in t_taula_simbols) return nambits;

    procedure entrabloc (ts : in out t_taula_simbols);

    procedure surtbloc (ts : in out t_taula_simbols);

    procedure posacamp (ts       : in out t_taula_simbols;
                        idr, idc : in     id_nom;
                        d        : in     descr;
                        e        :    out boolean);

    function conscamp (ts       : in     t_taula_simbols;
                       idr, idc : in     id_nom) return descr;

    procedure posaindex (ts       : in out t_taula_simbols;
                         ida, idi : in     id_nom;
                         d        : in     descr);

    procedure consindex (ts      : in     t_taula_simbols;
                         indxarr : in     indexarr;
                         ida     :    out id_nom;
                         d       :    out descr);

    function primerindex (ts  : in     t_taula_simbols;
                          ida : in     id_nom) return indexarr;

    function succindex (ts : in     t_taula_simbols;
                        ci : in     indexarr) return indexarr;

    function esvalidarr (indxarr : in     indexarr) return boolean;

    procedure posaarg (ts       : in out t_taula_simbols;
                       idp, ida : in     id_nom;
                       d        : in     descr;
                       e        :    out boolean);

    function primerarg (ts  : in     t_taula_simbols;
                        idp : in     id_nom) return indexarg;
                       
    function succarg (ts      : in    t_taula_simbols;
                      indxarg : in    indexarg) return indexarg;

    procedure consarg (ts      : in     t_taula_simbols;
                       indxarg : in     indexarg;
                       ida     :    out id_nom;
                       d       :    out descr);
                      
    function esvalidarg (indxarg : in     indexarg) return boolean;

    procedure actualitza (ts : in out t_taula_simbols;
                          id : in     id_nom;
                          d  : in     descr);

private
   
--    type nambits is new integer range -1..MAX_AMBITS;

    type node_descr is
        record
            p : nambits;
            d : descr;
            s : ndesplacament;
        end record;

    type node_despl is
        record
            id : id_nom;
            p  : nambits;
            d  : descr;
            s  : ndesplacament;
        end record;

    type tdescripcio is array (id_nom) of node_descr;
    type tdesplacament is array (ndesplacament) of node_despl;
    type tambit is array (nambits) of ndesplacament;

    type t_taula_simbols is
        record
            tdc    : tdescripcio;
            tdp    : tdesplacament;
            ta     : tambit;
            nprof  : nambits;
            ndespl : ndesplacament;
        end record;

end decls.p_taula_simbols;
