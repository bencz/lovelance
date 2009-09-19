with decls.dec_generals;
use  decls.dec_generals;

package decls.p_descripcio is

    type valor    is new integer;
    type despl    is new integer;

    type tipus_subjacent is 
         (tsnul, tsbool, tscar, tsent, tsarr, tsrec, tsstr);

    type descr_tipus (ts : tipus_subjacent := tsnul) is
        record
            ocup : despl;
            case ts is
                when tsnul    => null;
                when tsbool |
                     tscar  |
                     tsent    => linf, lsup : valor;
                when tsarr    => tcomp      : id_nom;
                                 b          : despl;
                when tsrec    => null;
                when tsstr    => null;
            end case;
        end record;

    type tipus_descr is
        (dnul_la, dconst, dvar, dtipus, dproc, dcamp, darg);
    
    type descr (td : tipus_descr := dnul_la) is 
        record
            case td is
                when dnul_la => null;
                when dconst  => tc  : id_nom;
                                vc  : valor;
                                nc  : num_var;
                when dtipus  => dt  : descr_tipus;
                when dvar    => tv  : id_nom;
                                nv  : num_var;
                when dproc   => np  : num_proc;
                when dcamp   => tcp : id_nom;
                                dsp : despl;
                when darg    => ta  : id_nom;
                                na  : num_var;
            end case;
        end record;

end decls.p_descripcio;
