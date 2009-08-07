with decls.c3a, ada.sequential_io;
use  decls.c3a;
package decls.c3a_b_io is

    package dfitxer_c3a_b is new ada.sequential_io (ins_c3a);

    type fitxer_c3a_b is private;

    procedure obrir (nom: in string);
    procedure tancar;
    procedure posainstr (ins: in i3a);
    procedure llistapc (ins: out i3a);

--    procedure posaetiq (f3as: in ada.text_io.file_type; ins: in ins_c3a);
--    procedure concatena;

private

    f_c3ab: decls.dfitxer_c3a_b.file_type;

end decls.c3a_b_io;
