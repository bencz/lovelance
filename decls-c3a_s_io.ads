with ada.text_io;
use  ada.text_io;

package decls.c3a_s_io is

    procedure obrir (nom: in string);
    procedure tancar;
    procedure posainstr (ins: in i3a);
--    procedure llistapc (ins: out i3a);
--    procedure posaetiq (f3as: in ada.text_io.file_type; ins: in ins_c3a);
--    procedure concatena;

private

    procedure escriu_variable (f  : in file_type;
                               nv : in num_var);
    procedure escriu_etiqueta (f  : in file_type;
                               et : in etiqueta);
    procedure escriu_procediment (f  : in file_type;
                                  np : in num_proc);

    fc3as: ada.text_io.file_type;

end decls.c3a_s_io;
