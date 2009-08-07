package body decls.c3a_b_io is

    procedure obrir (nom: in string)
    begin
        put_line("decls.c3a_b_io - obrir");
        create(f_c3ab, inout_file, nom&".c3ab");
    end prepara_gcodi_fitxer;

    procedure tancar is
    begin
        close(f_c3ab);
    end prepara_gcodi_fitxer;

    procedure posainstr (ins: in i3a)
    begin
        write (f_c3a, ins);
    end posainstr;

    procedure llistapc (ins: out i3a)
    begin
        read (f_c3a, ins);
    end llistapc;

--    procedure posaetiq (f3as: in ada.text_io.file_type; ins: in ins_c3a);
--    procedure concatena;

end decls.c3a_b_io;
