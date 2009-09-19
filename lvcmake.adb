with p_sintactica, decls.p_taula_noms, decls.p_atributs, p_semantica,
     p_semantica.gcodi.assemblador, p_lexic, lovelace_io,
     ada.text_io, ada.command_line, ada.io_exceptions;
     
use p_sintactica, p_semantica.gcodi.assemblador, p_lexic, lovelace_io, 
    ada.text_io, ada.command_line, ada.io_exceptions;
    
procedure lvcmake is
    error_arguments  : exception;
    error_compilacio : exception;
    error : boolean;
begin

    if argument_count /= 1 then
        raise error_arguments;
    end if;
    
    open_input(argument(1) & ".lvc");
    p_semantica.prepara_analisi(argument(1));
    yyparse;
    p_semantica.conclou_analisi(error);
    close_input;

    if not error then
        genera_assemblador(argument(1));
        genera_fitxer_compilacio(argument(1));
        new_line;
        put_line("Programa compilat correctament." &
                 " Executi " & argument(1) & ".bat");
    else
        raise error_compilacio;
    end if;

    exception
        when error_arguments =>
            put_line ("Error: Ha d'introduir nomes un argument.");

        when ada.io_exceptions.name_error =>
            put_line ("Error: el fitxer '" & argument(1) &
                      ".lvc' no existeix.");
                      
        when error_compilacio =>
            put_line("Hi ha errors de compilacio, comprovi" &
                      " el fitxer errors.log");
end lvcmake;
