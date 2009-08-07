with ada.text_io, ada.characters.handling;
use ada.text_io, ada.characters.handling;

package body decls.p_taula_noms is

    procedure tbuida (taula :    out t_taula_noms) is
    begin

        for i in id_tdispersio'range loop
            taula.td(i) := ID_NUL;
        end loop;

        taula.di := id_nom'first+1;
        taula.ds := id_str'first+1;
        taula.dc := id_tcaracters'first+1;    
    end tbuida;



    function hashing (nom: in string) 
                      return id_tdispersio is
                      
        bs: constant integer := character'pos(character'last)+1;

        type aa is array (nom'range) of integer;
        type ar is array (1..2 * nom'last) of integer;
        a: aa; -- Xifres numèriques equivalents a nom
        r: ar; -- Acumulador de les sumes del producte
        k, c, m, n: integer; -- Xifres que es porten

        long: constant integer := integer(MAX_IDENTIFICADORS);
            
    begin

        -- Inicialitzacions
        n := nom'last;
        c := 0;
        m := nom'length;

        -- Omplim el vector a que serà sobre el que farem les multiplicacions
        for i in 1..n loop 
            a(i) := character'pos(nom(i)); 
        end loop;

        -- Inicialitzam el vector de resultat
        for i in 1..2*n loop 
            r(i) := 0; 
        end loop;

        -- i representa l'index de la part de baix d'abaix
        for i in 1..n loop
            k := i - 1;
            c := 0;
            -- j representa l'index de la part de dalt 
            for j in 1..n loop
                c := c + r(k + j) + a(i) * a(j);
                r(k + j) := c mod bs; 
                c := c / bs;
            end loop;
            r(k + n + 1) := r(k + n + 1) + c;
        end loop;

        c := (r(n + 1) * bs + r(n)) mod long;
        return id_tdispersio(c + 1);
    end hashing;



    function igual(str : in     string;
                   tc  : in     t_taula_caracters;
                   itc : in     id_tcaracters) return boolean is
        i, j: integer;
    begin
        i := str'first;
        j := integer(itc);

        while str(integer(i)) = tc(integer(j)) and i < str'last loop
            i := i + 1;
            j := j + 1;
        end loop;

        return i = str'last and str(integer(i)) = tc(integer(j));
    end igual;
    


    procedure inserir(str   : in     string;
                      taula : in out t_taula_noms) is
    begin           
        for i in str'first..str'last loop
            taula.tc(integer(taula.dc)) := str(integer(i));
            taula.dc := taula.dc + 1;
        end loop;
        
        taula.tc(integer(taula.dc)) := ASCII.NUL;
        taula.dc := taula.dc + 1;
    end inserir;
    
   

    procedure posa (taula : in out t_taula_noms;
                    nom   : in     string;
                    idn   :    out id_nom) is

        pos_disp  : id_tdispersio;
        cont_disp : id_nom;
        nom_ins   : string := to_lower(nom);

    begin
        pos_disp  := hashing(nom_ins);
        cont_disp := taula.td(pos_disp);
               
        while cont_disp /= 0 and then
            not igual(nom, taula.tc, taula.ti(cont_disp).id) loop
            cont_disp := taula.ti(cont_disp).seg_id;
        end loop;
        
        if cont_disp = 0 then
            taula.ti(taula.di).id := taula.dc;
            taula.ti(taula.di).seg_id := taula.td(pos_disp);
            inserir(nom_ins, taula);
            
            taula.td(pos_disp) := taula.di;
            idn := taula.di;
            
            taula.di := taula.di + 1;
        else
            idn := cont_disp;
        end if;
    end posa;



    procedure posa (taula : in out t_taula_noms;
                    str   : in     string;
                    ids   :    out id_str) is
    begin
        taula.ts(taula.ds) := taula.dc;
        inserir(str, taula);
        ids := taula.ds;
        taula.ds := taula.ds + 1;
    end posa;



    function consulta (taula : in     t_taula_noms;
                       idn   : in     id_nom)
                       return string is

        primer, darrer :id_tcaracters;
    begin

        if idn > taula.di or idn <= 0 then
            raise id_no_valid;
        end if;

        primer := taula.ti(idn).id;
        darrer := primer;

        while (taula.tc(integer(darrer)) /= ASCII.NUL) loop
            darrer := darrer + 1;
        end loop;

        return taula.tc(integer(primer)..integer(darrer-1));
        
    end consulta;



    function consulta (taula : in     t_taula_noms;
                       ids   : in     id_str)
                       return string is
                       
        primer, darrer :id_tcaracters;
    begin
        
        if ids > taula.ds or ids <= 0 then
            raise id_no_valid;
        end if;

        primer := taula.ts(ids);
        darrer := primer;

        while (taula.tc(integer(darrer)) /= ASCII.NUL) loop
            darrer := darrer + 1;
        end loop;

        return taula.tc(integer(primer)..integer(darrer-1));
        
    end consulta;   



    -- Funcions de debugging
    procedure imprimir (taula : in t_taula_noms) is

        f_taula_dispersio: file_type;
        f_taula_identificadors: file_type;
        f_taula_strings: file_type;
        f_taula_caracters: file_type;

    begin  	
        create(f_taula_dispersio, out_file, "Taula_Dispersio.txt");
        create(f_taula_identificadors, out_file, "Taula_Identificadors.txt");
        create(f_taula_strings, out_file, "Taula_Strings.txt");
        create(f_taula_caracters, out_file, "Taula_Caracters.txt");

        for i in taula.td'first..taula.td'last loop
            put_line(f_taula_dispersio, i'img & " :___" & taula.td(i)'img);
        end loop;

        for i in taula.ti'first..taula.ti'last loop
            put_line(f_taula_identificadors, i'img & " :___" & taula.ti(i).id'img);
        end loop;
        
        for i in taula.ts'first..taula.ts'last loop
            put_line(f_taula_strings, i'img & " :___" & taula.ts(i)'img);
        end loop;

        for i in taula.tc'first..taula.tc'last loop		
            if taula.tc(i) /= ascii.nul then
                put_line(f_taula_caracters, i'img &"___"& taula.tc(i)'img);
            else
                new_line(f_taula_caracters, 1);
            end if;				
        end loop;

        close(f_taula_dispersio);
        close(f_taula_identificadors);
        close(f_taula_caracters);

      end imprimir;

end decls.p_taula_noms;
