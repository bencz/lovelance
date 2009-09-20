
package body p_lexic is
function YYLex return Token is
subtype short is integer range -32768..32767;
    yy_act : integer;
    yy_c : short;

-- returned upon end-of-file
YY_END_TOK : constant integer := 0;
YY_END_OF_BUFFER : constant := 50;
subtype yy_state_type is integer;
yy_current_state : yy_state_type;
INITIAL : constant := 0;
yy_accept : constant array(0..120) of short :=
    (   0,
        0,    0,   50,   48,    1,    1,    1,   48,   29,   30,
       38,   36,   32,   37,   34,   39,   26,   31,   33,   42,
       44,   40,   25,   25,   25,   25,   25,   25,   25,   25,
       25,   25,   25,   25,   25,   25,    0,   27,    0,   47,
       35,   45,   26,   46,   43,   41,   25,   25,   25,    0,
       25,   25,   25,   25,    8,    9,   10,   25,   25,   25,
       25,   25,   16,   17,   25,   25,   25,   25,   25,   25,
       25,   28,   47,    2,   25,   25,   25,   25,   25,    7,
       25,   12,   13,   14,   25,   18,   25,   25,   25,   25,
       25,   25,   25,   25,   25,    6,   11,   15,   25,   25,

       25,   22,   23,   25,    3,    4,   25,   25,   20,   25,
       24,   25,   25,   21,   25,   25,    5,   25,   19,    0
    ) ;

yy_ec : constant array(ASCII.NUL..ASCII.DEL) of short :=
    (   0,
        1,    1,    1,    1,    1,    1,    1,    2,    2,    3,
        1,    4,    4,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    5,    6,    7,    6,    6,    6,    6,    8,    9,
       10,   11,   12,   13,   14,   15,   16,   17,   17,   17,
       17,   17,   17,   17,   17,   17,   17,   18,   19,   20,
       21,   22,    6,    6,   23,   24,   25,   26,   27,   28,
       29,   30,   31,   32,   32,   33,   34,   35,   36,   37,
       32,   38,   39,   40,   41,   32,   42,   32,   43,   32,
        6,    6,    6,    6,   44,    6,   23,   24,   25,   26,

       27,   28,   29,   30,   31,   32,   32,   33,   34,   35,
       36,   37,   32,   38,   39,   40,   41,   32,   42,   32,
       43,   32,    6,    6,    6,    6,    1
    ) ;

yy_meta : constant array(0..44) of short :=
    (   0,
        1,    1,    2,    2,    3,    3,    3,    3,    3,    3,
        3,    3,    3,    3,    3,    3,    4,    3,    3,    3,
        3,    3,    4,    4,    4,    4,    4,    4,    4,    4,
        4,    4,    4,    4,    4,    4,    4,    4,    4,    4,
        4,    4,    4,    5
    ) ;

yy_base : constant array(0..125) of short :=
    (   0,
        0,    0,  222,  223,  223,  223,  214,    0,  223,  223,
      223,  223,  223,  206,  204,  197,  200,  195,  223,  194,
      223,  193,   10,   19,   11,  169,   16,   22,   20,   23,
       35,   37,   14,   45,   39,   40,  205,  204,  202,    0,
      223,  223,  192,  223,  223,  223,  164,   47,   36,    0,
       48,   50,   49,   60,  163,  162,  161,   51,   64,   54,
       57,   66,  160,  159,   62,   67,   65,   80,   85,   70,
       82,  223,    0,  158,   92,  157,   86,   77,   91,  156,
       83,  155,  154,  153,   89,  149,   94,   96,   87,   93,
      105,  101,   98,  104,  103,  138,  137,  136,  117,  119,

      112,  135,  134,  124,  133,  132,  129,  127,  131,  128,
      130,  120,  116,  126,  118,  121,  125,  139,  123,  223,
      181,  184,  186,  191,   49
    ) ;

yy_def : constant array(0..125) of short :=
    (   0,
      120,    1,  120,  120,  120,  120,  121,  122,  120,  120,
      120,  120,  120,  120,  120,  120,  120,  120,  120,  120,
      120,  120,  123,  123,  123,  123,  123,  123,  123,  123,
      123,  123,  123,  123,  123,  123,  121,  120,  120,  124,
      120,  120,  120,  120,  120,  120,  123,  123,  123,  125,
      123,  123,  123,  123,  123,  123,  123,  123,  123,  123,
      123,  123,  123,  123,  123,  123,  123,  123,  123,  123,
      123,  120,  124,  123,  123,  123,  123,  123,  123,  123,
      123,  123,  123,  123,  123,  123,  123,  123,  123,  123,
      123,  123,  123,  123,  123,  123,  123,  123,  123,  123,

      123,  123,  123,  123,  123,  123,  123,  123,  123,  123,
      123,  123,  123,  123,  123,  123,  123,  123,  123,    0,
      120,  120,  120,  120,  120
    ) ;

yy_nxt : constant array(0..267) of short :=
    (   0,
        4,    5,    6,    5,    5,    4,    7,    8,    9,   10,
       11,   12,   13,   14,   15,   16,   17,   18,   19,   20,
       21,   22,   23,   24,   25,   26,   27,   26,   26,   26,
       28,   26,   29,   30,   31,   32,   33,   34,   26,   35,
       26,   36,   26,    4,   48,   51,   52,   49,   53,   55,
       54,   66,   76,   50,   50,   58,   56,   50,   59,   50,
       57,   60,   50,   50,   63,   50,   50,   67,   69,   71,
       61,   68,   74,   75,   64,   62,   77,   65,   50,   50,
       50,   70,   50,   50,   78,   80,   81,   79,   50,   82,
       50,   50,   50,   50,   50,   83,   84,   50,   85,   88,

       50,   86,   87,   50,   89,   50,   91,   50,   50,   50,
       50,   90,   92,   50,   93,   95,   94,   96,   99,   97,
       50,   98,  101,   50,  100,   50,   50,  102,   50,   50,
       50,  103,   50,  104,   50,   50,   50,   50,  106,   50,
      105,   50,  107,  108,   50,  109,   50,   50,   50,  110,
      111,  112,  113,  114,  115,   50,  116,  117,  118,   50,
       50,   50,   50,   50,   50,  119,   50,   50,   50,   50,
       50,   50,   50,   50,   50,   50,   50,   50,   50,   50,
       50,   50,   50,   37,   37,   37,   39,   39,   39,   47,
       47,   73,   50,   73,   73,   73,   50,   50,   50,   50,

       50,   50,   50,   50,   50,   50,   50,   50,   43,   72,
       37,   38,   50,   46,   45,   44,   43,   42,   41,   40,
       38,  120,    3,  120,  120,  120,  120,  120,  120,  120,
      120,  120,  120,  120,  120,  120,  120,  120,  120,  120,
      120,  120,  120,  120,  120,  120,  120,  120,  120,  120,
      120,  120,  120,  120,  120,  120,  120,  120,  120,  120,
      120,  120,  120,  120,  120,  120,  120
    ) ;

yy_chk : constant array(0..267) of short :=
    (   0,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,   23,   24,   25,   23,   27,   28,
       27,   33,  125,   23,   25,   29,   28,   33,   30,   27,
       28,   31,   24,   29,   32,   28,   30,   34,   35,   36,
       31,   34,   48,   49,   32,   31,   51,   32,   31,   49,
       32,   35,   35,   36,   52,   54,   58,   53,   34,   59,
       48,   51,   53,   52,   58,   60,   61,   60,   62,   67,

       61,   65,   66,   54,   68,   65,   70,   59,   67,   62,
       66,   69,   71,   70,   75,   78,   77,   79,   87,   81,
       78,   85,   89,   68,   88,   71,   81,   90,   69,   77,
       89,   91,   85,   92,   79,   75,   90,   87,   94,   88,
       93,   93,   95,   99,   92,  100,   95,   94,   91,  101,
      104,  107,  108,  110,  112,  101,  113,  115,  116,  113,
       99,  115,  100,  112,  116,  118,  119,  104,  117,  114,
      108,  110,  107,  111,  109,  106,  105,  103,  102,   98,
       97,   96,  118,  121,  121,  121,  122,  122,  122,  123,
      123,  124,   86,  124,  124,  124,   84,   83,   82,   80,

       76,   74,   64,   63,   57,   56,   55,   47,   43,   39,
       38,   37,   26,   22,   20,   18,   17,   16,   15,   14,
        7,    3,  120,  120,  120,  120,  120,  120,  120,  120,
      120,  120,  120,  120,  120,  120,  120,  120,  120,  120,
      120,  120,  120,  120,  120,  120,  120,  120,  120,  120,
      120,  120,  120,  120,  120,  120,  120,  120,  120,  120,
      120,  120,  120,  120,  120,  120,  120
    ) ;


-- copy whatever the last rule matched to the standard output

procedure ECHO is
begin
   if (text_io.is_open(user_output_file)) then
     text_io.put( user_output_file, yytext );
   else
     text_io.put( yytext );
   end if;
end ECHO;

-- enter a start condition.
-- Using procedure requires a () after the ENTER, but makes everything
-- much neater.

procedure ENTER( state : integer ) is
begin
     yy_start := 1 + 2 * state;
end ENTER;

-- action number for EOF rule of a given start state
function YY_STATE_EOF(state : integer) return integer is
begin
     return YY_END_OF_BUFFER + state + 1;
end YY_STATE_EOF;

-- return all but the first 'n' matched characters back to the input stream
procedure yyless(n : integer) is
begin
        yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
        yy_cp := yy_bp + n;
        yy_c_buf_p := yy_cp;
        YY_DO_BEFORE_ACTION; -- set up yytext again
end yyless;

-- redefine this if you have something you want each time.
procedure YY_USER_ACTION is
begin
        null;
end;

-- yy_get_previous_state - get the state just before the EOB char was reached

function yy_get_previous_state return yy_state_type is
    yy_current_state : yy_state_type;
    yy_c : short;
begin
    yy_current_state := yy_start;

    for yy_cp in yytext_ptr..yy_c_buf_p - 1 loop
	yy_c := yy_ec(yy_ch_buf(yy_cp));
	if ( yy_accept(yy_current_state) /= 0 ) then
	    yy_last_accepting_state := yy_current_state;
	    yy_last_accepting_cpos := yy_cp;
	end if;
	while ( yy_chk(yy_base(yy_current_state) + yy_c) /= yy_current_state ) loop
	    yy_current_state := yy_def(yy_current_state);
	    if ( yy_current_state >= 121 ) then
		yy_c := yy_meta(yy_c);
	    end if;
	end loop;
	yy_current_state := yy_nxt(yy_base(yy_current_state) + yy_c);
    end loop;

    return yy_current_state;
end yy_get_previous_state;

procedure yyrestart( input_file : file_type ) is
begin
   open_input(text_io.name(input_file));
end yyrestart;

begin -- of YYLex
<<new_file>>
        -- this is where we enter upon encountering an end-of-file and
        -- yywrap() indicating that we should continue processing

    if ( yy_init ) then
        if ( yy_start = 0 ) then
            yy_start := 1;      -- first start state
        end if;

        -- we put in the '\n' and start reading from [1] so that an
        -- initial match-at-newline will be true.

        yy_ch_buf(0) := ASCII.LF;
        yy_n_chars := 1;

        -- we always need two end-of-buffer characters. The first causes
        -- a transition to the end-of-buffer state. The second causes
        -- a jam in that state.

        yy_ch_buf(yy_n_chars) := YY_END_OF_BUFFER_CHAR;
        yy_ch_buf(yy_n_chars + 1) := YY_END_OF_BUFFER_CHAR;

        yy_eof_has_been_seen := false;

        yytext_ptr := 1;
        yy_c_buf_p := yytext_ptr;
        yy_hold_char := yy_ch_buf(yy_c_buf_p);
        yy_init := false;
-- UMASS CODES :
--   Initialization
        tok_begin_line := 1;
        tok_end_line := 1;
        tok_begin_col := 0;
        tok_end_col := 0;
        token_at_end_of_line := false;
        line_number_of_saved_tok_line1 := 0;
        line_number_of_saved_tok_line2 := 0;
-- END OF UMASS CODES.
    end if; -- yy_init

    loop                -- loops until end-of-file is reached

-- UMASS CODES :
--    if last matched token is end_of_line, we must
--    update the token_end_line and reset tok_end_col.
    if Token_At_End_Of_Line then
      Tok_End_Line := Tok_End_Line + 1;
      Tok_End_Col := 0;
      Token_At_End_Of_Line := False;
    end if;
-- END OF UMASS CODES.

        yy_cp := yy_c_buf_p;

        -- support of yytext
        yy_ch_buf(yy_cp) := yy_hold_char;

        -- yy_bp points to the position in yy_ch_buf of the start of the
        -- current run.
	yy_bp := yy_cp;
	yy_current_state := yy_start;
	loop
		yy_c := yy_ec(yy_ch_buf(yy_cp));
		if ( yy_accept(yy_current_state) /= 0 ) then
		    yy_last_accepting_state := yy_current_state;
		    yy_last_accepting_cpos := yy_cp;
		end if;
		while ( yy_chk(yy_base(yy_current_state) + yy_c) /= yy_current_state ) loop
		    yy_current_state := yy_def(yy_current_state);
		    if ( yy_current_state >= 121 ) then
			yy_c := yy_meta(yy_c);
		    end if;
		end loop;
		yy_current_state := yy_nxt(yy_base(yy_current_state) + yy_c);
	    yy_cp := yy_cp + 1;
if ( yy_current_state = 120 ) then
    exit;
end if;
	end loop;
	yy_cp := yy_last_accepting_cpos;
	yy_current_state := yy_last_accepting_state;

<<next_action>>
	    yy_act := yy_accept(yy_current_state);
            YY_DO_BEFORE_ACTION;
            YY_USER_ACTION;

        if aflex_debug then  -- output acceptance info. for (-d) debug mode
            text_io.put( Standard_Error, "--accepting rule #" );
            text_io.put( Standard_Error, INTEGER'IMAGE(yy_act) );
            text_io.put_line( Standard_Error, "(""" & yytext & """)");
        end if;

-- UMASS CODES :
--   Update tok_begin_line, tok_end_line, tok_begin_col and tok_end_col
--   after matching the token.
        if yy_act /= YY_END_OF_BUFFER and then yy_act /= 0 then
-- Token are matched only when yy_act is not yy_end_of_buffer or 0.
          Tok_Begin_Line := Tok_End_Line;
          Tok_Begin_Col := Tok_End_Col + 1;
          Tok_End_Col := Tok_Begin_Col + yy_cp - yy_bp - 1;
          if yy_ch_buf ( yy_bp ) = ASCII.LF then
            Token_At_End_Of_Line := True;
          end if;
        end if;
-- END OF UMASS CODES.

<<do_action>>   -- this label is used only to access EOF actions
            case yy_act is
		when 0 => -- must backtrack
		-- undo the effects of YY_DO_BEFORE_ACTION
		yy_ch_buf(yy_cp) := yy_hold_char;
		yy_cp := yy_last_accepting_cpos;
		yy_current_state := yy_last_accepting_state;
		goto next_action;


when 1 => 
--# line 35 "lovelace.l"
null;

--
--Paraules reservades
--
when 2 => 
--# line 39 "lovelace.l"
calc_atom(yylval, yy_line_number, yy_begin_column); return pc_and;

when 3 => 
--# line 40 "lovelace.l"
calc_atom(yylval, yy_line_number, yy_begin_column); return pc_array;

when 4 => 
--# line 41 "lovelace.l"
calc_atom(yylval, yy_line_number, yy_begin_column); return pc_begin;

when 5 => 
--# line 42 "lovelace.l"
calc_atom(yylval, yy_line_number, yy_begin_column); return pc_constant;

when 6 => 
--# line 43 "lovelace.l"
calc_atom(yylval, yy_line_number, yy_begin_column); return pc_else;

when 7 => 
--# line 44 "lovelace.l"
calc_atom(yylval, yy_line_number, yy_begin_column); return pc_end;

when 8 => 
--# line 45 "lovelace.l"
calc_atom(yylval, yy_line_number, yy_begin_column); return pc_if;

when 9 => 
--# line 46 "lovelace.l"
calc_atom(yylval, yy_line_number, yy_begin_column); return pc_in;

when 10 => 
--# line 47 "lovelace.l"
calc_atom(yylval, yy_line_number, yy_begin_column); return pc_is;

when 11 => 
--# line 48 "lovelace.l"
calc_atom(yylval, yy_line_number, yy_begin_column); return pc_loop;

when 12 => 
--# line 49 "lovelace.l"
calc_atom(yylval, yy_line_number, yy_begin_column); return pc_mod;

when 13 => 
--# line 50 "lovelace.l"
calc_atom(yylval, yy_line_number, yy_begin_column); return pc_new;

when 14 => 
--# line 51 "lovelace.l"
calc_atom(yylval, yy_line_number, yy_begin_column); return pc_not;

when 15 => 
--# line 52 "lovelace.l"
calc_atom(yylval, yy_line_number, yy_begin_column); return pc_null;

when 16 => 
--# line 53 "lovelace.l"
calc_atom(yylval, yy_line_number, yy_begin_column); return pc_of;

when 17 => 
--# line 54 "lovelace.l"
calc_atom(yylval, yy_line_number, yy_begin_column); return pc_or;

when 18 => 
--# line 55 "lovelace.l"
calc_atom(yylval, yy_line_number, yy_begin_column); return pc_out;

when 19 => 
--# line 56 "lovelace.l"
calc_atom(yylval, yy_line_number, yy_begin_column); return pc_procedure;

when 20 => 
--# line 57 "lovelace.l"
calc_atom(yylval, yy_line_number, yy_begin_column); return pc_range;

when 21 => 
--# line 58 "lovelace.l"
calc_atom(yylval, yy_line_number, yy_begin_column); return pc_record;

when 22 => 
--# line 59 "lovelace.l"
calc_atom(yylval, yy_line_number, yy_begin_column); return pc_then;

when 23 => 
--# line 60 "lovelace.l"
calc_atom(yylval, yy_line_number, yy_begin_column); return pc_type;

when 24 => 
--# line 61 "lovelace.l"
calc_atom(yylval, yy_line_number, yy_begin_column); return pc_while;

--
--Identificadors i literals
--
when 25 => 
--# line 65 "lovelace.l"
calc_id(yylval, yytext, yy_line_number, yy_begin_column); return identificador;

when 26 => 
--# line 66 "lovelace.l"
calc_num(yylval, yytext, yy_line_number, yy_begin_column); return literal;

when 27 => 
--# line 67 "lovelace.l"
calc_str(yylval, yytext, yy_line_number, yy_begin_column); return literal;

when 28 => 
--# line 68 "lovelace.l"
calc_car(yylval, yytext, yy_line_number, yy_begin_column); return literal;

--
--Simbols
--
when 29 => 
--# line 72 "lovelace.l"
calc_atom(yylval, yy_line_number, yy_begin_column); return s_parobert;

when 30 => 
--# line 73 "lovelace.l"
calc_atom(yylval, yy_line_number, yy_begin_column); return s_partancat;

when 31 => 
--# line 74 "lovelace.l"
calc_atom(yylval, yy_line_number, yy_begin_column); return s_dospunts;

when 32 => 
--# line 75 "lovelace.l"
calc_atom(yylval, yy_line_number, yy_begin_column); return s_coma;

when 33 => 
--# line 76 "lovelace.l"
calc_atom(yylval, yy_line_number, yy_begin_column); return s_punticoma;

when 34 => 
--# line 77 "lovelace.l"
calc_atom(yylval, yy_line_number, yy_begin_column); return s_punt;

when 35 => 
--# line 78 "lovelace.l"
calc_atom(yylval, yy_line_number, yy_begin_column); return s_rang;

--
--Operadors
--
when 36 => 
--# line 82 "lovelace.l"
calc_atom(yylval, yy_line_number, yy_begin_column); return op_suma;

when 37 => 
--# line 83 "lovelace.l"
calc_atom(yylval, yy_line_number, yy_begin_column); return op_resta;

when 38 => 
--# line 84 "lovelace.l"
calc_atom(yylval, yy_line_number, yy_begin_column); return op_multiplicacio;

when 39 => 
--# line 85 "lovelace.l"
calc_atom(yylval, yy_line_number, yy_begin_column); return op_divisio;

when 40 => 
--# line 86 "lovelace.l"
calc_atom(yylval, yy_line_number, yy_begin_column); return op_major;

when 41 => 
--# line 87 "lovelace.l"
calc_atom(yylval, yy_line_number, yy_begin_column); return op_majorigual;

when 42 => 
--# line 88 "lovelace.l"
calc_atom(yylval, yy_line_number, yy_begin_column); return op_menor;

when 43 => 
--# line 89 "lovelace.l"
calc_atom(yylval, yy_line_number, yy_begin_column); return op_menorigual;

when 44 => 
--# line 90 "lovelace.l"
calc_atom(yylval, yy_line_number, yy_begin_column); return op_igual;

when 45 => 
--# line 91 "lovelace.l"
calc_atom(yylval, yy_line_number, yy_begin_column); return op_diferent;

when 46 => 
--# line 92 "lovelace.l"
calc_atom(yylval, yy_line_number, yy_begin_column); return op_assignacio;

--
--Comentaris
--
when 47 => 
--# line 96 "lovelace.l"
null;

--
--Altres construccions
--
when 48 => 
--# line 100 "lovelace.l"
return error;

when 49 => 
--# line 101 "lovelace.l"
ECHO;
when YY_END_OF_BUFFER + INITIAL + 1 => 
    return End_Of_Input;
                when YY_END_OF_BUFFER =>
                    -- undo the effects of YY_DO_BEFORE_ACTION
                    yy_ch_buf(yy_cp) := yy_hold_char;

                    yytext_ptr := yy_bp;

                    case yy_get_next_buffer is
                        when EOB_ACT_END_OF_FILE =>
                            begin
                            if ( yywrap ) then
                                -- note: because we've taken care in
                                -- yy_get_next_buffer() to have set up yytext,
                                -- we can now set up yy_c_buf_p so that if some
                                -- total hoser (like aflex itself) wants
                                -- to call the scanner after we return the
                                -- End_Of_Input, it'll still work - another
                                -- End_Of_Input will get returned.

                                yy_c_buf_p := yytext_ptr;

                                yy_act := YY_STATE_EOF((yy_start - 1) / 2);

                                goto do_action;
                            else
                                --  start processing a new file
                                yy_init := true;
                                goto new_file;
                            end if;
                            end;
                        when EOB_ACT_RESTART_SCAN =>
                            yy_c_buf_p := yytext_ptr;
                            yy_hold_char := yy_ch_buf(yy_c_buf_p);
                        when EOB_ACT_LAST_MATCH =>
                            yy_c_buf_p := yy_n_chars;
                            yy_current_state := yy_get_previous_state;

                            yy_cp := yy_c_buf_p;
                            yy_bp := yytext_ptr;
                            goto next_action;
                        when others => null;
                        end case; -- case yy_get_next_buffer()
                when others =>
                    text_io.put( "action # " );
                    text_io.put( INTEGER'IMAGE(yy_act) );
                    text_io.new_line;
                    raise AFLEX_INTERNAL_ERROR;
            end case; -- case (yy_act)
        end loop; -- end of loop waiting for end of file
end YYLex;
--# line 101 "lovelace.l"
end p_lexic;

