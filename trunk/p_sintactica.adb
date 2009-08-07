
with text_io;
with p_lexic;
with lovelace_tokens;
with lovelace_goto;
with lovelace_shift_reduce;
with p_semantica;

use text_io;
use p_lexic;
use lovelace_tokens;
use lovelace_goto;
use lovelace_shift_reduce;
use p_semantica;

package body p_sintactica is
	procedure yyerror(str : in string) is
	begin
		put_line(str & ": " & yylval.a'img);
		put_line("Linia: " & yylval.linia'img);
		put_line("Columna: " & yylval.columna'img);	
	end yyerror;
procedure YYParse is

   -- Rename User Defined Packages to Internal Names.
    package yy_goto_tables         renames
      Lovelace_Goto;
    package yy_shift_reduce_tables renames
      Lovelace_Shift_Reduce;
    package yy_tokens              renames
      Lovelace_Tokens;

   use yy_tokens, yy_goto_tables, yy_shift_reduce_tables;

   procedure yyerrok;
   procedure yyclearin;


   package yy is

       -- the size of the value and state stacks
       stack_size : constant Natural := 300;

       -- subtype rule         is natural;
       subtype parse_state  is natural;
       -- subtype nonterminal  is integer;

       -- encryption constants
       default           : constant := -1;
       first_shift_entry : constant :=  0;
       accept_code       : constant := -3001;
       error_code        : constant := -3000;

       -- stack data used by the parser
       tos                : natural := 0;
       value_stack        : array(0..stack_size) of yy_tokens.yystype;
       state_stack        : array(0..stack_size) of parse_state;

       -- current input symbol and action the parser is on
       action             : integer;
       rule_id            : rule;
       input_symbol       : yy_tokens.token;


       -- error recovery flag
       error_flag : natural := 0;
          -- indicates  3 - (number of valid shifts after an error occurs)

       look_ahead : boolean := true;
       index      : integer;

       -- Is Debugging option on or off
        DEBUG : constant boolean := FALSE;

    end yy;


    function goto_state
      (state : yy.parse_state;
       sym   : nonterminal) return yy.parse_state;

    function parse_action
      (state : yy.parse_state;
       t     : yy_tokens.token) return integer;

    pragma inline(goto_state, parse_action);


    function goto_state(state : yy.parse_state;
                        sym   : nonterminal) return yy.parse_state is
        index : integer;
    begin
        index := goto_offset(state);
        while  integer(goto_matrix(index).nonterm) /= sym loop
            index := index + 1;
        end loop;
        return integer(goto_matrix(index).newstate);
    end goto_state;


    function parse_action(state : yy.parse_state;
                          t     : yy_tokens.token) return integer is
        index      : integer;
        tok_pos    : integer;
        default    : constant integer := -1;
    begin
        tok_pos := yy_tokens.token'pos(t);
        index   := shift_reduce_offset(state);
        while integer(shift_reduce_matrix(index).t) /= tok_pos and then
              integer(shift_reduce_matrix(index).t) /= default
        loop
            index := index + 1;
        end loop;
        return integer(shift_reduce_matrix(index).act);
    end parse_action;

-- error recovery stuff

    procedure handle_error is
      temp_action : integer;
    begin

      if yy.error_flag = 3 then -- no shift yet, clobber input.
      if yy.debug then
          text_io.put_line("Ayacc.YYParse: Error Recovery Clobbers " &
                   yy_tokens.token'image(yy.input_symbol));
      end if;
        if yy.input_symbol = yy_tokens.end_of_input then  -- don't discard,
        if yy.debug then
            text_io.put_line("Ayacc.YYParse: Can't discard END_OF_INPUT, quiting...");
        end if;
        raise yy_tokens.syntax_error;
        end if;

            yy.look_ahead := true;   -- get next token
        return;                  -- and try again...
    end if;

    if yy.error_flag = 0 then -- brand new error
        yyerror("Syntax Error");
    end if;

    yy.error_flag := 3;

    -- find state on stack where error is a valid shift --

    if yy.debug then
        text_io.put_line("Ayacc.YYParse: Looking for state with error as valid shift");
    end if;

    loop
        if yy.debug then
          text_io.put_line("Ayacc.YYParse: Examining State " &
               yy.parse_state'image(yy.state_stack(yy.tos)));
        end if;
        temp_action := parse_action(yy.state_stack(yy.tos), error);

            if temp_action >= yy.first_shift_entry then
                if yy.tos = yy.stack_size then
                    text_io.put_line(" Stack size exceeded on state_stack");
                    raise yy_Tokens.syntax_error;
                end if;
                yy.tos := yy.tos + 1;
                yy.state_stack(yy.tos) := temp_action;
                exit;
            end if;

        Decrement_Stack_Pointer :
        begin
          yy.tos := yy.tos - 1;
        exception
          when Constraint_Error =>
            yy.tos := 0;
        end Decrement_Stack_Pointer;

        if yy.tos = 0 then
          if yy.debug then
            text_io.put_line("Ayacc.YYParse: Error recovery popped entire stack, aborting...");
          end if;
          raise yy_tokens.syntax_error;
        end if;
    end loop;

    if yy.debug then
        text_io.put_line("Ayacc.YYParse: Shifted error token in state " &
              yy.parse_state'image(yy.state_stack(yy.tos)));
    end if;

    end handle_error;

   -- print debugging information for a shift operation
   procedure shift_debug(state_id: yy.parse_state; lexeme: yy_tokens.token) is
   begin
       text_io.put_line("Ayacc.YYParse: Shift "& yy.parse_state'image(state_id)&" on input symbol "&
               yy_tokens.token'image(lexeme) );
   end;

   -- print debugging information for a reduce operation
   procedure reduce_debug(rule_id: rule; state_id: yy.parse_state) is
   begin
       text_io.put_line("Ayacc.YYParse: Reduce by rule "&rule'image(rule_id)&" goto state "&
               yy.parse_state'image(state_id));
   end;

   -- make the parser believe that 3 valid shifts have occured.
   -- used for error recovery.
   procedure yyerrok is
   begin
       yy.error_flag := 0;
   end yyerrok;

   -- called to clear input symbol that caused an error.
   procedure yyclearin is
   begin
       -- yy.input_symbol := yylex;
       yy.look_ahead := true;
   end yyclearin;


begin
    -- initialize by pushing state 0 and getting the first input symbol
    yy.state_stack(yy.tos) := 0;

    loop

        yy.index := shift_reduce_offset(yy.state_stack(yy.tos));
        if integer(shift_reduce_matrix(yy.index).t) = yy.default then
            yy.action := integer(shift_reduce_matrix(yy.index).act);
        else
            if yy.look_ahead then
                yy.look_ahead   := false;

                yy.input_symbol := yylex;
            end if;
            yy.action :=
             parse_action(yy.state_stack(yy.tos), yy.input_symbol);
        end if;


        if yy.action >= yy.first_shift_entry then  -- SHIFT

            if yy.debug then
                shift_debug(yy.action, yy.input_symbol);
            end if;

            -- Enter new state
            if yy.tos = yy.stack_size then
                text_io.put_line(" Stack size exceeded on state_stack");
                raise yy_Tokens.syntax_error;
            end if;
            yy.tos := yy.tos + 1;
            yy.state_stack(yy.tos) := yy.action;
              yy.value_stack(yy.tos) := yylval;

        if yy.error_flag > 0 then  -- indicate a valid shift
            yy.error_flag := yy.error_flag - 1;
        end if;

            -- Advance lookahead
            yy.look_ahead := true;

        elsif yy.action = yy.error_code then       -- ERROR

            handle_error;

        elsif yy.action = yy.accept_code then
            if yy.debug then
                text_io.put_line("Ayacc.YYParse: Accepting Grammar...");
            end if;
            exit;

        else -- Reduce Action

            -- Convert action into a rule
            yy.rule_id  := -1 * yy.action;

            -- Execute User Action
            -- user_action(yy.rule_id);


                case yy.rule_id is

when  1 =>
--#line  77
rs_prog(
yy.value_stack(yy.tos));

when  2 =>
--#line  86
rs_decl_prog(
yyval, 
yy.value_stack(yy.tos-8), 
yy.value_stack(yy.tos-1));

when  3 =>
--#line  90
rs_m_decl_prog;

when  4 =>
--#line  95
rs_encap_id(
yyval,
yy.value_stack(yy.tos));

when  5 =>
--#line  97
rs_encap_p_encap(
yyval,
yy.value_stack(yy.tos-1));

when  6 =>
--#line  102
rs_p_encap_id(
yyval, 
yy.value_stack(yy.tos-2), 
yy.value_stack(yy.tos));

when  7 =>
--#line  104
rs_p_encap_rec(
yyval, 
yy.value_stack(yy.tos-2), 
yy.value_stack(yy.tos));

when  8 =>
--#line  109
rs_argument(
yyval, 
yy.value_stack(yy.tos-3), 
yy.value_stack(yy.tos-1), 
yy.value_stack(yy.tos));

when  9 =>
--#line  114
rs_mode_in(
yyval);

when  10 =>
--#line  116
rs_mode_out(
yyval);

when  11 =>
--#line  118
rs_mode_in_out(
yyval);

when  18 =>
--#line  135
rs_decl_var(
yyval, 
yy.value_stack(yy.tos-1), 
yy.value_stack(yy.tos));

when  19 =>
--#line  140
rs_c_decl_var(
yyval, 
yy.value_stack(yy.tos-1));

when  20 =>
--#line  142
rs_c_decl_var_rec(
yyval, 
yy.value_stack(yy.tos), 
yy.value_stack(yy.tos-1));

when  21 =>
--#line  147
rs_decl_const(
yy.value_stack(yy.tos-6), 
yy.value_stack(yy.tos-3), 
yy.value_stack(yy.tos-1));

when  25 =>
--#line  158
rs_decl_record(
yy.value_stack(yy.tos-3));

when  26 =>
--#line  163
rs_p_record_rec(
yyval, 
yy.value_stack(yy.tos-4), 
yy.value_stack(yy.tos-3), 
yy.value_stack(yy.tos-1));

when  27 =>
--#line  165
rs_p_record(
yyval, 
yy.value_stack(yy.tos-6), 
yy.value_stack(yy.tos-3), 
yy.value_stack(yy.tos-1));

when  28 =>
--#line  170
rs_decl_array(
yy.value_stack(yy.tos-4), 
yy.value_stack(yy.tos-1));

when  29 =>
--#line  175
rs_p_array(
yyval, 
yy.value_stack(yy.tos-4), 
yy.value_stack(yy.tos));

when  30 =>
--#line  177
rs_p_array_rec(
yyval, 
yy.value_stack(yy.tos-2), 
yy.value_stack(yy.tos));

when  31 =>
--#line  182
rs_decl_subrang(
yy.value_stack(yy.tos-8), 
yy.value_stack(yy.tos-5), 
yy.value_stack(yy.tos-3), 
yy.value_stack(yy.tos-1));

when  32 =>
--#line  187
rs_lim_id(
yyval, 
yy.value_stack(yy.tos));

when  33 =>
--#line  189
rs_lim_lit(
yyval, 
yy.value_stack(yy.tos));

when  34 =>
--#line  194
rs_ref_id(
yyval, 
yy.value_stack(yy.tos));

when  35 =>
--#line  196
rs_ref_rec(
yyval, 
yy.value_stack(yy.tos-2), 
yy.value_stack(yy.tos));

when  36 =>
--#line  198
rs_ref_prmb_rind(
yyval, 
yy.value_stack(yy.tos-1));

when  37 =>
--#line  203
rs_prmb_rind(
yyval, 
yy.value_stack(yy.tos-2), 
yy.value_stack(yy.tos));

when  38 =>
--#line  205
rs_prmb_rind_rec(
yyval, 
yy.value_stack(yy.tos-2), 
yy.value_stack(yy.tos));

when  39 =>
--#line  210
rs_e_menys_unitari(
yyval, 
yy.value_stack(yy.tos));

when  40 =>
--#line  212
rs_e_par(
yyval, 
yy.value_stack(yy.tos-1));

when  41 =>
--#line  214
rs_e_lit(
yyval, 
yy.value_stack(yy.tos));

when  42 =>
--#line  216
rs_e_ref(
yyval, 
yy.value_stack(yy.tos));

when  43 =>
--#line  218
rs_op_major(
yyval, 
yy.value_stack(yy.tos-2), 
yy.value_stack(yy.tos));

when  44 =>
--#line  220
rs_op_major_igual(
yyval, 
yy.value_stack(yy.tos-2), 
yy.value_stack(yy.tos));

when  45 =>
--#line  222
rs_op_menor(
yyval, 
yy.value_stack(yy.tos-2), 
yy.value_stack(yy.tos));

when  46 =>
--#line  224
rs_op_menor_igual(
yyval, 
yy.value_stack(yy.tos-2), 
yy.value_stack(yy.tos));

when  47 =>
--#line  226
rs_op_igual(
yyval, 
yy.value_stack(yy.tos-2), 
yy.value_stack(yy.tos));

when  48 =>
--#line  228
rs_op_diferent(
yyval, 
yy.value_stack(yy.tos-2), 
yy.value_stack(yy.tos));

when  49 =>
--#line  230
rs_op_suma(
yyval, 
yy.value_stack(yy.tos-2), 
yy.value_stack(yy.tos));

when  50 =>
--#line  232
rs_op_resta(
yyval, 
yy.value_stack(yy.tos-2), 
yy.value_stack(yy.tos));

when  51 =>
--#line  234
rs_op_mult(
yyval, 
yy.value_stack(yy.tos-2), 
yy.value_stack(yy.tos));

when  52 =>
--#line  236
rs_op_div(
yyval, 
yy.value_stack(yy.tos-2), 
yy.value_stack(yy.tos));

when  53 =>
--#line  238
rs_op_and(
yyval, 
yy.value_stack(yy.tos-2), 
yy.value_stack(yy.tos));

when  54 =>
--#line  240
rs_op_or(
yyval, 
yy.value_stack(yy.tos-2), 
yy.value_stack(yy.tos));

when  55 =>
--#line  242
rs_op_not(
yyval, 
yy.value_stack(yy.tos));

when  56 =>
--#line  244
rs_op_mod(
yyval, 
yy.value_stack(yy.tos-2), 
yy.value_stack(yy.tos));

when  63 =>
--#line  263
rs_sent_if;

when  64 =>
--#line  265
rs_sent_if_else(
yy.value_stack(yy.tos-4));

when  65 =>
--#line  270
rs_p_sent_if(
yy.value_stack(yy.tos-1));

when  66 =>
--#line  274
rs_m_if(
yyval);

when  67 =>
--#line  279
rs_sent_while(
yy.value_stack(yy.tos-4));

when  68 =>
--#line  284
rs_p_sent_while(
yyval, 
yy.value_stack(yy.tos-2), 
yy.value_stack(yy.tos-1));

when  69 =>
--#line  288
rs_m_while(
yyval);

when  70 =>
--#line  293
rs_sent_assig(
yy.value_stack(yy.tos-3), 
yy.value_stack(yy.tos-1));

when  71 =>
--#line  298
rs_sent_crid(
yy.value_stack(yy.tos-1));

                    when others => null;
                end case;


            -- Pop RHS states and goto next state
            yy.tos      := yy.tos - rule_length(yy.rule_id) + 1;
            if yy.tos > yy.stack_size then
                text_io.put_line(" Stack size exceeded on state_stack");
                raise yy_Tokens.syntax_error;
            end if;
            yy.state_stack(yy.tos) := goto_state(yy.state_stack(yy.tos-1) ,
                                 get_lhs_rule(yy.rule_id));

              yy.value_stack(yy.tos) := yyval;

            if yy.debug then
                reduce_debug(yy.rule_id,
                    goto_state(yy.state_stack(yy.tos - 1),
                               get_lhs_rule(yy.rule_id)));
            end if;

        end if;


    end loop;

end yyparse;
end p_sintactica;
