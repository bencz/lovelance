-- Copyright (c) 1990 Regents of the University of California.
-- All rights reserved.
--
--    The primary authors of ayacc were David Taback and Deepak Tolani.
--    Enhancements were made by Ronald J. Schmalz.
--
--    Send requests for ayacc information to ayacc-info@ics.uci.edu
--    Send bug reports for ayacc to ayacc-bugs@ics.uci.edu
--
-- Redistribution and use in source and binary forms are permitted
-- provided that the above copyright notice and this paragraph are
-- duplicated in all such forms and that any documentation,
-- advertising materials, and other materials related to such
-- distribution and use acknowledge that the software was developed
-- by the University of California, Irvine.  The name of the
-- University may not be used to endorse or promote products derived
-- from this software without specific prior written permission.
-- THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
-- IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
-- WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.

-- Module       : lr0_machine_body.ada
-- Component of : ayacc
-- Version      : 1.2
-- Date         : 11/21/86  12:31:19
-- SCCS File    : disk21~/rschm/hasee/sccs/ayacc/sccs/sxlr0_machine_body.ada

-- $Header: lr0_machine_body.a,v 0.1 86/04/01 15:06:56 ada Exp $ 
-- $Log:	lr0_machine_body.a,v $
-- Revision 0.1  86/04/01  15:06:56  ada
--  This version fixes some minor bugs with empty grammars 
--  and $$ expansion. It also uses vads5.1b enhancements 
--  such as pragma inline. 
-- 
-- 
-- Revision 0.0  86/02/19  18:37:23  ada
-- 
-- These files comprise the initial version of Ayacc
-- designed and implemented by David Taback and Deepak Tolani.
-- Ayacc has been compiled and tested under the Verdix Ada compiler
-- version 4.06 on a vax 11/750 running Unix 4.2BSD.
--  

with Symbol_Info, Text_IO;
use  Text_IO;
package body LR0_Machine is

  -- SCCS_ID : constant String := "@(#) lr0_machine_body.ada, Version 1.2";

    use  Parse_State_Set_Pack;
    use  Item_Set_Pack;
    use  Grammar_Symbol_Set_Pack;
    use  Transition_Set_Pack;

-----    type Item_Array is array (Item_Array_Index range <>) of Item;


    -- The type declarations for storing the nonterminal transitions of
    -- the DFA in the states.
-----    type Transition_Array is
-----	array(Integer range <>) of Transition;


    -- Note: terminal_goto is not yet used.
    type State is
	record
	    Nonterminal_Goto 	:     Transition_Array_Pointer  :=  null;
--&	    terminal_goto	:     transition_array_pointer  :=  null;
	    Kernel  		:     Item_Array_Pointer        :=  null;
	    Preds   		:     Parse_State_Set;
	end record;


    type State_Array                is array (Parse_State range <>) of State;
    type State_Array_Pointer        is access State_Array;

--& Due to a bug in the Verdix 4.06 compiler, we cannot have the
--& 'terminal_goto' declaration in type state. (Everything will compile,
--& but when it is run we get a bus error.) 
--& The following declarations are used to get around the Verdix bug.

    type State_2 is
	record
	    Terminal_Goto	:     Transition_Array_Pointer  :=  null;
	end record;

    type State_Array_2           is array(Parse_State range <>) of State_2;
    type State_Array_Pointer_2   is access State_Array_2;

    State_Info_2  : State_Array_Pointer_2;

--& End of Verdix bug fix.



    First_State			    :  constant Parse_State := 0;
    Last_State                      :  Parse_State;


    Max_State			    :  Parse_State; -- estimated max

    State_Info                      :  State_Array_Pointer := null;
    -- The pointer to array of state information.


    Estimated_Number_of_States : Integer;

    
    --									--
    --    The following arrays are used for looking up a state given    --
    --    the transition symbol. TERMINAL_GOTOS holds the states that   --
    --    have terminal transitions into them, NONTERMINAL_GOTOS holds  --
    --    states that have nonterminal transitions into them and        --
    --    OVERFLOW_GOTOS holds the overflow from the previous arrays.   --
    --									--
   

    type Goto_Array      is array (Grammar_Symbol range <>) of Parse_State;
    type Overflow_Array  is array (Parse_State range<>)     of Parse_State;

    type Goto_Array_Pointer     is access Goto_Array;
    type Overflow_Array_Pointer is access Overflow_Array;

    Terminal_Gotos     : Goto_Array_Pointer;
    Nonterminal_Gotos  : Goto_Array_Pointer;
    Overflow_Gotos     : Overflow_Array_Pointer;



    -- This array is used to store the items generated by get_closure
    -- and closure. 
    -- It is also declared to work around a VADS 5.1b bug for arrays whose
    -- bounds are set when the procedure is called (the memory is not 
    -- dealocated when the procedure is exited).
    Closure_Items  	       :   Item_Array_Pointer;

    type Boolean_Array         is array(Grammar_Symbol range <>) of Boolean;
    type Boolean_Array_Pointer is access Boolean_Array;
    Examined_Symbols	       :  Boolean_Array_Pointer;

------------------------------------------------------------------------------
    function Find_State
	(Kernel_Set : Item_Set;
	 Trans_Sym  : Grammar_Symbol) return Parse_State;



    function "<" (Item_1, Item_2: Item) return Boolean is
    begin
	if Item_1.Rule_ID = Item_2.Rule_ID then
	    return Item_1.Dot_Position < Item_2.Dot_Position;
	else
	    return Item_1.Rule_ID < Item_2.Rule_ID;
	end if;
    end "<";

    function "<" (Trans_1, Trans_2: Transition) return Boolean is
    begin
	if Trans_1.Symbol = Trans_2.Symbol then
	    return Trans_1.State_ID < Trans_2.State_ID;
	else
	    return Trans_1.Symbol < Trans_2.Symbol;
	end if;
    end "<";


    procedure Get_Closure
	(State_ID       : in  Parse_State; 
	 Closure_Items  : in  Item_Array_Pointer;
	 Last           : out Item_Array_Index);

    procedure Make_LR0_States is

	Goto_Set	: Item_Set;
	Current_State   : Parse_State;
	Goto_State	: Parse_State;

	Gotos		: Transition_Array_Pointer;

	Nt_Trans	: Transition;
	Nt_Trans_Set	: Transition_Set;
	Nt_Trans_Iter	: Transition_Iterator;

	T_Trans		: Transition;
	T_Trans_Set	: Transition_Set;
	T_Trans_Iter	: Transition_Iterator;

	I   : Item;
	Sym : Grammar_Symbol;

	Last : Item_Array_Index;	-- The last item in closure_items.

	Kernel : Item_Array_Pointer;

	Did_Goto :
	    array(First_Symbol(Nonterminal)..Last_Symbol(Terminal)) of
		Boolean;    -- did_goto(sym) = True if computed goto for sym.

    begin
	Current_State := 0;

    Generate_States:
	loop

--& VADS version 4.06 corrupts memory on the following statement
--&	    did_goto := (others => false);
--& Therefore, we do it the hard way.
	    for S in Did_Goto'range loop
		Did_Goto(S) := False;
	    end loop;
--& End bug hack.


	    Make_Null(Nt_Trans_Set);
	    Make_Null(T_Trans_Set);

	    Get_Closure(Current_State, Closure_Items, Last);

	    -- generate goto's for current state --

	    -- This is somewhat hacked but...
	    -- For simplicity, the kernel Items are appended to
	    -- the end of the CLOSURE_ITEMS array so that the
	    -- reflexive transitive closure of the state is in closure_items.
	    -- (GET_CLOSURE only puts the transitive closure 
	    --  into closure_items).
	    -- This assumes that CLOSURE_ITEMS is large enough to hold the
	    -- closure + kernel. This assumtion should hold for all
	    -- but contrived grammars (I hope).

	    Kernel := State_Info(Current_State).Kernel;
	    for Item_Index in Kernel.all'range loop
		Last := Last + 1;
		Closure_Items(Last) := Kernel(Item_Index);
	    end loop;

	    for Item_Index in 1..Last loop

		I := Closure_Items(Item_Index);
		if I.Dot_Position  < Length_of(I.Rule_ID) then

		    Sym := Get_RHS(I.Rule_ID, I.Dot_Position + 1);

		    -- generate goto on SYM if not done yet.
		    if not Did_Goto(Sym) then
			Did_Goto(Sym) := True;

			-- get the items in goto of sym
			Make_Null(Goto_Set);
			I.Dot_Position := I.Dot_Position + 1;
			Insert(I, Into => Goto_Set);
			for J in Item_Index+1..Last loop
			    I := Closure_Items(J);
			    if I.Dot_Position < Length_of(I.Rule_ID) then
				I.Dot_Position := I.Dot_Position + 1;
				if Get_RHS(I.Rule_ID, I.Dot_Position) = Sym
				then
				    Insert(I, Into => Goto_Set);
				end if;
			    end if;
			end loop;
			Goto_State := Find_State(Goto_Set, Sym);
			Make_Null(Goto_Set);
			if Is_Nonterminal(Sym) then
			    Nt_Trans := (Symbol => Sym, State_ID => Goto_State);
			    Insert(Nt_Trans, Into => Nt_Trans_Set);
			else -- terminal transition
			    T_Trans := (Symbol => Sym, State_ID => Goto_State);
			    Insert(T_Trans, Into => T_Trans_Set);
			end if;
			Insert(Current_State, 
			       Into => State_Info(Goto_State).Preds);
		    end if;
		end if;

	    end loop;

	    -- at this point, all the goto's for the current
	    -- state have been generated.

	    State_Info(Current_State).Nonterminal_Goto :=
		new Transition_Array(1..Size_of(Nt_Trans_Set));

	    Gotos := State_Info(Current_State).Nonterminal_Goto;
	    Initialize(Nt_Trans_Iter, Nt_Trans_Set);
	    for S in 1..Size_of(Nt_Trans_Set) loop
		Next(Nt_Trans_Iter, Nt_Trans);
		Gotos(S) := Nt_Trans;
	    end loop;


	    State_Info_2(Current_State).Terminal_Goto :=
		new Transition_Array(1..Size_of(T_Trans_Set));

	    Gotos := State_Info_2(Current_State).Terminal_Goto;
	    Initialize(T_Trans_Iter, T_Trans_Set);
	    for S in 1..Size_of(T_Trans_Set) loop
		Next(T_Trans_Iter, T_Trans);
		Gotos(S) := T_Trans;
	    end loop;

	    Make_Null(Nt_Trans_Set);
	    Make_Null(T_Trans_Set);

	    Current_State := Current_State + 1;
	    exit Generate_States when Current_State > Last_State;
	     
	end loop Generate_States;

    end Make_LR0_States;



    -- This procedure allocates the arrays for computing the LR(0) states.
    -- The number of states is not known at this point, so it is
    -- estimated using a formula taken from
    --
    --	Purdom, P. W.: "The Size of LALR(1) Parsers," BIT, VOL. 14,
    --                     No. 3, July 1974, pp.326-337
    --
    -- The formula is
    --	Number of states = 0.5949 * C + 0.02
    --
    -- where C is the number of rules plus the total number of symbols on
    -- the right hand side. We round this figures a little...

    procedure LR0_Initialize is
	C : Integer := 0;
	First_Item: constant Item := (Rule_ID => First_Rule, Dot_Position => 0);
    begin

	-- estimate the number of states --

	for R in First_Rule..Last_Rule loop
	    C := C + 1 + Length_of(R);
	end loop;
	Estimated_Number_of_States := 
	    Integer(0.6 * Float(C) + 1.0);


	-- Increase the estimate by 25% just in case --

	Max_State := 2 + Parse_State(1.25 * Float(Estimated_Number_of_States));
	if Max_State < 100 then
	    Max_State := 100;
	end if;

	-- Initialize the state array --

        State_Info := new State_Array(0..Max_State);

	State_Info(First_State).Kernel    := new Item_Array(1..1);
	State_Info(First_State).Kernel(1) := First_Item;
	Make_Null(State_Info(First_State).Preds);

	--& Hack state_info_2
	State_Info_2 := new State_Array_2(0..Max_State);

	Last_State := 0;

	--  set up the goto arrays   --

	Terminal_Gotos := new Goto_Array
	    (First_Symbol(Terminal)..Last_Symbol(Terminal));
	Nonterminal_Gotos := new Goto_Array
	    (First_Symbol(Nonterminal)..Last_Symbol(Nonterminal));
	Overflow_Gotos := new Overflow_Array(First_State..Max_State);


	--  Initialize them to the null_parse_state  --

--& more Verdix BUGS
--&	terminal_gotos.all     := 
--&       (terminal_gotos.all'range => null_parse_state);
--&
--&     nonterminal_gotos.all  := 
--&       (nonterminal_gotos.all'range => null_parse_state);

--& Start Verdix bug fix
	for I in Terminal_Gotos.all'range loop
	    Terminal_Gotos(I) := Null_Parse_State;
	end loop;
	for I in Nonterminal_Gotos.all'range loop
	    Nonterminal_Gotos(I) := Null_Parse_State;
	end loop;
--& End Verdix Bug fix

	-- initialize closure_items to the size of the maximum closure
	-- i. e. the number of rules in the grammar.
	-- Hack: The size is increased by 15 to hold any kernel that
	--       might be appended to closure_items. It is hoped
	--       that no kernel is greater that 15. If a kernel is greater
	--       than 15, it is hoped that #rules+15 > transitive closure
	--       of any state.
	Closure_Items := new Item_Array
		(1..Item_Array_Index(Last_Rule-First_Rule+1+15));

	Examined_Symbols := new Boolean_Array
		 (First_Symbol(Nonterminal)..Last_Symbol(Nonterminal));
	Make_LR0_States;


    end LR0_Initialize;


    function First_Parse_State return Parse_State is
    begin
	return First_State; -- a constant;
    end First_Parse_State;

    function Last_Parse_State  return Parse_State is
    begin
	return Last_State;
    end Last_Parse_State;



    procedure Get_Closure
	(State_ID       : in  Parse_State; 
	 Closure_Items  : in  Item_Array_Pointer;
	 Last           : out Item_Array_Index) is
    ---
	use Symbol_Info;


	Sym	      : Grammar_Symbol;
	-- Iterator      : Item_Iterator;
	Temp_Item     : Item;
	New_Item      : Item;
	-- Index         : Yield_Index;
	-- Last_Index    : Yield_Index;
	Closure_Index : Item_Array_Index;  -- for looping thru Closure_Items
	Next_Free     : Item_Array_Index;  -- next free in closure_items
	Kernel_Ptr    : Item_Array_Pointer;  -- points to kernel
    begin
--	examined_symbols := (others => false);
	for I in Examined_Symbols.all'range loop
	    Examined_Symbols(I) := False;
	end loop;

	New_Item.Dot_Position := 0;	-- Used to add closure items.
	Next_Free := 1;

	-- first add all the items directly derivable from kernel to closure.
	Kernel_Ptr := State_Info(State_ID).Kernel;
	for T in Kernel_Ptr.all'range loop
	    Temp_Item := Kernel_Ptr(T);
	    if Temp_Item.Dot_Position < Length_of(Temp_Item.Rule_ID) then
		Sym := Get_RHS(Temp_Item.Rule_ID, Temp_Item.Dot_Position+1);
		if Is_Nonterminal(Sym) and then not Examined_Symbols(Sym) then
		    Examined_Symbols(Sym) := True;
		    for I in Nonterminal_Yield_Index(Sym)..
			     Nonterminal_Yield_Index(Sym+1) - 1
		    loop
			New_Item.Rule_ID := Nonterminal_Yield(I);
			Closure_Items(Next_Free) := New_Item;
			Next_Free := Next_Free + 1;
		    end loop;
		end if;
	    end if;
	end loop;

	-- Now compute the closure of the items in Closure_Items.
	Closure_Index := 1;
	while Closure_Index < Next_Free loop
	    Temp_Item := Closure_Items(Closure_Index);
	    if Temp_Item.Dot_Position < Length_of(Temp_Item.Rule_ID) then
		Sym := Get_RHS(Temp_Item.Rule_ID, Temp_Item.Dot_Position+1);
		if Is_Nonterminal(Sym) and then not Examined_Symbols(Sym) then
		    Examined_Symbols(Sym) := True;
		    for I in Nonterminal_Yield_Index(Sym)..
			     Nonterminal_Yield_Index(Sym+1) - 1
		    loop
			New_Item.Rule_ID := Nonterminal_Yield(I);
			Closure_Items(Next_Free) := New_Item;
			Next_Free := Next_Free + 1;
		    end loop;
		end if;
	    end if;
	    Closure_Index := Closure_Index + 1;
	end loop;
	Last := Next_Free - 1;

    end Get_Closure;

    procedure Closure(Set_1: in out Item_Set) is
	use Symbol_Info;

	Next_Free     : Item_Array_Index; -- index of next free in Closure_Items
	Sym	      : Grammar_Symbol;
	Iterator      : Item_Iterator;
	Temp_Item     : Item;
	New_Item      : Item;
	-- Index         : Yield_Index;
	-- Last_Index    : Yield_Index;
	Closure_Index : Item_Array_Index;  -- for looping thru Closure_Items

    begin
	Next_Free := 1;

--	examined_symbols := (others => false);
	for I in Examined_Symbols.all'range loop
	    Examined_Symbols(I) := False;
	end loop;

	New_Item.Dot_Position := 0;	-- Used to add closure items.

	-- first add all the items directly derivable from set_1 to closure.
	Initialize(Iterator, Set_1);
	while More(Iterator) loop
	    Next(Iterator, Temp_Item);
	    if Temp_Item.Dot_Position < Length_of(Temp_Item.Rule_ID) then
		Sym := Get_RHS(Temp_Item.Rule_ID, Temp_Item.Dot_Position+1);
		if Is_Nonterminal(Sym) and then not Examined_Symbols(Sym) then
		    Examined_Symbols(Sym) := True;
		    for I in Nonterminal_Yield_Index(Sym)..
			     Nonterminal_Yield_Index(Sym+1) - 1
		    loop
			New_Item.Rule_ID := Nonterminal_Yield(I);
			Closure_Items(Next_Free) := New_Item;
			Next_Free := Next_Free + 1;
		    end loop;
		end if;
	    end if;
	end loop;

	-- Now comput the closure of the items in Closure_Items.
	Closure_Index := 1;
	while Closure_Index < Next_Free loop
	    Temp_Item := Closure_Items(Closure_Index);
	    if Temp_Item.Dot_Position < Length_of(Temp_Item.Rule_ID) then
		Sym := Get_RHS(Temp_Item.Rule_ID, Temp_Item.Dot_Position+1);
		if Is_Nonterminal(Sym) and then not Examined_Symbols(Sym) then
		    Examined_Symbols(Sym) := True;
		    for I in Nonterminal_Yield_Index(Sym)..
			     Nonterminal_Yield_Index(Sym+1) - 1
		    loop
			New_Item.Rule_ID := Nonterminal_Yield(I);
			Closure_Items(Next_Free) := New_Item;
			Next_Free := Next_Free + 1;
		    end loop;
		end if;
	    end if;
	    Closure_Index := Closure_Index + 1;
	end loop;

	-- Now add all the closure items to set_1.
	for I in 1..Next_Free-1 loop
	    Insert(Closure_Items(I), Into => Set_1);
	end loop;

    end Closure;



    function Find_State(Kernel_Set : Item_Set;
			Trans_Sym  : Grammar_Symbol) return Parse_State is
	Last      : constant Item_Array_Index := 
		     Item_Array_Index(Size_of(Kernel_Set));
	-- Temp_Item : Item;
	Iterator  : Item_Set_Pack.Set_Iterator;
	Kernel    : Item_Array(1..Last);
	S         : Parse_State;

    begin

	if Last = 0 then
	    Put_Line("Ayacc: Possible Error in Find_State.");
	    return Null_Parse_State;
	end if;

	-- Copy kernel_set into the array KERNEL to compare it
	-- to exiting states.

	Initialize(Iterator, Kernel_Set);
	for I in 1..Last loop		    -- last = # of items in kernel_set
	    Next(Iterator, Kernel(I));
	end loop;

	-- Look for state in existing states  --

	if Is_Terminal(Trans_Sym) then
	    S := Terminal_Gotos(Trans_Sym);
	else
	    S := Nonterminal_Gotos(Trans_Sym);
	end if;

	while S /= Null_Parse_State loop

	    -- Uncomment the following 3 lines of code when you
	    -- you use a bug free compiler.
	    --&if kernel = state_info(S).kernel.all then
		--&return S;
	    --&end if;

	    -- The following code is to fix a bug in the Verdix compiler
	    -- remove it when you use a bug free compiler.
	    if Kernel'Last /= State_Info(S).Kernel.all'Last then
		goto Continue;
	    end if;
	    for J in Kernel'range loop
		if Kernel(J).Rule_ID /= State_Info(S).Kernel(J).Rule_ID
		or else
		   Kernel(J).Dot_Position /= 
		   State_Info(S).Kernel(J).Dot_Position
		then
		    goto Continue;
		end if;
	    end loop;
	    return S;
	    -- The end of verdix compiler bug fix.

	<<Continue>>
	    S := Overflow_Gotos(S);
	end loop;

	-- Didn't find state, create a new state. --

	Last_State := Last_State + 1;
	State_Info(Last_State).Kernel      := new Item_Array(1..Last);
	State_Info(Last_State).Kernel.all  := Kernel;
	Make_Null(State_Info(Last_State).Preds);

	-- Save state number in list of transitions on symbol Trans_Sym.
	if Is_Terminal(Trans_Sym) then
	    Overflow_Gotos(Last_State) := Terminal_Gotos(Trans_Sym);
	    Terminal_Gotos(Trans_Sym)  := Last_State;
	else
	    Overflow_Gotos(Last_State) := Nonterminal_Gotos(Trans_Sym);
	    Nonterminal_Gotos(Trans_Sym) := Last_State;
	end if;

	return Last_State;
    end Find_State;


    function Get_Goto(State_ID : Parse_State;
		      Sym      : Grammar_Symbol) return Parse_State is
	Gotos : Transition_Array_Pointer;
    begin
	Gotos := State_Info(State_ID).Nonterminal_Goto;
	for S in Gotos.all'range loop
	    if Sym = Gotos(S).Symbol then
		return Gotos(S).State_ID;
	    end if;
	end loop;
	Put_Line("Ayacc: Possible Error in Get_Goto.");
	return Null_Parse_State;
    end Get_Goto;
		


    procedure Get_Pred_Set
	(State_ID : in Parse_State;
	 I        : in Item;
	 Pred_Set : in out Parse_State_Set) is
    ----
	New_Item     : Item;
	Temp_Item    : Item;
	Iterator     : Parse_State_Iterator;
	Pred_State   : Parse_State;
    begin
	if I.Dot_Position = 0 then
	    Insert(State_ID, Into => Pred_Set);
	else
	    Temp_Item := State_Info(State_ID).Kernel(1);
	    if Get_RHS(Temp_Item.Rule_ID, Temp_Item.Dot_Position) =
	       Get_RHS(I.Rule_ID, I.Dot_Position)
	    then
		New_Item := (I.Rule_ID, I.Dot_Position - 1);
		Initialize(Iterator, State_Info(State_ID).Preds);
		while More(Iterator) loop
		    Next(Iterator, Pred_State);
		    Get_Pred_Set(Pred_State, New_Item, Pred_Set);
		end loop;
	    end if;
	end if;
    end Get_Pred_Set;


    procedure Get_Transitions
	(State_ID : in     Parse_State;
	 Kind     : in     Transition_Type;
	 Set_1    : in out Transition_Set) is
    
	Gotos	       : Transition_Array_Pointer;

    begin

	Make_Null(Set_1);

	if Kind = Terminals or else Kind = Grammar_Symbols then
	    Gotos := State_Info_2(State_ID).Terminal_Goto;
	    for S in reverse Gotos.all'range loop
		Insert(Gotos(S), Into => Set_1);
	    end loop;
	end if;

	if Kind /= Terminals then
	    Gotos := State_Info(State_ID).Nonterminal_Goto;
	    for S in reverse Gotos.all'range loop
		Insert(Gotos(S), Into => Set_1);
	    end loop;
	    return;
	end if;

    end Get_Transitions;




    procedure Get_Transition_Symbols
	(State_ID : in     Parse_State;
	 Kind     : in     Transition_Type;
	 Set_1    : in out Grammar_Symbol_Set) is

	Gotos       : Transition_Array_Pointer;
    
    begin

	Make_Null(Set_1);

	if Kind = Terminals or else Kind = Grammar_Symbols then
	    Gotos := State_Info_2(State_ID).Terminal_Goto;
	    for S in reverse Gotos.all'range loop
		Insert(Gotos(S).Symbol, Into => Set_1);
	    end loop;
	end if;

	if Kind = Terminals then return; end if;

	if Kind = Nonterminals or else Kind = Grammar_Symbols then
	    Gotos := State_Info(State_ID).Nonterminal_Goto;
	    for S in reverse Gotos.all'range loop
		Insert(Gotos(S).Symbol, Into => Set_1);
	    end loop;
	end if;

    end Get_Transition_Symbols;



    procedure Get_Kernel
	(State_ID : in     Parse_State;
	 Set_1    : in out Item_Set) is
    begin
	Make_Null(Set_1);
	for I in State_Info(State_ID).Kernel.all'range loop
	    Insert(State_Info(State_ID).Kernel(I), Into => Set_1);
	end loop;
    end Get_Kernel;



    procedure Initialize(Iterator : in out Kernel_Iterator;
			 State_ID : in Parse_State) is
    begin
	if State_ID in First_State..Last_State then
	    Iterator.Kernel   := State_Info(State_ID).Kernel;
	    Iterator.Curser   := 1;
	else
	    raise State_Out_of_Bounds;
	end if;
    end Initialize;

    function More(Iterator: Kernel_Iterator) return Boolean is
    begin
	return Iterator.Curser <= Iterator.Kernel.all'Last;
    end More;

    procedure Next(Iterator: in out Kernel_Iterator; I : out Item) is
    begin
	I := Iterator.Kernel(Iterator.Curser);
	Iterator.Curser := Iterator.Curser + 1;
    end Next;


    procedure Initialize
	(Iterator : in out Nt_Transition_Iterator;
	 State_ID : in Parse_State) is
    begin
	if State_ID in First_State..Last_State then
	    Iterator.Nonterm_Trans := State_Info(State_ID).Nonterminal_Goto;
	    Iterator.Curser := 1;
	else
	    raise State_Out_of_Bounds;
	end if;
    end Initialize;

    function More(Iterator : Nt_Transition_Iterator) return Boolean is
    begin
	return Iterator.Curser <= Iterator.Nonterm_Trans.all'Last;
    end More;


    procedure Next
	(Iterator: in out Nt_Transition_Iterator;
	 Trans : out Transition) is
    begin
	Trans := Iterator.Nonterm_Trans(Iterator.Curser);
	Iterator.Curser := Iterator.Curser + 1;
    end Next;

-----------------------------------------------------

-- terminal interator stuff.

    procedure Initialize
	(Iterator : in out T_Transition_Iterator;
	 State_ID : in Parse_State) is
    begin
	if State_ID in First_State..Last_State then
	    Iterator.Term_Trans := State_Info_2(State_ID).Terminal_Goto;
	    Iterator.Curser := 1;
	else
	    raise State_Out_of_Bounds;
	end if;
    end Initialize;

    function More(Iterator : T_Transition_Iterator) return Boolean is
    begin
	return Iterator.Curser <= Iterator.Term_Trans.all'Last;
    end More;


    procedure Next
	(Iterator: in out T_Transition_Iterator;
	 Trans : out Transition) is
    begin
	Trans := Iterator.Term_Trans(Iterator.Curser);
	Iterator.Curser := Iterator.Curser + 1;
    end Next;

end LR0_Machine;
