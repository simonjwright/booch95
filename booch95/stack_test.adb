-- Copyright (C) 1994-1998 Grady Booch, David Weller and Simon Wright.
-- All Rights Reserved.
--
--      This program is free software; you can redistribute it
--      and/or modify it under the terms of the Ada Community
--      License which comes with this Library.
--
--      This program is distributed in the hope that it will be
--      useful, but WITHOUT ANY WARRANTY; without even the implied
--      warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--      PURPOSE. See the Ada Community License for more details.
--      You should have received a copy of the Ada Community
--      License with this library, in the file named "Ada Community
--      License" or "ACL". If not, contact the author of this library
--      for a copy.
--

-- $Id$

with Ada.Text_IO;
with Stack_Test_Support;
procedure Stack_Test is
  use Ada.Text_IO;
  use Stack_Test_Support;
  use Containers;
  use Stacks;
  use SB;
  use SD;
  use SU;

  --    Global_Items : array(1..10) of aliased Chunk;

  procedure Process (C : Character; OK : out Boolean) is
  begin
    Put_Line ("Item: " & C);
    Ok := True;
  end Process;

  procedure Assertion (Cond : Boolean; Message : String) is
  begin
    if not Cond then
      Put_Line (Message);
    end if;
  end Assertion;
  pragma Inline (Assertion);

  procedure Test_Active_Iterator (L : Container'Class) is
    Iter : Iterator := New_Iterator (L);
    Success : Boolean;
    Temp : Character;
  begin
    while not Is_Done (Iter) loop
      Temp := Current_Item (Iter);
      Process (Temp, Success);
      Next (Iter);
    end loop;
  end Test_Active_Iterator;

  procedure Test_Passive_Iterator (L : Container'Class) is
    procedure Iterate is new Visit
       (Apply => Process, Over_The_Container => L);
  begin
    Iterate;
  end Test_Passive_Iterator;

  procedure Test_Primitive (S1, S2 : in out Stack'Class) is
  begin
    for I in Character'('a') .. Character'('z') loop
      Push (S1, I);
    end loop;
    Clear (S1);
    Assertion (Is_Empty (S1), "** P01: Stack is not initially empty");
    Assertion (Depth (S1) = 0, "** P02: Stack depth is not initially zero");
    Push (S1, '1');
    Push (S1, '2');
    Push (S1, '3');
    Assertion (not (Is_Empty (S1)), "** P03: Stack is empty");
    Assertion ((Depth (S1) = 3), "** P04: Stack depth is not correct");
    Assertion ((Top (S1) = '3'), "** P05: Stack top is not correct");
    Clear (S1);
    Assertion (Is_Empty (S1), "** P06: Stack is not empty");
    Assertion ((Depth (S1) = 0), "** P07: Stack depth is not zero");
    Push (S1, '4');
    Push (S1, '5');
    Push (S1, '6');
    Assertion (not (Is_Empty (S1)), "** P08: Stack is empty");
    Assertion ((Depth (S1) = 3), "** P09: Stack depth is not correct");
    Assertion ((Top (S1) = '6'), "** P10: Stack top is not correct");
    Pop (S1);
    Pop (S1);
    Assertion (not (Is_Empty (S1)), "** P11: Stack is empty");
    Assertion ((Depth (S1) = 1), "** P12: Stack depth is not correct");
    Assertion ((Top (S1) = '4'), "** P13: Stack top is not correct");
    Pop (S1);
    Assertion (Is_Empty (S1), "** P14: Stack is not empty");
    Assertion ((Depth (S1) = 0), "** P15: Stack depth is not zero");
    Push (S1, '7');
    Push (S1, '8');
    Push (S1, '9');
    Pop (S1);
    Pop (S1);
    Assertion (not (Is_Empty (S1)), "** P16: Stack is empty");
    Assertion ((Depth (S1) = 1), "** P17: Stack depth is not correct");
    Assertion ((Top (S1) = '7'), "** P18: Stack top is not correct");
    S2:= S1;
    Assertion (not (Is_Empty (S1)), "** P19: Stack is empty");
    Assertion ((Depth (S1) = 1), "** P20: Stack depth is not correct");
    Assertion ((Top (S1) = '7'), "** P21: Stack top is not correct");
    Assertion (not (Is_Empty (S2)), "** P22: Stack is empty");
    Assertion ((Depth (S2) = 1), "** P23: Stack depth is not correct");
    Assertion ((Top (S2) = '7'), "** P24: Stack top is not correct");
    Assertion (Are_Equal (S1, S2), "** P25: Stacks are not equal");
    Clear (S2);
    Assertion (not (Is_Empty (S1)) , "** P26: Stack is empty");
    Assertion (Is_Empty (S2), "** P29: Stack is not empty");
    Assertion ((Depth (S1) = 1), "** P27: Stack depth is not correct");
    Assertion ((Top (S1) = '7'), "** P28: Stack top is not correct");
    Assertion ((Depth (S2) = 0), "** P30: Stack depth is not correct");
    Assertion (not Are_Equal (S1, S2), "** P31: Stacks not equal");
  end Test_Primitive;

  --    procedure Test_User_Defined(S1, S2 : in out Stack'Class) is
  --    begin
  --       Assertion (Is_Empty (S1), "** P01: Stack is not initially empty");
  --       Assertion (Depth (S1) = 0, "** P02: Stack depth is not initially zero");
  --       Push (S1, Global_Items(1));
  --       Push (S1, Global_Items(2));
  --       Push (S1, Global_Items(3));
  --       Assertion (not (Is_Empty (S1)), "** P03: Stack is empty");
  --       Assertion ((Depth (S1) = 3), "** P04: Stack depth is not correct");
  --       Assertion ((Top (S1) = Global_Items(1)), "** P05: Stack top is not correct");
  --       Clear (S1);
  --       Assertion (Is_Empty (S1), "** P06: Stack is not empty");
  --       Assertion ((Depth (S1) = 0), "** P07: Stack depth is not zero");
  --       Push (S1, Global_Items(4));
  --       Push (S1, Global_Items(5));
  --       Push (S1, Global_Items(6));
  --       Assertion (not (Is_Empty (S1)), "** P08: Stack is empty");
  --       Assertion ((Depth (S1) = 3), "** P09: Stack depth is not correct");
  --       Assertion ((Top (S1) = Global_Items(6)), "** P10: Stack top is not correct");
  --       Pop (S1);
  --       Pop (S1);
  --       Assertion (not (Is_Empty (S1)), "** P11: Stack is empty");
  --       Assertion ((Depth (S1) = 1), "** P12: Stack depth is not correct");
  --       Assertion ((Top (S1) = Global_Items(4)), "** P13: Stack top is not correct");
  --       Pop (S1);
  --       Assertion (Is_Empty (S1), "** P14: Stack is not empty");
  --       Assertion ((Depth (S1) = 0), "** P15: Stack depth is not zero");
  --       Push (S1, Global_Items(7));
  --       Push (S1, Global_Items(8));
  --       Push (S1, Global_Items(9));
  --       Pop (S1);
  --       Pop (S1);
  --       Assertion (not (Is_Empty (S1)), "** P16: Stack is empty");
  --       Assertion ((Depth (S1) = 1), "** P17: Stack depth is not correct");
  --       Assertion ((Top (S1) = Global_Items(9)), "** P18: Stack top is not correct");
  --       S2:= S1;
  --       Assertion (not (Is_Empty (S1)), "** P19: Stack is empty");
  --       Assertion ((Depth (S1) = 1), "** P20: Stack depth is not correct");
  --       Assertion ((Top (S1) = Global_Items(7)), "** P21: Stack top is not correct");
  --       Assertion (not (Is_Empty (S2)), "** P22: Stack is empty");
  --       Assertion ((Depth (S2) = 1), "** P23: Stack depth is not correct");
  --       Assertion ((Top (S2) = Global_Items(7)), "** P24: Stack top is not correct");
  --       Assertion ((s1 = s2), "** P25: Stacks are not equal");
  --       Clear (S2);
  --       Assertion ( ( not(Is_Empty (S1)) ) , "** P26: Stack is empty");
  --       Assertion (Is_Empty (S2), "** P29: Stack is not empty");
  --       Assertion ((Depth (S1) = 1), "** P27: Stack depth is not correct");
  --       Assertion ((Top (S1) = Global_Items(7)), "** P28: Stack top is not correct");
  --       Assertion ((Depth (S2) = 0), "** P30: Stack depth is not correct");
  --       Assertion ((s1 /= s2), "** P31: Stacks not equal");
  --    end Test_User_Defined;

  Stack_B_P1, Stack_B_P2 : SB.Bounded_Stack;
  Stack_D_P1, Stack_D_P2 : SD.Dynamic_Stack;
  Stack_U_P1, Stack_U_P2 : SU.Unbounded_Stack;
  --    Stack_b_u1, Stack_b_u2 : aliased User_Bound_Stacks.Bounded_Stack;
  --    Stack_d_u1, Stack_d_u2 : aliased User_Dynamic_Stacks.Dyn_Stack;
  --    Stack_u_u1, Stack_U_u2 : aliased User_Unbounded_Stacks.Unb_Stack;

begin

  Put_Line ("Starting Stack tests");

  Put_Line ("...Bounded Stack");
  Test_Primitive (Stack_B_P1, Stack_B_P2);
  --  Test_User_defined(Stack_b_u1, Stack_b_u2);

  Put_Line ("...Dynamic Stack");
  Preallocate (Stack_D_P1, 50);
  Test_Primitive (Stack_D_P1, Stack_D_P2);
  --  Test_User_defined(Stack_d_u1, Stack_d_u2);

  Put_Line ("...Unbounded Stack");
  Test_Primitive (Stack_U_P1, Stack_U_P2);
  --  Test_User_defined(Stack_u_u1, Stack_u_u2);

  Put_Line ("...Stack Active Iterator");
  Put_Line ("   Bounded:");
  Test_Active_Iterator (Stack_B_P1);
  Put_Line ("   Dynamic:");
  Test_Active_Iterator (Stack_D_P1);
  Put_Line ("   Unbounded:");
  Test_Active_Iterator (Stack_U_P1);

  Put_Line ("...Stack Passive Iterator");
  Put_Line ("   Bounded:");
  Test_Passive_Iterator (Stack_B_P1);
  Put_Line ("   Dynamic:");
  Test_Passive_Iterator (Stack_D_P1);
  Put_Line ("   Unbounded:");
  Test_Passive_Iterator (Stack_U_P1);

  Assertion ((Top (Stack_B_P1) = '7'), "** M01: Stack top is not correct");
  Assertion ((Depth (Stack_B_P2) = 0), "** M02: Stack depth is not correct");
  --  Assertion ((Top (Stack_b_u1) = Global_Items(7)), "** M03: Stack top is not correct");
  --  Assertion ((Depth (Stack_b_u2) = 0), "** M04: Stack depth is not correct");

  Assertion ((Top (Stack_D_P1) = '7'), "** M05: Stack top is not correct");
  Assertion ((Depth (Stack_D_P2) = 0), "** M06: Stack depth is not correct");
  --  Assertion ((Top (Stack_d_u1) = Global_Items(7)), "** M07: Stack top is not correct");
  --  Assertion ((Depth (Stack_d_u2) = 0), "** M08: Stack depth is not correct");

  Assertion ((Top (Stack_U_P1) = '7'), "** M09: Stack top is not correct");
  Assertion ((Depth (Stack_U_P2) = 0), "** M10: Stack depth is not correct");
  --  Assertion ((Top (Stack_u_u1) = Global_Items(7)), "** M11: Stack top is not correct");
  --  Assertion ((Depth (Stack_u_u2) = 0), "** M12: Stack depth is not correct");

  Assertion (Available (Stack_B_P1) = 99 , "** M13: Available space not correct");
  Assertion (Available (Stack_B_P2) =100 , "** M14: Available space not correct");
  --  Assertion (Available (Stack_B_U1) = 99 , "** M15: Available space not correct");
  --  Assertion (Available (Stack_B_U1) =100 , "** M16: Available space not correct");

  Put_Line ("Completed Stack tests");

end Stack_Test;
