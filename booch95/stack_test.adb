-- Copyright (C) 1994-2000 Grady Booch, David Weller and Simon Wright.
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
with BC;
with Stack_Test_Support;
procedure Stack_Test is
  use Ada.Text_IO;
  use Stack_Test_Support;
  use Containers;
  use Stacks;
  use SB;
  use SD;
  use SU;

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

  procedure Test_Active_Iterator (S : Container'Class) is
    Iter : Iterator'Class := New_Iterator (S);
    Success : Boolean;
    Temp : Character;
  begin
    while not Is_Done (Iter) loop
      Temp := Current_Item (Iter);
      Process (Temp, Success);
      Next (Iter);
    end loop;
  end Test_Active_Iterator;

  procedure Test_Passive_Iterator (S : Container'Class) is
    procedure Iterate is new Visit (Apply => Process);
    Iter : Iterator'Class := New_Iterator (S);
  begin
    Iterate (Using => Iter);
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
    S2 := S1;
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
    Push (S2, 'z');
    Push (S2, 'a');
    declare
      procedure P (Ch : in out Character) is
      begin
        Ch := Character'Succ (Ch);
      end P;
      procedure Acc is new Stacks.Process_Top (P);
    begin
      Acc (S2);
    end;
    Assertion (Depth (S2) = 2, "** P32: Stack depth is not correct");
    Assertion (Top (S2) = 'b', "** P33: Stack top is not correct");
    Pop (S2);
    Assertion (Top (S2) = 'z', "** P34: Stack top is not correct");
    Clear (S2);
    Push (S2, '7');
    Assertion (S1 = S2, "** P35: Stacks are not equal");
    Clear (S2);
  end Test_Primitive;

  procedure Test_Iterator_Deletion (S : in out Stack'Class) is
    Iter : Iterator'Class := New_Iterator (S);
    Delete : Boolean;
  begin
    Clear (S);
    Push (S, '1');
    Push (S, '2');
    Push (S, '3');
    Push (S, '4');
    Push (S, '5');
    Push (S, '6');
    Delete := False;
    Reset (Iter);
    while not Is_Done (Iter) loop
      if Delete then
        Delete_Item_At (Iter);
        Delete := False;
      else
        Next (Iter);
        Delete := True;
      end if;
    end loop;
    begin
      Delete_Item_At (Iter);
      Assertion (False, "** I01: Deletion succeeded");
    exception
      when BC.Not_Found => null;
      when others =>
        Assertion (False, "** I02: Unexpected exception");
    end;
    Assertion (Depth (S) = 3, "** I03: Stack length is not correct");
    Assertion (Top (S) = '6', "** I04: Stack item is not correct");
    Pop (S);
    Assertion (Top (S) = '4', "** I05: Stack item is not correct");
    Pop (S);
    Assertion (Top (S) = '2', "** I06: Stack item is not correct");
    Pop (S);
    Assertion (Depth (S) = 0, "** I07: Stack length is not zero");
  end Test_Iterator_Deletion;

  Stack_B_P1, Stack_B_P2 : SB.Bounded_Stack;
  Stack_D_P1, Stack_D_P2 : SD.Dynamic_Stack;
  Stack_U_P1, Stack_U_P2 : SU.Unbounded_Stack;

begin

  Put_Line ("Starting Stack tests");

  Put_Line ("...Bounded Stack");
  Test_Primitive (Stack_B_P1, Stack_B_P2);

  Put_Line ("...Dynamic Stack");
  Preallocate (Stack_D_P1, 50);
  Test_Primitive (Stack_D_P1, Stack_D_P2);

  Put_Line ("...Unbounded Stack");
  Test_Primitive (Stack_U_P1, Stack_U_P2);

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

  Assertion ((Top (Stack_D_P1) = '7'), "** M05: Stack top is not correct");
  Assertion ((Depth (Stack_D_P2) = 0), "** M06: Stack depth is not correct");

  Assertion ((Top (Stack_U_P1) = '7'), "** M09: Stack top is not correct");
  Assertion ((Depth (Stack_U_P2) = 0), "** M10: Stack depth is not correct");

  Assertion
     (Available (Stack_B_P1) = 99 , "** M13: Available space not correct");
  Assertion
     (Available (Stack_B_P2) = 100 , "** M14: Available space not correct");

  Put_Line ("...Stack Iterator Deletion");
  Put_Line ("   Bounded:");
  Test_Iterator_Deletion (Stack_B_P1);
  Put_Line ("   Dynamic:");
  Test_Iterator_Deletion (Stack_D_P1);
  Put_Line ("   Unbounded:");
  Test_Iterator_Deletion (Stack_U_P1);

  Put_Line ("Completed Stack tests");

end Stack_Test;
