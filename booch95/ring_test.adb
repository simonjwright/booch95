-- Copyright (C) 1994-2000 Grady Booch and Simon Wright.
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
with Ring_Test_Support;

procedure Ring_Test is

  use Ada.Text_IO;
  use Ring_Test_Support;
  use Containers;
  use Rings;

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

  procedure Test_Active_Iterator (R : Container'Class) is
    Iter : Iterator'Class := New_Iterator (R);
    Success : Boolean;
    Temp : Character;
  begin
    while not Is_Done (Iter) loop
      Temp := Current_Item (Iter);
      Process (Temp, Success);
      Next (Iter);
    end loop;
  end Test_Active_Iterator;

  procedure Test_Passive_Iterator (R : Container'Class) is
    procedure Iterate is new Visit (Apply => Process);
    Iter : Iterator'Class := New_Iterator (R);
  begin
    Iterate (Using => Iter);
  end Test_Passive_Iterator;

  procedure Test_Primitive (R1, R2 : in out Ring'Class) is
  begin
    Assertion (Is_Empty (R1), "** P01: Ring is not initially empty");
    Assertion (Extent (R1) = 0, "** P02: Ring extent is not initially zero");
    Insert (R1, '1');
    Insert (R1, '2');
    Insert (R1, '3');
    Assertion (not Is_Empty (R1), "** P03: Ring is empty");
    Assertion (Extent(R1) = 3, "** P04: Ring extent is not correct");
    Assertion (Top (R1) = '3', "** P05: Ring top is not correct");
    Clear (R1);
    Assertion (Is_Empty (R1), "** P06: Ring is not empty");
    Assertion (Extent (R1) = 0, "** P07: Ring extent is not zero");
    Insert (R1, '4');
    Insert (R1, '5');
    Insert (R1, '6');
    Assertion (not Is_Empty (R1), "** P08: Ring is empty");
    Assertion (Extent(R1) = 3, "** P09: Ring extent is not correct");
    Assertion (Top (R1) = '6', "** P10: Ring top is not correct");
    Pop (R1);
    Pop (R1);
    Assertion (not Is_Empty (R1), "** P11: Ring is empty");
    Assertion (Extent(R1) = 1, "** P12: Ring extent is not correct");
    Assertion (Top (R1) = '4', "** P13: Ring top is not correct");
    Pop (R1);
    Assertion (Is_Empty (R1), "** P14: Ring is not empty");
    Assertion (Extent (R1) = 0, "** P15: Ring extent is not zero");
    Insert (R1, '7');
    Insert (R1, '8');
    Insert (R1, '9');
    Assertion (not Is_Empty (R1), "** P16: Ring is empty");
    Assertion (Extent(R1) = 3, "** P17: Ring extent is not correct");
    Assertion (Top (R1) = '9', "** P13: Ring top is not correct");
    R2 := R1;
    Assertion (not Is_Empty (R1), "** P19: Ring is empty");
    Assertion (Extent(R1) = 3, "** P20: Ring extent is not correct");
    Assertion (Top (R1) = '9', "** P21: Ring top is not correct");
    Assertion (not Is_Empty (R2), "** P22: Ring is empty");
    Assertion (Extent(R2) = 3, "** P23: Ring extent is not correct");
    Assertion (Top (R2) = '9', "** P24: Ring top is not correct");
    Assertion (Are_Equal (R1, R2), "** P25a: Rings are not equal");
    Assertion (R1 = R2, "** P25b: Rings are not equal");
    Rotate (R2);
    Assertion (Top (R2) = '8', "** P26: Ring top is not correct");
    Rotate (R2);
    Assertion (Top (R2) = '7', "** P27: Ring top is not correct");
    Rotate (R2, Forward);
    Assertion (Top (R2) = '9', "** P28: Ring top is not correct");
    Rotate (R2, Backward);
    Assertion (Top (R2) = '7', "** P29: Ring top is not correct");
    Rotate (R2, Backward);
    Assertion (Top (R2) = '8', "** P30: Ring top is not correct");
    Assertion (not At_Mark (R2), "** P31: Ring at mark");
    Mark (R2);
    Assertion (At_Mark (R2), "** P32: Ring not at mark");
    Rotate (R2, Backward);
    Assertion (Top (R2) = '9', "** P33: Ring top is not correct");
    Assertion (not At_Mark (R2), "** P34: Ring at mark");
    Rotate_To_Mark (R2);
    Assertion (At_Mark (R2), "** P35: Ring not at mark");
    Assertion (Top (R2) = '8', "** P36: Ring top is not correct");
    Assertion (not Are_Equal (R1, R2), "** P37a: Rings are equal");
    Assertion (R1 /= R2, "** P37b: Rings are equal");
    Pop (R2);
    Assertion (Extent (R2) = 2, "** P38: Ring extent is not correct");
    Assertion (Top (R2) = '7', "** P39: Ring top is not correct");
    Assertion (At_Mark (R2), "** P40: Ring at mark");
  end Test_Primitive;

--   Ring_B_P1, Ring_B_P2 : RB.Bounded_Ring;
--   Ring_D_P1, Ring_D_P2 : RD.Dynamic_Ring;
  Ring_U_P1, Ring_U_P2 : Unbounded_Ring;
  Ring_UG_P1, Ring_UG_P2 : Guarded_Unbounded_Ring;
  Ring_US_P1, Ring_US_P2 : Synchronized_Unbounded_Ring;

begin

  Put_Line ("Starting Ring tests");

--   Put_Line ("...Bounded Ring");
--   Test_Primitive (Ring_B_P1, Ring_B_P2);

--   Put_Line ("...Dynamic Ring");
--   Preallocate (Ring_D_P1, 50);
--   Test_Primitive (Ring_D_P1, Ring_D_P2);

  Put_Line ("...Unbounded Ring");
  Test_Primitive (Ring_U_P1, Ring_U_P2);
  Put_Line ("...Unbounded Guarded Ring");
  Test_Primitive (Ring_UG_P1, Ring_UG_P2);
  Put_Line ("...Unbounded Synchronized Ring");
  Test_Primitive (Ring_US_P1, Ring_US_P2);

  Put_Line ("...Ring Active Iterator");
--   Put_Line ("   Bounded:");
--   Test_Active_Iterator (Ring_B_P1);
--   Put_Line ("   Dynamic:");
--   Test_Active_Iterator (Ring_D_P1);
  Put_Line ("   Unbounded:");
  Test_Active_Iterator (Ring_U_P1);
  Put_Line ("   Unbounded Guarded:");
  Test_Active_Iterator (Ring_UG_P1);
  Put_Line ("   Unbounded Synchronized:");
  Test_Active_Iterator (Ring_US_P1);

  Put_Line ("...Ring Passive Iterator");
--   Put_Line ("   Bounded:");
--   Test_Passive_Iterator (Ring_B_P1);
--   Put_Line ("   Dynamic:");
--   Test_Passive_Iterator (Ring_D_P1);
  Put_Line ("   Unbounded:");
  Test_Passive_Iterator (Ring_U_P1);
  Put_Line ("   Unbounded Guarded:");
  Test_Passive_Iterator (Ring_UG_P1);
  Put_Line ("   Unbounded Synchronized:");
  Test_Passive_Iterator (Ring_US_P1);

  Put_Line ("Completed Ring tests");

end Ring_Test;
