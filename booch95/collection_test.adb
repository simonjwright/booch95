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
with BC;
with Collection_Test_Support;

procedure Collection_Test is

  use Ada.Text_IO;
  use Collection_Test_Support;
  use Containers;
  use Collections;
--   use CB;
--   use CD;
  use CU;

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

  procedure Test_Active_Iterator (C : Container'Class) is
    Iter : Iterator'Class := New_Iterator (C);
    Success : Boolean;
    Temp : Character;
  begin
    while not Is_Done (Iter) loop
      Temp := Current_Item (Iter);
      Process (Temp, Success);
      Next (Iter);
    end loop;
  end Test_Active_Iterator;

  procedure Test_Passive_Iterator (C : Container'Class) is
    procedure Iterate is new Visit (Apply => Process);
    Iter : Iterator'Class := New_Iterator (C);
  begin
    Iterate (Using => Iter);
  end Test_Passive_Iterator;

  procedure Test_Iterator_Deletion (C : in out Collection'Class) is
    Iter : Iterator'Class := New_Iterator (C);
    Delete : Boolean;
  begin
    Clear (C);
    Append (C, '1');
    Append (C, '2');
    Append (C, '3');
    Append (C, '4');
    Append (C, '5');
    Append (C, '6');
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
      Assertion (False, "** IS01: Deletion succeeded");
    exception
      when BC.Not_Found => null;
      when others =>
        Assertion (False, "** IS02: Unexpected exception");
    end;
    Assertion (Length (C) = 3, "** IS03: Collection length is not correct");
    Assertion (First (C) = '1', "** IS04: Collection item is not correct");
    Remove (C, 1);
    Assertion (First (C) = '3', "** IS05: Collection item is not correct");
    Remove (C, 1);
    Assertion (First (C) = '5', "** IS06: Collection item is not correct");
    Remove (C, 1);
    Assertion (Length (C) = 0, "** IS07: Collection length is not zero");
  end Test_Iterator_Deletion;

  procedure Test_Primitive (C1, C2 : in out Collection'Class) is
  begin
    for C in Character'('a') .. Character'('z') loop
      Append (C1, C);
    end loop;
    Clear (C1);
    Assertion (Is_Empty (C1), "** P01: Collection is not initially empty");
    Assertion (Length (C1) = 0,
               "** P02: Collection Length is not initially zero");
    Insert (C1, '1');
    Insert (C1, '3', 1);
    Insert (C1, '2', 2);
    Assertion (not Is_Empty (C1), "** P03: Collection is empty");
    Assertion (Length (C1) = 3, "** P04: Collection Length is not correct");
    Assertion (First (C1) = '3', "** P05: Collection First is not correct");
    Assertion (Last (C1) = '1', "** P05: Collection Last is not correct");
    Assertion (Item_At (C1, 2) = '2', "** P07: Collection Item is not correct");
    Clear (C1);
    Append (C1, '6');
    Append (C1, '4');
    Append (C1, '5', 1);
    Assertion (not Is_Empty (C1), "** P08: Collection is empty");
    Assertion (Length (C1) = 3, "** P09: Collection Length is not correct");
    Assertion (First (C1) = '6', "** P10: Collection First is not correct");
    Assertion (Last (C1) = '4', "** P11: Collection Last is not correct");
    Remove (C1, 2);
    Remove (C1, 1);
    Assertion (Length (C1) = 1, "** P12: Collection Length is not correct");
    Assertion (First (C1) = '4', "** P13: Collection First is not correct");
    Remove (C1, 1);
    Assertion (Is_Empty (C1), "** P14: Collection is not empty");
    Assertion (Length (C1) = 0, "** P15: Collection Length is not zero");
    Insert (C1, '8');
    Append (C1, '7');
    Insert (C1, '9');
    Remove (C1, 2);
    Remove (C1, 1);
    Assertion (Item_At (C1, 1) = '7', "** P16: Collection Item is not correct");
    Assertion (First (C1) = '7', "** P17: Collection First is not correct");
    Assertion (Last (C1) = '7', "** P18: Collection Last is not correct");
    C2 := C1;
    Assertion (not Is_Empty (C1), "** P19: Collection is empty");
    Assertion (Length (C1) = 1, "** P20: Collection Length is not correct");
    Assertion (First (C1) = '7', "** P21: Collection First is not correct");
    Assertion (not Is_Empty (C2), "** P22: Collection is empty");
    Assertion (Length (C2) = 1, "** P23: Collection Length is not correct");
    Assertion (Last (C2) = '7', "** P24: Collection Last is not correct");
    Assertion (C1 = C2, "** P25: Collections are not equal");
    Clear (C2);
    Assertion (not Is_Empty (C1), "** P26: Collection is empty");
    Assertion (Length (C1) = 1, "** P27: Collection Length is not correct");
    Assertion (Item_At (C1, 1) = '7', "** P28: Collection Item is not correct");
    Assertion (Is_Empty (C2), "** P29: Collection is not empty");
    Assertion (Length (C2) = 0, "** P30: Collection Length is not correct");
    Assertion (C1 /= C2, "** P31: Collections not equal");
    for C in reverse Character'('a') .. Character'('z') loop
      Append (C2, C);
    end loop;
    C1 := C2;
    Assertion (C1 = C2, "** P32: Collections are not equal");
    Assertion (Location (C1, 'g') = 20,
               "** P33: Collection Location is not correct");
    Replace (C1, 20, 'A');
    Assertion (Item_At (C1, 20) = 'A', "** P34: Collection Item is not correct");
    Replace (C1, 20, 'g');
    Append (C1, 'A');
    Assertion (Item_At (C1, 20) = 'g',
               "** P35: Collection Item is not correct");
    Clear (C1);
    Clear (C2);
    Insert (C1, '7');
  end Test_Primitive;

--   Collection_B_P1, Collection_B_P2 : CB.Bounded_Collection;
--   Collection_D_P1, Collection_D_P2 : CD.Dynamic_Collection;
--   Collection_U_P1, Collection_U_P2 : CU.Unbounded_Collection;
  Collection_U_P1, Collection_U_P2 : CU.Unbounded_Collection;
--   Collection_U_P1, Collection_U_P2 : CU.Unbounded_Collection;

begin

  Put_Line ("Starting Collection tests");

--   Put_Line ("...Bounded Collection");
--   Test_Primitive (Collection_B_P1, Collection_B_P2);

--   Put_Line ("...Dynamic Collection");
--   Preallocate (Collection_D_P1, 50);
--   Test_Primitive (Collection_D_P1, Collection_D_P2);

  Put_Line ("...Unbounded Collection");
  Test_Primitive (Collection_U_P1, Collection_U_P2);

  Put_Line ("...Collection Active Iterator");
--   Put_Line ("   Bounded:");
--   Test_Active_Iterator (Collection_B_P1);
--   Put_Line ("   Dynamic:");
--   Test_Active_Iterator (Collection_D_P1);
  Put_Line ("   Unbounded:");
  Test_Active_Iterator (Collection_U_P1);

  Put_Line ("...Collection Passive Iterator");
--   Put_Line ("   Bounded:");
--   Test_Passive_Iterator (Collection_B_P1);
--   Put_Line ("   Dynamic:");
--   Test_Passive_Iterator (Collection_D_P1);
  Put_Line ("   Unbounded:");
  Test_Passive_Iterator (Collection_U_P1);

  Put_Line ("...Collection Iterator Deletion");
  Put_Line ("   Unbounded:");
  Test_Iterator_Deletion (Collection_U_P1);

  Put_Line ("Completed Collection tests");

end Collection_Test;
