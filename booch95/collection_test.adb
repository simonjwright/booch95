-- Copyright (C) 1994-1999 Grady Booch and Simon Wright.
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
with Collection_Test_Support;

procedure Collection_Test is

  use Ada.Text_IO;
  use Collection_Test_Support;
  use Containers;
  use Collections;
--   use SB;
--   use SD;
  use CU;

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

  procedure Test_Active_Iterator (C : Container'Class) is
    Iter : Iterator := New_Iterator (C);
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
    Iter : Iterator := New_Iterator (C);
  begin
    Iterate (Using => Iter);
  end Test_Passive_Iterator;

  procedure Test_Primitive (C1, C2 : in out Collection'Class) is
  begin
    --| for (char c = 'a'; (c <= 'z'); c++)
    --|   c1.Append(c);
    for C in Character'('a') .. Character'('z') loop
      Append (C1, C);
    end loop;
    --| c1.Clear();
    Clear (C1);
    --| assertion(c1.IsEmpty(), "** P01: Collection is not initially empty");
    Assertion (Is_Empty (C1), "** P01: Collection is not initially empty");
    --| assertion((c1.Length() == 0), "** P02: Collection Length is not initially zero");
    Assertion (Length (C1) = 0,
	       "** P02: Collection Length is not initially zero");   
    --| c1.Insert('1');
    Insert (C1, '1');
    --| c1.Insert('3', 0);
    Insert (C1, '3', 1);
    --| c1.Insert('2', 1);
    Insert (C1, '2', 2);
    --| assertion(!(c1.IsEmpty()), "** P03: Collection is empty");
    Assertion (not Is_Empty (C1), "** P03: Collection is empty");
    --| assertion((c1.Length() == 3), "** P04: Collection Length is not correct");
    Assertion (Length (C1) = 3, "** P04: Collection Length is not correct");
    --| assertion((c1.First() == '3'), "** P05: Collection First is not correct");
    Assertion (First (C1) = '3', "** P05: Collection First is not correct");
    --| assertion((c1.Last() == '1'), "** P05: Collection Last is not correct");
    Assertion (Last (C1) = '1', "** P05: Collection Last is not correct");
    --| assertion((c1[1] == '2'), "** P07: Collection Item is not correct");
    Assertion (Item_At (C1, 2) = '2', "** P07: Collection Item is not correct");
    --| c1.Clear();
    Clear (C1);
    --| c1.Append('6');
    Append (C1, '6');
    --| c1.Append('4');
    Append (C1, '4');
    --| c1.Append('5', 0);
    Append (C1, '5', 1);
    --| assertion(!(c1.IsEmpty()), "** P08: Collection is empty");
    Assertion (not Is_Empty (C1), "** P08: Collection is empty");
    --| assertion((c1.Length() == 3), "** P09: Collection Length is not correct");
    Assertion (Length (C1) = 3, "** P09: Collection Length is not correct");
    --| assertion((c1.First() == '6'), "** P10: Collection First is not correct");
    Assertion (First (C1) = '6', "** P10: Collection First is not correct");
    --| assertion((c1.Last() == '4'), "** P11: Collection Last is not correct");
    Assertion (Last (C1) = '4', "** P11: Collection Last is not correct");
    --| c1.Remove(1);
    Remove (C1, 2);
    --| c1.Remove(0);
    Remove (C1, 1);
    --| assertion((c1.Length() == 1), "** P12: Collection Length is not correct");
    Assertion (Length (C1) = 1, "** P12: Collection Length is not correct");
    --| assertion((c1.First() == '4'), "** P13: Collection First is not correct");
    Assertion (First (C1) = '4', "** P13: Collection First is not correct");
    --| c1.Remove(0);
    Remove (C1, 1);
    --| assertion(c1.IsEmpty(), "** P14: Collection is not empty");
    Assertion (Is_Empty (C1), "** P14: Collection is not empty");
    --| assertion((c1.Length() == 0), "** P15: Collection Length is not zero");
    Assertion (Length (C1) = 0, "** P15: Collection Length is not zero");
    --| c1.Insert('8');
    Insert (C1, '8');
    --| c1.Append('7');
    Append (C1, '7');
    --| c1.Insert('9');
    Insert (C1, '9');
    --| c1.Remove(1);
    Remove (C1, 2);
    --| c1.Remove(0);
    Remove (C1, 1);
    --| assertion((c1[0] == '7'), "** P16: Collection Item is not correct");
    Assertion (Item_At (C1, 1) = '7', "** P16: Collection Item is not correct");
    --| assertion((c1.First() == '7'), "** P17: Collection First is not correct");
    Assertion (First (C1) = '7', "** P17: Collection First is not correct");
    --| assertion((c1.Last() == '7'), "** P18: Collection Last is not correct");
    Assertion (Last (C1) = '7', "** P18: Collection Last is not correct");
    --| c2 = c1;
    C2 := C1;
    --| assertion(!(c1.IsEmpty()), "** P19: Collection is empty");
    Assertion (not Is_Empty (C1), "** P19: Collection is empty");
    --| assertion((c1.Length() == 1), "** P20: Collection Length is not correct");
    Assertion (Length (C1) = 1, "** P20: Collection Length is not correct");
    --| assertion((c1.First() == '7'), "** P21: Collection First is not correct");
    Assertion (First (C1) = '7', "** P21: Collection First is not correct");
    --| assertion(!(c2.IsEmpty()), "** P22: Collection is empty");
    Assertion (not Is_Empty (C2), "** P22: Collection is empty");
    --| assertion((c2.Length() == 1), "** P23: Collection Length is not correct");
    Assertion (Length (C2) = 1, "** P23: Collection Length is not correct");
    --| assertion((c2.Last() == '7'), "** P24: Collection Last is not correct");
    Assertion (Last (C2) = '7', "** P24: Collection Last is not correct");
    --| assertion((c1 == c2), "** P25: Collections are not equal");
    Assertion (C1 = C2, "** P25: Collections are not equal");
    --| c2.Clear();
    Clear (C2);
    --| assertion(!(c1.IsEmpty()), "** P26: Collection is empty");
    Assertion (not Is_Empty (C1), "** P26: Collection is empty");
    --| assertion((c1.Length() == 1), "** P27: Collection Length is not correct");
    Assertion (Length (C1) = 1, "** P27: Collection Length is not correct");
    --| assertion((c1[0] == '7'), "** P28: Collection Item is not correct");
    Assertion (Item_At (C1, 1) = '7', "** P28: Collection Item is not correct");
    --| assertion(c2.IsEmpty(), "** P29: Collection is not empty");
    Assertion (Is_Empty (C2), "** P29: Collection is not empty");
    --| assertion((c2.Length() == 0), "** P30: Collection Length is not correct");
    Assertion (Length (C2) = 0, "** P30: Collection Length is not correct");
    --| assertion((c1 != c2), "** P31: Collections not equal");
    Assertion (C1 /= C2, "** P31: Collections not equal");
    --| for (char c = 'z'; (c >= 'a'); c--)
    --|   c2.Append(c);
    for C in reverse Character'('a') .. Character'('z') loop
      Append (C2, C);
    end loop;
    --| c1 = c2;
    C1 := C2;
    --| assertion((c1 == c2), "** P32: Collections not equal");
    Assertion (C1 = C2, "** P32: Collections are not equal");
    --| assertion((c1.Location('g') == 19), "** P33: Collection Location is not correct");
    Assertion (Location (C1, 'g') = 20,
	       "** P33: Collection Location is not correct");
    --| c1.Replace(19, 'A');
    Replace (C1, 20, 'A');
    --| assertion((c1[19] == 'A'), "** P34: Collection Item is not correct");
    Assertion (Item_At (C1, 20) = 'A', "** P34: Collection Item is not correct");
    --| c1.Replace(19, 'g');
    Replace (C1, 20, 'g');
    --| c1.Append('A');
    Append (C1, 'A');
    --| assertion((c1[19] == 'g'), "** P35: Collection Item is not correct");
    Assertion (Item_At (C1, 20) = 'g',
	       "** P35: Collection Item is not correct");
    --| c1.Clear();
    Clear (C1);
    --| c2.Clear();
    Clear (C2);
    --| c1.Insert('7');
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

  Put_Line ("Completed Collection tests");

end Collection_Test;
