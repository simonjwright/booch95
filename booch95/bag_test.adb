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

with Ada.Text_Io;
with BC;
with Bag_Test_Support;

procedure Bag_Test is

  use Ada.Text_Io;
  use Bag_Test_Support;

  procedure Assertion (B : Boolean; S : String) is
  begin
    if not B then
      Put_Line (S);
    end if;
  end Assertion;

--    procedure Print_Bag (S : in out Bags.Bag'Class; Named : String) is
--      procedure Print (Item : Character; OK : out Boolean) is
--      begin
--        Put (" " & Item);
--        OK := True;
--      end Print;
--      procedure Visitor is new Containers.Visit (Print);
--    begin
--      Put ("Bag " & Named);
--      Visitor (S);
--      New_Line;
--    end Print_Bag;

  procedure Test (B1, B2 : in out Bags.Bag'Class) is
    Status : Boolean;
  begin
    Assertion (Bags.Is_Empty (B1),
               "** P01: Bag is not initially empty");
    Assertion (Bags.Extent (B1) = 0,
               "** P02: Bag Extent is not initially zero");
    Assertion (Bags.Total_Size (B1) = 0,
               "** P03: Bag TotalSize is not initially zero");
    Bags.Add (B1, '1');
    Bags.Add (B1, '2');
    Bags.Add (B1, '2');
    Bags.Add (B1, '3');
    Assertion (not Bags.Is_Empty (B1), "** P04: Bag is empty");
    Assertion (Bags.Extent (B1) = 3, "** P05: Bag extent is not correct");
    Assertion (Bags.Total_Size (B1) = 4,
               "** P06: Bag TotalSize is not correct");
    Assertion (Bags.Is_Member (B1, '1'),
               "** P07: Bag membership is not correct");
    Assertion (Bags.Is_Member (B1, '2'),
               "** P08: Bag membership is not correct");
    Assertion (Bags.Is_Member (B1, '3'),
               "** P09: Bag membership is not correct");
    Assertion (Bags.Count (B1. '2') = 2,
	       "** P10: Bag Count is not correct");
    Assertion (Bags.Count (B1. '3') = 1,
	       "** P11: Bag Count is not correct");
    Bags.Clear (B1);
    Assertion (Bags.Is_Empty (B1), "** P12: Bag is not empty");
    Assertion (Bags.Extent (B1) = 0, "** P13: Bag extent is not zero");
    Assertion (Bags.Total_Size (B1) = 0,
               "** P14: Bag TotalSize is not zero");
    Bags.Add (B1, '4');
    Bags.Add (B1, '5');
    Bags.Add (B1, '6');
    Bags.Add (B1, '5');
    Bags.Add (B1, '6');
    Bags.Add (B1, '6');
    Assertion (not Bags.Is_Empty (B1), "** P15: Bag is empty");
    Assertion (Bags.Extent (B1) = 3, "** P16: Bag extent is not correct");
    Assertion (Bags.Total_Size (B1) = 6,
               "** P17: Bag TotalSize is not zero");
    Assertion (Bags.Is_Member (B1, '4'),
               "** P18: Bag membership is not correct");
    Assertion (Bags.Is_Member (B1, '5'),
               "** P19: Bag membership is not correct");
    Assertion (Bags.Is_Member (B1, '6'),
               "** P20: Bag membership is not correct");
    Bags.Remove (B1, '4');
    Bags.Remove (B1, '5');
    Assertion (not Bags.Is_Empty (B1), "** P21: Bag is empty");
    Assertion (Bags.Extent (B1) = 2, "** P22: Bag extent is not correct");
    Assertion (Bags.Total_Size (B1) = 4,
               "** P23: Bag TotalSize is not zero");
-- XXX
    Assertion (not Bags.Is_Member (B1, '4'),
               "** P17: Bag membership is not correct");
    Assertion (Bags.Is_Member (B1, '5'),
               "** P18: Bag membership is not correct");
    Assertion (not Bags.Is_Member (B1, '6'),
               "** P19: Bag membership is not correct");
    Bags.Remove (B1, '5');
    Assertion (Bags.Is_Empty (B1), "** P20: Bag is not empty");
    Assertion (Bags.Extent (B1) = 0, "** P21: Bag extent is not zero");
    Bags.Add (B1, '7');
    Bags.Add (B1, '8');
    Bags.Add (B1, '9');
    Bags.Remove (B1, '8');
    Bags.Remove (B1, '9');
    Assertion (not Bags.Is_Empty (B1), "** P22: Bag is empty");
    Assertion (Bags.Extent (B1) = 1, "** P23: Bag extent is not correct");
    Assertion (Bags.Is_Member (B1, '7'),
               "** P24: Bag membership is not correct");
    B2 := B1;
    Assertion (not Bags.Is_Empty (B1), "** P25: Bag is empty");
    Assertion (Bags.Extent (B1) = 1, "** P26: Bag extent is not correct");
    Assertion (Bags.Is_Member (B1, '7'),
               "** P27: Bag membership is not correct");
    Assertion (not Bags.Is_Empty (B2), "** P28: Bag is empty");
    Assertion (Bags.Extent (B2) = 1, "** P29: Bag extent is not correct");
    Assertion (Bags.Is_Member (B2, '7'),
               "** P30: Bag membership is not correct");
    Assertion (Bags.Are_Equal (B1, B2), "** P31: Bags are not equal");
    Assertion (Bags.Is_Subset (B2, B1), "** P32: Bags are not subsets");
    Assertion (not Bags.Is_Proper_Subset (B2, B1),
               "** P33: Bags are proper subsets");
    Bags.Add (B1, '1');
    Bags.Add (B1, '2');
    Bags.Add (B1, '3');
    Assertion (Bags.Is_Subset (B2, B1), "** P34: Bags are not subsets");
    Assertion (Bags.Is_Proper_Subset (B2, B1),
               "** P35: Bags are not proper subsets");
    Bags.Add (B2, '8');
    Bags.Add (B2, '9');
    Bags.Union (B1, B2);
    Assertion (Bags.Extent (B1) = 6, "** P36: Bag extent is not correct");
    Assertion (Bags.Is_Member (B1, '8'),
               "** P37: Bag membership is not correct");
    Assertion (Bags.Is_Member (B1, '9'),
               "** P38: Bag membership is not correct");
    Bags.Remove (B1, '9');
    Assertion (not Bags.Is_Subset (B2, B1),
               "** P39: Bags are subsets");
    Assertion (not Bags.Is_Proper_Subset (B2, B1),
               "** P40: Bags are proper subsets");
    Bags.Intersection (B1, B2);
    Assertion (Bags.Extent (B1) = 2, "** P41: Bag extent is not correct");
    Assertion (Bags.Is_Member (B1, '7'),
               "** P42: Bag membership is not correct");
    Assertion (Bags.Is_Member (B1, '8'),
               "** P43: Bag membership is not correct");
    Bags.Add (B1, '1');
    Bags.Add (B1, '2');
    Bags.Add (B1, '3');
    Bags.Difference (B1, B2);
    Assertion (Bags.Extent (B1) = 3, "** P44: Bag extent is not correct");
    Assertion (Bags.Is_Member (B1, '1'),
               "** P45: Bag membership is not correct");
    Assertion (Bags.Is_Member (B1, '2'),
               "** P46: Bag membership is not correct");
    Assertion (Bags.Is_Member (B1, '3'),
               "** P47: Bag membership is not correct");
    Bags.Remove (B1, '2');
    Bags.Remove (B1, '3');
    Bags.Add (B1, '3', Added => Status);
    if not Status then
      Put_Line ("** P48: Bag add is not correct");
    end if;
    Bags.Add (B1, '3', Added => Status);
    if Status then
      Put_Line ("** P49: Bag add is not correct");
    end if;
    begin
      Bags.Remove (B1, '3');
    exception
      when others =>
        Put_Line ("** P50: Bag remove is not correct");
    end;
    begin
      Bags.Remove (B1, '3');
      Put_Line ("** P51: Bag remove is not correct");
    exception
      when BC.Not_Found => null;
      when others => Put_Line ("** P51: Bag remove is not correct");
    end;
  end Test;

  procedure Test_Active_Iterator (B : in out Bags.Bag'Class) is
    use Containers; use Bags;
    Iter : Containers.Iterator := New_Iterator (B);
  begin
    while not Containers.Is_Done (Iter) loop
      Put_Line ("      Item: "
                & Containers.Current_Item (Iter));
      Containers.Next (Iter);
    end loop;
  end Test_Active_Iterator;

  procedure Process (Item : Character; OK : out Boolean) is
  begin
    Put_Line ("      Item: " & Item);
    OK := True;
  end Process;

  procedure Process_Modifiable (Item : in out Character; OK : out Boolean) is
  begin
    Put_Line ("      Item (RW): " & Item);
    OK := True;
  end Process_Modifiable;

  procedure Test_Passive_Iterator (B : in out Bags.Bag'Class) is
    procedure Visitor is new Containers.Visit (Process);
  begin
    Visitor (B);
  end Test_Passive_Iterator;

  procedure Test_Passive_Modifying_Iterator (B : in out Bags.Bag'Class) is
    procedure Modifier is new Containers.Modify (Process_Modifiable);
  begin
    Modifier (B);
  end Test_Passive_Modifying_Iterator;

--    Bag_B_Pu1, Bag_B_Pu2 : BB.Bounded_Bag;
--    Bag_D_Pu1, Bag_D_Pu2 : BD.Dynamic_Bag;
  Bag_U_Pu1, Bag_U_Pu2 : BU.Unbounded_Bag;

begin
  Put_Line ("Starting bag tests");
--    Put_Line ("...Bounded Bag");
--    Test (Bag_B_Pu1, Bag_B_Pu2);
--    Put_Line ("...Dynamic Bag");
--    SD.Preallocate (Bag_D_Pu1, 50);
--    Test (Bag_D_Pu1, Bag_D_Pu2);
  Put_Line ("...Unbounded Bag");
  Test (Bag_U_Pu1, Bag_U_Pu2);

  Put_Line ("...Bag Active Iterator");
--    Put_Line ("   Bounded:");
--    Test_Active_Iterator (Bag_B_Pu1);
--    Put_Line ("   Dynamic:");
--    Test_Active_Iterator (Bag_D_Pu1);
  Put_Line ("   Unbounded:");
  Test_Active_Iterator (Bag_U_Pu1);
  Put_Line ("...Bag Passive Iterator");
--    Put_Line ("   Bounded:");
--    Test_Passive_Iterator (Bag_B_Pu1);
--    Test_Passive_Modifying_Iterator (Bag_B_Pu1);
--    Put_Line ("   Dynamic:");
--    Test_Passive_Iterator (Bag_D_Pu1);
--    Test_Passive_Modifying_Iterator (Bag_D_Pu1);
  Put_Line ("   Unbounded:");
  Test_Passive_Iterator (Bag_U_Pu1);
  Test_Passive_Modifying_Iterator (Bag_U_Pu1);

--    Assertion (SB.Is_Member (Bag_B_Pu1, '1'),
--               "** M01: Bag membership is not correct");
--    Assertion (SB.Extent (Bag_B_Pu2) = 3, "** M02: Bag extent is not correct");
--    Assertion (SD.Is_Member (Bag_D_Pu1, '1'),
--               "** M05: Bag membership is not correct");
--    Assertion (SD.Extent (Bag_D_Pu2) = 3, "** M06: Bag extent is not correct");
--    Assertion (SU.Is_Member (Bag_U_Pu1, '1'),
--               "** M09: Bag membership is not correct");
--    Assertion (SU.Extent (Bag_U_Pu2) = 3, "** M10: Bag extent is not correct");
--    Assertion (SB.Available (Bag_B_Pu1) = 299,
--               "** M13: Available space is not correct");
--    Assertion (SB.Available (Bag_B_Pu2) = 297,
--               "** M14: Available space is not correct");
  Put_Line ("Completed bag tests");
end Bag_Test;
