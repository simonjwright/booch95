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

with Ada.Text_Io;
with BC;
with Chunks;
with Map_Test_Support;

procedure Map_Test is

  use Ada.Text_Io;
  use Map_Test_Support;
  use Chunks;

  procedure Assertion (B : Boolean; S : String) is
  begin
    if not B then
      Put_Line (S);
    end if;
  end Assertion;

  procedure Test (M1, M2 : in out Maps.Map'Class) is
  begin
    Assertion (Maps.Is_Empty (M1),
               "** P01: Map is not initially empty");
    Assertion (Maps.Extent (M1) = 0,
               "** P02: Map Extent is not initially zero");
    Maps.Bind (M1, '1', Gitems (1)'Access);
    Maps.Bind (M1, '2', Gitems (2)'Access);
    Maps.Bind (M1, '3', Gitems (3)'Access);
    Maps.Bind (M1, '4', Gitems (4)'Access);
    Maps.Bind (M1, '5', Gitems (5)'Access);
    Maps.Bind (M1, '6', Gitems (6)'Access);
    Maps.Bind (M1, '7', Gitems (7)'Access);
    Assertion (not Maps.Is_Empty (M1), "** P03: Map is empty");
    Assertion (Maps.Extent (M1) = 7, "** P04: Map Extent is not correct");
    Assertion (Maps.Is_Bound (M1, '3'), "** P05: Map binding is not correct");
    Maps.Clear (M1);
    Assertion (Maps.Is_Empty (M1), "** P06: Map is not empty");
    Assertion (Maps.Extent (M1) = 0, "** P07: Map Extent is not zero");
    Maps.Bind (M1, '1', Gitems (1)'Access);
    Maps.Bind (M1, '2', Gitems (2)'Access);
    Maps.Bind (M1, '3', Gitems (3)'Access);
    Maps.Bind (M1, '4', Gitems (4)'Access);
    Assertion (not Maps.Is_Empty (M1), "** P08: Map is empty");
    Assertion (Maps.Extent (M1) = 4, "** P09: Map Extent is not correct");
    Assertion (Maps.Is_Bound (M1, '4'), "** P10: Map binding is not correct");
    Maps.Unbind (M1, '1');
    Maps.Unbind (M1, '3');
    Assertion (not Maps.Is_Empty (M1), "** P11: Map is empty");
    Assertion (Maps.Extent (M1) = 2, "** P12: Map Extent is not correct");
    Assertion (Maps.Is_Bound (M1, '2'), "** P13: Map binding is not correct");
    Maps.Unbind (M1, '4');
    Maps.Unbind (M1, '2');
    Assertion (Maps.Is_Empty (M1), "** P14: Map is not empty");
    Assertion (Maps.Extent (M1) = 0, "** P15: Map Extent is not zero");
    Maps.Bind (M1, '5', Gitems (5)'Access);
    Maps.Bind (M1, '6', Gitems (6)'Access);
    Maps.Bind (M1, '7', Gitems (7)'Access);
    Maps.Rebind (M1, '5', Gitems (7)'Access);
    Maps.Rebind (M1, '6', Gitems (6)'Access);
    Maps.Rebind (M1, '7', Gitems (5)'Access);
    Assertion (not Maps.Is_Empty (M1), "** P16: Map is empty");
    Assertion (Maps.Extent (M1) = 3, "** P17: Map Extent is not correct");
    Assertion (Maps.Is_Bound (M1, '7'), "** P18: Map binding is not correct");
    Assertion (Maps.Value_Of (M1, '5') = Gitems (7)'Access,
               "** P19: Map binding is not correct");
    Assertion (Maps.Value_Of (M1, '6') = Gitems (6)'Access,
               "** P20: Map binding is not correct");
    Assertion (Maps.Value_Of (M1, '7') = Gitems (5)'Access,
               "** P21: Map binding is not correct");
    M2 := M1;
    Assertion (not Maps.Is_Empty (M2), "** P22: Map is empty");
    Assertion (Maps.Extent (M2) = 3, "** P23: Map Extent is not correct");
    Assertion (Maps.Is_Bound (M2, '7'), "** P24: Map binding is not correct");
    Assertion (Maps.Value_Of (M2, '5') = Gitems (7)'Access,
               "** P25: Map binding is not correct");
    Assertion (Maps.Value_Of (M2, '6') = Gitems (6)'Access,
               "** P26: Map binding is not correct");
    Assertion (Maps.Value_Of (M2, '7') = Gitems (5)'Access,
               "** P27: Map binding is not correct");
    Assertion (Maps.Are_Equal (M1, M2), "** P28: Maps are not equal");
    Maps.Clear (M2);
    Assertion (not Maps.Is_Empty (M1), "** P29: Map is empty");
    Assertion (Maps.Extent (M1) = 3, "** P30: Map Extent is not correct");
    Assertion (Maps.Is_Bound (M1, '6'), "** P31: Map binding is not correct");
    Assertion (Maps.Is_Empty (M2), "** P32: Map is not empty");
    Assertion (Maps.Extent (M2) = 0, "** P33: Map Extent is not correct");
    Assertion (Maps."/=" (M1, M2), "** P34: Maps equal");
    Assertion (Maps.Is_Bound (M1, '6'), "** P35: Map binding is not correct");
    Maps.Unbind (M1, '6');
    Assertion (not Maps.Is_Bound (M1, '6'),
               "** P37: Map binding is not correct");
    Maps.Bind (M1, '6', Gitems (6)'Access);
    Assertion (Maps.Is_Bound (M1, '6'), "** P38: Map binding is not correct");
    begin
      Maps.Bind (M1, '6', Gitems (6)'Access);
      Put_Line ("** P40: Map was not already bound");
    exception
      when BC.Duplicate => null;
    end;
    Maps.Unbind (M1, '6');
    begin
      Maps.Rebind (M1, '6', Gitems (6)'Access);
      Put_Line ("** P41: Map was not already unbound");
    exception
      when BC.Not_Found => null;
    end;
    begin
      Maps.Unbind (M1, '6');
      Put_Line ("** P42: Map was not already unbound");
    exception
      when BC.Not_Found => null;
    end;
    Maps.Bind (M1, '6', Gitems (6)'Access);
  end Test;

  procedure Test_Active_Iterator (M : in out Maps.Map'Class) is
    use Containers; use Maps; use MB;
    Iter : Containers.Iterator := New_Iterator (M);
  begin
    while not Containers.Is_Done (Iter) loop
      Put_Line ("      Item: "
                & Containers.Current_Item (Iter)
                & " Value: "
                & Image (Maps.Current_Value (Iter).all));
      Containers.Next (Iter);
    end loop;
  end Test_Active_Iterator;

  procedure Process (Item : Character; Value : Chunk_Ptr; OK : out Boolean) is
  begin
    Put_Line ("      Item: " & Item & " Value: " & Image (Value.all));
    OK := True;
  end Process;

  procedure Process_Modifiable (Item : Character;
                     Value : in out Chunk_Ptr;
                     OK : out Boolean) is
  begin
    Put_Line ("      Item: " & Item & " Value (RW): " & Image (Value.all));
    OK := True;
  end Process_Modifiable;

  procedure Test_Passive_Iterator (M : in out Maps.Map'Class) is
    procedure Visitor is new Maps.Visit (Process);
  begin
    Visitor (Over_The_Container => M);
  end Test_Passive_Iterator;

  procedure Test_Passive_Modifying_Iterator (M : in out Maps.Map'Class) is
    procedure Modifier is new Maps.Modify (Process_Modifiable);
  begin
    Modifier (Over_The_Container => M);
  end Test_Passive_Modifying_Iterator;

  Map_B_Pu1, Map_B_Pu2 : MB.Bounded_Map;
  Map_D_Pu1, Map_D_Pu2 : MD.Dynamic_Map;
  Map_U_Pu1, Map_U_Pu2 : MU.Unbounded_Map;
  Map_UG_Pu1, Map_UG_Pu2 : MUG.Guarded_Unbounded_Map;
  Map_US_Pu1, Map_US_Pu2 : MUS.Synchronized_Unbounded_Map;

begin
  Put_Line ("Starting map tests");
  Put_Line ("...Bounded Map");
  Test (Map_B_Pu1, Map_B_Pu2);
  Put_Line ("...Dynamic Map");
  MD.Preallocate (Map_D_Pu1, 50);
  Test (Map_D_Pu1, Map_D_Pu2);
  Put_Line ("...Unbounded Map");
  Test (Map_U_Pu1, Map_U_Pu2);
  Put_Line ("...Guarded Unbounded Map");
  Test (Map_UG_Pu1, Map_UG_Pu2);
  Put_Line ("...Synchronized Unbounded Map");
  Test (Map_US_Pu1, Map_US_Pu2);

  Put_Line ("...Map Active Iterator");
  Put_Line ("   Bounded:");
  Test_Active_Iterator (Map_B_Pu1);
  Put_Line ("   Dynamic:");
  Test_Active_Iterator (Map_D_Pu1);
  Put_Line ("   Unbounded:");
  Test_Active_Iterator (Map_U_Pu1);
  Put_Line ("   Guarded Unbounded:");
  Test_Active_Iterator (Map_UG_Pu1);
  Put_Line ("   Synchronized Unbounded:");
  Test_Active_Iterator (Map_US_Pu1);

  Put_Line ("...Map Passive Iterator");
  Put_Line ("   Bounded:");
  Test_Passive_Iterator (Map_B_Pu1);
  Test_Passive_Modifying_Iterator (Map_B_Pu1);
  Put_Line ("   Dynamic:");
  Test_Passive_Iterator (Map_D_Pu1);
  Test_Passive_Modifying_Iterator (Map_D_Pu1);
  Put_Line ("   Unbounded:");
  Test_Passive_Iterator (Map_U_Pu1);
  Test_Passive_Modifying_Iterator (Map_U_Pu1);
  Put_Line ("   Guarded Unbounded:");
  Test_Passive_Iterator (Map_UG_Pu1);
  Test_Passive_Modifying_Iterator (Map_UG_Pu1);
  Put_Line ("   Synchronized Unbounded:");
  Test_Passive_Iterator (Map_US_Pu1);
  Test_Passive_Modifying_Iterator (Map_US_Pu1);

  Assertion (MB.Is_Bound (Map_B_Pu1, '6'),
             "** M01: Map binding is not correct");
  Assertion (MB.Extent (Map_B_Pu2) = 0, "** M02: Map Extent is not correct");
  Assertion (MD.Is_Bound (Map_D_Pu1, '6'),
             "** M03: Map binding is not correct");
  Assertion (MD.Extent (Map_D_Pu2) = 0, "** M04: Map Extent is not correct");
  Assertion (MU.Is_Bound (Map_U_Pu1, '6'),
             "** M05: Map binding is not correct");
  Assertion (MU.Extent (Map_U_Pu2) = 0, "** M06: Map Extent is not correct");
  -- I don't understand this one ..
  declare
    Map_D_Pu3 : MD.Dynamic_Map := Map_D_Pu1;
  begin
    Assertion (MD."=" (Map_D_Pu1, Map_D_Pu3), "** M08: Maps are not equal");
  end;
  declare
    Map_U_Pu3 : MU.Unbounded_Map := Map_U_Pu1;
  begin
    Assertion (MU."=" (Map_U_Pu1, Map_U_Pu3), "** M09: Maps are not equal");
  end;
  Assertion (MB.Available (Map_B_Pu1) = 297,
             "** M10: Available space is not correct");
  Assertion (MB.Available (Map_B_Pu2) = 300,
             "** M11: Available space is not correct");
  Put_Line ("Completed map tests");
end Map_Test;
