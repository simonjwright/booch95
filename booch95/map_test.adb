-- Copyright (C) 1994-1998 Grady Booch and Simon Wright.
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
--| void test(BC_TMap<Char, CPtr>& m1, BC_TMap<Char, CPtr>& m2)
--| {
--|   m1.SetHashFunction(CharHash);
--|   m2.SetHashFunction(CharHash);
--|   assertion(m1.IsEmpty(), "** P01: Map is not initially empty");
    Assertion (Maps.Is_Empty (M1),
               "** P01: Map is not initially empty");
--|   assertion((m1.Extent() == 0), "** P02: Map Extent is not initially zero");
    Assertion (Maps.Extent (M1) = 0,
               "** P02: Map Extent is not initially zero");
--|   m1.Bind('1', &gItems[1]);
    Maps.Bind (M1, '1', Gitems (1)'Access);
--|   m1.Bind('2', &gItems[2]);
    Maps.Bind (M1, '2', Gitems (2)'Access);
--|   m1.Bind('3', &gItems[3]);
    Maps.Bind (M1, '3', Gitems (3)'Access);
--|   m1.Bind('4', &gItems[4]);
    Maps.Bind (M1, '4', Gitems (4)'Access);
--|   m1.Bind('5', &gItems[5]);
    Maps.Bind (M1, '5', Gitems (5)'Access);
--|   m1.Bind('6', &gItems[6]);
    Maps.Bind (M1, '6', Gitems (6)'Access);
--|   m1.Bind('7', &gItems[7]);
    Maps.Bind (M1, '7', Gitems (7)'Access);
--|   assertion(!(m1.IsEmpty()), "** P03: Map is empty");
    Assertion (not Maps.Is_Empty (M1), "** P03: Map is empty");
--|   assertion((m1.Extent() == 7), "** P04: Map Extent is not correct");
    Assertion (Maps.Extent (M1) = 7, "** P04: Map Extent is not correct");
--|   assertion(m1.IsBound('3'), "** P05: Map binding is not correct");
    Assertion (Maps.Is_Bound (M1, '3'), "** P05: Map binding is not correct");
--|   m1.Clear();
    Maps.Clear (M1);
--|   assertion(m1.IsEmpty(), "** P06: Map is not empty");
    Assertion (Maps.Is_Empty (M1), "** P06: Map is not empty");
--|   assertion((m1.Extent() == 0), "** P07: Map Extent is not zero");
    Assertion (Maps.Extent (M1) = 0, "** P07: Map Extent is not zero");
--|   m1.Bind('1', &gItems[1]);
    Maps.Bind (M1, '1', Gitems (1)'Access);
--|   m1.Bind('2', &gItems[2]);
    Maps.Bind (M1, '2', Gitems (2)'Access);
--|   m1.Bind('3', &gItems[3]);
    Maps.Bind (M1, '3', Gitems (3)'Access);
--|   m1.Bind('4', &gItems[4]);
    Maps.Bind (M1, '4', Gitems (4)'Access);
--|   assertion(!(m1.IsEmpty()), "** P08: Map is empty");
    Assertion (not Maps.Is_Empty (M1), "** P08: Map is empty");
--|   assertion((m1.Extent() == 4), "** P09: Map Extent is not correct");
    Assertion (Maps.Extent (M1) = 4, "** P09: Map Extent is not correct");
--|   assertion(m1.IsBound('4'), "** P10: Map binding is not correct");
    Assertion (Maps.Is_Bound (M1, '4'), "** P10: Map binding is not correct");
--|   m1.Unbind('1');
    Maps.Unbind (M1, '1');
--|   m1.Unbind('3');
    Maps.Unbind (M1, '3');
--|   assertion(!(m1.IsEmpty()), "** P11: Map is empty");
    Assertion (not Maps.Is_Empty (M1), "** P11: Map is empty");
--|   assertion((m1.Extent() == 2), "** P12: Map Extent is not correct");
    Assertion (Maps.Extent (M1) = 2, "** P12: Map Extent is not correct");
--|   assertion(m1.IsBound('2'), "** P13: Map binding is not correct");
    Assertion (Maps.Is_Bound (M1, '2'), "** P13: Map binding is not correct");
--|   m1.Unbind('4');
    Maps.Unbind (M1, '4');
--|   m1.Unbind('2');
    Maps.Unbind (M1, '2');
--|   assertion(m1.IsEmpty(), "** P14: Map is not empty");
    Assertion (Maps.Is_Empty (M1), "** P14: Map is not empty");
--|   assertion((m1.Extent() == 0), "** P15: Map Extent is not zero");
    Assertion (Maps.Extent (M1) = 0, "** P15: Map Extent is not zero");
--|   m1.Bind('5', &gItems[5]);
    Maps.Bind (M1, '5', Gitems (5)'Access);
--|   m1.Bind('6', &gItems[6]);
    Maps.Bind (M1, '6', Gitems (6)'Access);
--|   m1.Bind('7', &gItems[7]);
    Maps.Bind (M1, '7', Gitems (7)'Access);
--|   m1.Rebind('5', &gItems[7]);
    Maps.Rebind (M1, '5', Gitems (7)'Access);
--|   m1.Rebind('6', &gItems[6]);
    Maps.Rebind (M1, '6', Gitems (6)'Access);
--|   m1.Rebind('7', &gItems[5]);
    Maps.Rebind (M1, '7', Gitems (5)'Access);
--|   assertion(!(m1.IsEmpty()), "** P16: Map is empty");
    Assertion (not Maps.Is_Empty (M1), "** P16: Map is empty");
--|   assertion((m1.Extent() == 3), "** P17: Map Extent is not correct");
    Assertion (Maps.Extent (M1) = 3, "** P17: Map Extent is not correct");
--|   assertion(m1.IsBound('7'), "** P18: Map binding is not correct");
    Assertion (Maps.Is_Bound (M1, '7'), "** P18: Map binding is not correct");
--|   assertion((*m1.ValueOf('5') == &gItems[7]), "** P19: Map binding is not correct");
    Assertion (Maps.Value_Of (M1, '5') = Gitems (7)'Access,
               "** P19: Map binding is not correct");
--|   assertion((*m1.ValueOf('6') == &gItems[6]), "** P20: Map binding is not correct");
    Assertion (Maps.Value_Of (M1, '6') = Gitems (6)'Access,
               "** P20: Map binding is not correct");
--|   assertion((*m1.ValueOf('7') == &gItems[5]), "** P21: Map binding is not correct");
    Assertion (Maps.Value_Of (M1, '7') = Gitems (5)'Access,
               "** P21: Map binding is not correct");
--|   m2 = m1;
    M2 := M1;
--|   assertion(!(m2.IsEmpty()), "** P22: Map is empty");
    Assertion (not Maps.Is_Empty (M2), "** P22: Map is empty");
--|   assertion((m2.Extent() == 3), "** P23: Map Extent is not correct");
    Assertion (Maps.Extent (M2) = 3, "** P23: Map Extent is not correct");
--|   assertion(m2.IsBound('7'), "** P24: Map binding is not correct");
    Assertion (Maps.Is_Bound (M2, '7'), "** P24: Map binding is not correct");
--|   assertion((*m2.ValueOf('5') == &gItems[7]), "** P25: Map binding is not correct");
    Assertion (Maps.Value_Of (M2, '5') = Gitems (7)'Access,
               "** P25: Map binding is not correct");
--|   assertion((*m2.ValueOf('6') == &gItems[6]), "** P26: Map binding is not correct");
    Assertion (Maps.Value_Of (M2, '6') = Gitems (6)'Access,
               "** P26: Map binding is not correct");
--|   assertion((*m2.ValueOf('7') == &gItems[5]), "** P27: Map binding is not correct");
    Assertion (Maps.Value_Of (M2, '7') = Gitems (5)'Access,
               "** P27: Map binding is not correct");
--|   assertion((m1 == m2), "** P28: Maps are not equal");
    Assertion (Maps.Are_Equal (M1, M2), "** P28: Maps are not equal");
--|   m2.Clear();
    Maps.Clear (M2);
--|   assertion(!(m1.IsEmpty()), "** P29: Map is empty");
    Assertion (not Maps.Is_Empty (M1), "** P29: Map is empty");
--|   assertion((m1.Extent() == 3), "** P30: Map Extent is not correct");
    Assertion (Maps.Extent (M1) = 3, "** P30: Map Extent is not correct");
--|   assertion(m1.IsBound('6'), "** P31: Map binding is not correct");
    Assertion (Maps.Is_Bound (M1, '6'), "** P31: Map binding is not correct");
--|   assertion(m2.IsEmpty(), "** P32: Map is not empty");
    Assertion (Maps.Is_Empty (M2), "** P32: Map is not empty");
--|   assertion((m2.Extent() == 0), "** P33: Map Extent is not correct");
    Assertion (Maps.Extent (M2) = 0, "** P33: Map Extent is not correct");
--|   assertion((m1 != m2), "** P34: Maps not equal");
    Assertion (Maps."/=" (M1, M2), "** P34: Maps equal");
--|   assertion(m1.IsBound('6'), "** P35: Map binding is not correct");
    Assertion (Maps.Is_Bound (M1, '6'), "** P35: Map binding is not correct");
--|   assertion(m1.IsBound('6'), "** P36: Map binding is not correct");
--|   m1.Unbind('6');
    Maps.Unbind (M1, '6');
--|   assertion(!(m1.IsBound('6')), "** P37: Map binding is not correct");
    Assertion (not Maps.Is_Bound (M1, '6'),
               "** P37: Map binding is not correct");
--|   m1.Bind('6', &gItems[6]);
    Maps.Bind (M1, '6', Gitems (6)'Access);
--|   assertion(m1.IsBound('6'), "** P38: Map binding is not correct");
    Assertion (Maps.Is_Bound (M1, '6'), "** P38: Map binding is not correct");
--|   assertion(m1.IsBound('6'), "** P39: Map binding is not correct");
--|   assertion(!m1.Bind('6', &gItems[6]), "** P40: Map was already bound");
    begin
      Maps.Bind (M1, '6', Gitems (6)'Access);
      Put_Line ("** P40: Map was not already bound");
    exception
      when BC.Duplicate => null;
    end;
--|   m1.Unbind('6');
    Maps.Unbind (M1, '6');
--|   assertion(!m1.Rebind('6', &gItems[6]), "** P41: Map was already unbound");
    begin
      Maps.Rebind (M1, '6', Gitems (6)'Access);
      Put_Line ("** P41: Map was not already unbound");
    exception
      when BC.Not_Found => null;
    end;
--|   assertion(!m1.Unbind('6'), "** P42: Map was already unbound");
    begin
      Maps.Unbind (M1, '6');
      Put_Line ("** P42: Map was not already unbound");
    exception
      when BC.Not_Found => null;
    end;
    Maps.Bind (M1, '6', Gitems (6)'Access);
--|   m1.Bind('6', &gItems[6]);
--| }
  end Test;

--| void test_active_iterator (BC_TMap<Char, CPtr>& m)
--| {
--|   Active_Iterator iter(m);
--|   while (!iter.IsDone()) {
--|     process(*iter.CurrentItem(), *iter.CurrentValue());
--|     iter.Next();;
--|   }
--| }

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

--| BC_Boolean process(const Char& Item, const CPtr& value)
--| {
--|   cout << "      Item: " << Item << " Value: " << *value << "\n";
--|   return 1;
--| }

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

--| void test_passive_iterator (BC_TMap<Char, CPtr>& m)
--| {
--|   Passive_Iterator iter(m);
--|   iter.Apply(process);
--| }

  procedure Test_Passive_Iterator (M : in out Maps.Map'Class) is
    procedure Visitor is new Maps.Visit (Process, M);
  begin
    Visitor;
  end Test_Passive_Iterator;

  procedure Test_Passive_Modifying_Iterator (M : in out Maps.Map'Class) is
    procedure Modifier is new Maps.Modify (Process_Modifiable, M);
  begin
    Modifier;
  end Test_Passive_Modifying_Iterator;

--|   Bounded_Char_Chunk_Map map_b_pu1, map_b_pu2;
  Map_B_Pu1, Map_B_Pu2 : MB.Bounded_Map;
--|   Dynamic_Char_Chunk_Map map_d_pu1(5), map_d_pu2(5);
--|   Unbounded_Char_Chunk_Map map_u_pu1, map_u_pu2;

begin
--|
--|   message("Starting map tests");
  Put_Line ("Starting map tests");
--|
--|   message("...Bounded Map");
  Put_Line ("...Bounded Map");
--|   test(map_b_pu1, map_b_pu2);
  Test (Map_B_Pu1, Map_B_Pu2);
--|
--|   message("...Dynamic Map");
--|   map_d_pu1.Preallocate(50);
--|   test(map_d_pu1, map_d_pu2);
--|
--|   message("...Unbounded Map");
--|   test(map_u_pu1, map_u_pu2);
--|
--|   message("...Map Active Iterator");
  Put_Line ("...Map Active Iterator");
--|   message("   Bounded:");
  Put_Line ("   Bounded:");
--|   test_active_iterator(map_b_pu1);
  Test_Active_Iterator (Map_B_Pu1);
--|   message("   Dynamic:");
--|   test_active_iterator(map_d_pu1);
--|   message("   Unbounded:");
--|   test_active_iterator(map_u_pu1);
--|
--|   message("...Map Passive Iterator");
  Put_Line ("...Map Passive Iterator");
--|   message("   Bounded:");
  Put_Line ("   Bounded:");
--|   test_passive_iterator(map_b_pu1);
  Test_Passive_Iterator (Map_B_Pu1);
  Test_Passive_Modifying_Iterator (Map_B_Pu1);
--|   message("   Dynamic:");
--|   test_passive_iterator(map_d_pu1);
--|   message("   Unbounded:");
--|   test_passive_iterator(map_u_pu1);
--|
--|   assertion(map_b_pu1.IsBound('6'), "** M01: Map binding is not correct");
  Assertion (MB.Is_Bound (Map_B_Pu1, '6'),
             "** M01: Map binding is not correct");
--|   assertion((map_b_pu2.Extent() == 0), "** M02: Map Extent is not correct");
  Assertion (MB.Extent (Map_B_Pu2) = 0, "** M02: Map Extent is not correct");
--|   assertion(map_d_pu1.IsBound('6'), "** M03: Map binding is not correct");
--|   assertion((map_d_pu2.Extent() == 0), "** M04: Map Extent is not correct");
--|   assertion(map_u_pu1.IsBound('6'), "** M05: Map binding is not correct");
--|   assertion((map_u_pu2.Extent() == 0), "** M06: Map Extent is not correct");
--|
--| //  Bounded_Char_Chunk_Map map_b_pu3(map_b_pu1);
--|   Dynamic_Char_Chunk_Map map_d_pu3(map_d_pu1);
--|   Unbounded_Char_Chunk_Map map_u_pu3(map_u_pu1);
--|
--| //  assertion((map_b_pu1 == map_b_pu3), "** M07: Maps are not equal");
--|   assertion((map_d_pu1 == map_d_pu3), "** M08: Maps are not equal");
--|   assertion((map_u_pu1 == map_u_pu3), "** M09: Maps are not equal");
--|
--| //  map_b_pu3.Clear();
--|   map_d_pu3.Clear();
--|   map_u_pu3.Clear();
--|
--|   assertion((map_b_pu1.Available() == 297), "** M10: Available space is not correct");
  Assertion (MB.Available (Map_B_Pu1) = 297,
             "** M10: Available space is not correct");
--|   assertion((map_b_pu2.Available() == 300), "** M11: Available space is not correct");
  Assertion (MB.Available (Map_B_Pu2) = 300,
             "** M11: Available space is not correct");
--|
--|   message("Completed map tests");
  Put_Line ("Completed map tests");
--|
--|   return 0;
--|
end Map_Test;

--| //  The C++ Booch Components (Version 2.3)
--| //  (C) Copyright 1990-1994 Grady Booch. All Rights Reserved..
--| //
--| //  MapT.cpp
--| //
--| //  This file contains tests for the map classes.
--|
--| #include "Items.h"
--| #include "BCStoreM.h"
--| #include "BCMapB.h"
--| #include "BCMapD.h"
--| #include "BCMapU.h"
--|
--| #if NEED_BODIES || __BCPLUSPLUS__ || THINK_CPLUS
--| #include "BCExcept.cpp"
--| #include "BCPool.cpp"
--| #include "BCNodes.cpp"
--| #include "BCBound.cpp"
--| #include "BCDynami.cpp"
--| #include "BCUnboun.cpp"
--| #include "BCHashTa.cpp"
--| #include "BCMap.cpp"
--| #include "BCMapB.cpp"
--| #include "BCMapD.cpp"
--| #include "BCMapU.cpp"
--| #endif
--|
--| Chunk gItems[10];
--|
--| BC_CPool memory_pool(1024);
--| BC_CPool& BC_CManaged::fPool = memory_pool;
--|
--| typedef BC_TMap<Char, CPtr> Char_Chunk_Map;
--|
--| typedef BC_TNode<Char, BC_CManaged> Char_Node;
--| typedef BC_TNode<CPtr, BC_CManaged> Chunk_Node;
--|
--| typedef BC_TBounded<Char, 100U> Char_Bounded;
--| typedef BC_TBounded<CPtr, 100U> Chunk_Bounded;
--|
--| typedef BC_TDynamic<Char, BC_CManaged> Char_Dynamic;
--| typedef BC_TDynamic<CPtr, BC_CManaged> Chunk_Dynamic;
--|
--| typedef BC_TUnbounded<Char, BC_CManaged> Char_Unbounded;
--| typedef BC_TUnbounded<CPtr, BC_CManaged> Chunk_Unbounded;
--|
--| typedef BC_TTable<Char, CPtr, 3U, BC_TBounded<Char, 100U>, BC_TBounded<CPtr, 100U> >Bounded_Table;
--| typedef BC_TTable<Char, CPtr, 3U, BC_TDynamic<Char, BC_CManaged>, BC_TDynamic<CPtr, BC_CManaged> > Dynamic_Table;
--| typedef BC_TTable<Char, CPtr, 3U, BC_TUnbounded<Char, BC_CManaged>, BC_TUnbounded<CPtr, BC_CManaged> > Unbounded_Table;
--|
--| typedef BC_TBoundedMap<Char, CPtr, 3U, 100U> Bounded_Char_Chunk_Map;
--|
--| typedef BC_TDynamicMap<Char, CPtr, 3U, BC_CManaged> Dynamic_Char_Chunk_Map;
--|
--| typedef BC_TUnboundedMap<Char, CPtr, 3U, BC_CManaged> Unbounded_Char_Chunk_Map;
--|
--| typedef BC_TMapActiveIterator<Char, CPtr> Active_Iterator;
--| typedef BC_TMapPassiveIterator<Char, CPtr> Passive_Iterator;
--|
--| void test(BC_TMap<Char, CPtr>& m1, BC_TMap<Char, CPtr>& m2)
--| {
--|   m1.SetHashFunction(CharHash);
--|   m2.SetHashFunction(CharHash);
--|   assertion(m1.IsEmpty(), "** P01: Map is not initially empty");
--|   assertion((m1.Extent() == 0), "** P02: Map Extent is not initially zero");
--|   m1.Bind('1', &gItems[1]);
--|   m1.Bind('2', &gItems[2]);
--|   m1.Bind('3', &gItems[3]);
--|   m1.Bind('4', &gItems[4]);
--|   m1.Bind('5', &gItems[5]);
--|   m1.Bind('6', &gItems[6]);
--|   m1.Bind('7', &gItems[7]);
--|   assertion(!(m1.IsEmpty()), "** P03: Map is empty");
--|   assertion((m1.Extent() == 7), "** P04: Map Extent is not correct");
--|   assertion(m1.IsBound('3'), "** P05: Map binding is not correct");
--|   m1.Clear();
--|   assertion(m1.IsEmpty(), "** P06: Map is not empty");
--|   assertion((m1.Extent() == 0), "** P07: Map Extent is not zero");
--|   m1.Bind('1', &gItems[1]);
--|   m1.Bind('2', &gItems[2]);
--|   m1.Bind('3', &gItems[3]);
--|   m1.Bind('4', &gItems[4]);
--|   assertion(!(m1.IsEmpty()), "** P08: Map is empty");
--|   assertion((m1.Extent() == 4), "** P09: Map Extent is not correct");
--|   assertion(m1.IsBound('4'), "** P10: Map binding is not correct");
--|   m1.Unbind('1');
--|   m1.Unbind('3');
--|   assertion(!(m1.IsEmpty()), "** P11: Map is empty");
--|   assertion((m1.Extent() == 2), "** P12: Map Extent is not correct");
--|   assertion(m1.IsBound('2'), "** P13: Map binding is not correct");
--|   m1.Unbind('4');
--|   m1.Unbind('2');
--|   assertion(m1.IsEmpty(), "** P14: Map is not empty");
--|   assertion((m1.Extent() == 0), "** P15: Map Extent is not zero");
--|   m1.Bind('5', &gItems[5]);
--|   m1.Bind('6', &gItems[6]);
--|   m1.Bind('7', &gItems[7]);
--|   m1.Rebind('5', &gItems[7]);
--|   m1.Rebind('6', &gItems[6]);
--|   m1.Rebind('7', &gItems[5]);
--|   assertion(!(m1.IsEmpty()), "** P16: Map is empty");
--|   assertion((m1.Extent() == 3), "** P17: Map Extent is not correct");
--|   assertion(m1.IsBound('7'), "** P18: Map binding is not correct");
--|   assertion((*m1.ValueOf('5') == &gItems[7]), "** P19: Map binding is not correct");
--|   assertion((*m1.ValueOf('6') == &gItems[6]), "** P20: Map binding is not correct");
--|   assertion((*m1.ValueOf('7') == &gItems[5]), "** P21: Map binding is not correct");
--|   m2 = m1;
--|   assertion(!(m2.IsEmpty()), "** P22: Map is empty");
--|   assertion((m2.Extent() == 3), "** P23: Map Extent is not correct");
--|   assertion(m2.IsBound('7'), "** P24: Map binding is not correct");
--|   assertion((*m2.ValueOf('5') == &gItems[7]), "** P25: Map binding is not correct");
--|   assertion((*m2.ValueOf('6') == &gItems[6]), "** P26: Map binding is not correct");
--|   assertion((*m2.ValueOf('7') == &gItems[5]), "** P27: Map binding is not correct");
--|   assertion((m1 == m2), "** P28: Maps are not equal");
--|   m2.Clear();
--|   assertion(!(m1.IsEmpty()), "** P29: Map is empty");
--|   assertion((m1.Extent() == 3), "** P30: Map Extent is not correct");
--|   assertion(m1.IsBound('6'), "** P31: Map binding is not correct");
--|   assertion(m2.IsEmpty(), "** P32: Map is not empty");
--|   assertion((m2.Extent() == 0), "** P33: Map Extent is not correct");
--|   assertion((m1 != m2), "** P34: Maps not equal");
--|   assertion(m1.IsBound('6'), "** P35: Map binding is not correct");
--|   assertion(m1.IsBound('6'), "** P36: Map binding is not correct");
--|   m1.Unbind('6');
--|   assertion(!(m1.IsBound('6')), "** P37: Map binding is not correct");
--|   m1.Bind('6', &gItems[6]);
--|   assertion(m1.IsBound('6'), "** P38: Map binding is not correct");
--|   assertion(m1.IsBound('6'), "** P39: Map binding is not correct");
--|   assertion(!m1.Bind('6', &gItems[6]), "** P40: Map was already bound");
--|   m1.Unbind('6');
--|   assertion(!m1.Rebind('6', &gItems[6]), "** P41: Map was already unbound");
--|   assertion(!m1.Unbind('6'), "** P42: Map was already unbound");
--|   m1.Bind('6', &gItems[6]);
--| }
--|
--| BC_Boolean process(const Char& Item, const CPtr& value)
--| {
--|   cout << "      Item: " << Item << " Value: " << *value << "\n";
--|   return 1;
--| }
--|
--| void test_active_iterator (BC_TMap<Char, CPtr>& m)
--| {
--|   Active_Iterator iter(m);
--|   while (!iter.IsDone()) {
--|     process(*iter.CurrentItem(), *iter.CurrentValue());
--|     iter.Next();;
--|   }
--| }
--|
--| void test_passive_iterator (BC_TMap<Char, CPtr>& m)
--| {
--|   Passive_Iterator iter(m);
--|   iter.Apply(process);
--| }
--|
--| main () {
--|
--|   Bounded_Char_Chunk_Map map_b_pu1, map_b_pu2;
--|   Dynamic_Char_Chunk_Map map_d_pu1(5), map_d_pu2(5);
--|   Unbounded_Char_Chunk_Map map_u_pu1, map_u_pu2;
--|
--|   message("Starting map tests");
--|
--|   message("...Bounded Map");
--|   test(map_b_pu1, map_b_pu2);
--|
--|   message("...Dynamic Map");
--|   map_d_pu1.Preallocate(50);
--|   test(map_d_pu1, map_d_pu2);
--|
--|   message("...Unbounded Map");
--|   test(map_u_pu1, map_u_pu2);
--|
--|   message("...Map Active Iterator");
--|   message("   Bounded:");
--|   test_active_iterator(map_b_pu1);
--|   message("   Dynamic:");
--|   test_active_iterator(map_d_pu1);
--|   message("   Unbounded:");
--|   test_active_iterator(map_u_pu1);
--|
--|   message("...Map Passive Iterator");
--|   message("   Bounded:");
--|   test_passive_iterator(map_b_pu1);
--|   message("   Dynamic:");
--|   test_passive_iterator(map_d_pu1);
--|   message("   Unbounded:");
--|   test_passive_iterator(map_u_pu1);
--|
--|   assertion(map_b_pu1.IsBound('6'), "** M01: Map binding is not correct");
--|   assertion((map_b_pu2.Extent() == 0), "** M02: Map Extent is not correct");
--|   assertion(map_d_pu1.IsBound('6'), "** M03: Map binding is not correct");
--|   assertion((map_d_pu2.Extent() == 0), "** M04: Map Extent is not correct");
--|   assertion(map_u_pu1.IsBound('6'), "** M05: Map binding is not correct");
--|   assertion((map_u_pu2.Extent() == 0), "** M06: Map Extent is not correct");
--|
--| //  Bounded_Char_Chunk_Map map_b_pu3(map_b_pu1);
--|   Dynamic_Char_Chunk_Map map_d_pu3(map_d_pu1);
--|   Unbounded_Char_Chunk_Map map_u_pu3(map_u_pu1);
--|
--| //  assertion((map_b_pu1 == map_b_pu3), "** M07: Maps are not equal");
--|   assertion((map_d_pu1 == map_d_pu3), "** M08: Maps are not equal");
--|   assertion((map_u_pu1 == map_u_pu3), "** M09: Maps are not equal");
--|
--| //  map_b_pu3.Clear();
--|   map_d_pu3.Clear();
--|   map_u_pu3.Clear();
--|
--|   assertion((map_b_pu1.Available() == 297), "** M10: Available space is not correct");
--|   assertion((map_b_pu2.Available() == 300), "** M11: Available space is not correct");
--|
--|   message("Completed map tests");
--|
--|   return 0;
--|
--| }
--|
