-- The Ada 95 Booch Components (Version 1.0 beta 1)
-- Copyright (C)1994-1997 Grady Booch and David Weller.  All Rights Reserved.
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
--   This file contains tests for the list classes.
with Text_Io;
with Root_Container;
with Root_SList;
with Root_Dlist;
with Character_References;
procedure List_Test is
   use Text_Io;
   use Character_References;
   use Root_Dlist; use Root_Slist; use Root_Container;

   procedure Assertion ( Cond : Boolean; Message : String) is
   begin
      if not Cond then
     Put_Line(Message);
      end if;
   end Assertion;
   pragma Inline(Assertion);

   procedure Test_Single(L1, L2, T1, T2, T3 : in out Single_List) is
   begin
      assertion(Is_Null(l1), "** S01: List is not initially null");
      assertion((Length(l1) = 0), "** S02: List Length is not initially zero");
      assertion(not Is_Shared(l1), "** S03: List is initially shared");
      Append(l1, '3');
      Append(l1, '1', 1);
      Append(l1, '2', 2);
      assertion(not Is_Null(l1), "** S04: List is empty");
      assertion((Length(l1) = 3), "** S05: List Length is not correct");
      assertion((Head(l1) = '3'), "** S06: List Head is not correct");
      assertion(not Is_Shared(l1), "** S07: List is shared");
      Clear(l1);
      assertion(Is_Null(l1), "** S08: List is not null");
      assertion((Length(l1) = 0), "** S09: List Length is not zero");
      assertion(not Is_Shared(l1), "** S10: List is shared");
      Insert(l1, '4');
      Insert(l1, '5');
      Insert(l1, '6');
      assertion(not Is_Null(l1), "** S11: List is empty");
      assertion((Length(l1) = 3), "** S12: List Length is not correct");
      assertion((Foot(l1) = '4'), "** S13: List Head is not correct");
      assertion(not Is_Shared(l1), "** S14: List is shared");
      t1 := l1;
      assertion((l1 = t1), "** S15: Lists are not equal");
      assertion(Is_Shared(l1), "** S16: List is not shared");
      assertion(Is_Shared(t1), "** S17: List is not shared");
      Set_Head(t1, '7');
      assertion((l1 = t1), "** S18: Lists are not equal");
      assertion((Item_At(l1, 1) = '7'), "** S19: List Head is not correct");
      assertion((Head(t1) = '7'), "** S20: List Head is not correct");
      Insert(l1, '8');
      assertion((l1 /= t1), "** S21: Lists are equal");
      assertion((Length(l1) = 4), "** S22: List Length is not correct");
      assertion((Item_At(L1, 2) = '7'), "** S23: List Head is not correct");
      assertion((Length(t1) = 3), "** S24: List Length is not correct");
      assertion((Head(t1) = '7'), "** S25: List Head is not correct");
      t2 := t1;
      Tail(t2);
      assertion((t1 /= t2), "** S26: Lists are equal");
      assertion((Length(t2) = 2), "** S27: List Length is not correct");
      assertion((Head(t2) = '5'), "** S28: List Head is not correct");
      Clear(t1);
      assertion((Length(l1) = 4), "** S29: List Length is not correct");
      assertion((Head(l1) = '8'), "** S30: List Head is not correct");
      assertion(not Is_Shared(l1), "** S31: List is not shared");
      assertion(Is_Null(t1), "** S32: List is not null");
      assertion((Length(t1) = 0), "** S33: List Length is not correct");
      assertion(not Is_Shared(t1), "** S34: List is shared");
      assertion((Length(t2) = 2), "** S35: List Length is not correct");
      assertion((Head(t2) = '5'), "** S36: List Head is not correct");
      assertion(Is_Shared(t2), "** S37: List is not shared");
      Clear(l1);
      assertion(Is_Null(l1), "** S38: List is not null");
      assertion((Length(l1) = 0), "** S39: List Length is not correct");
      assertion(not Is_Shared(l1), "** S40: List is shared");
      assertion((Length(t2) = 2), "** S41: List Length is not correct");
      assertion((Head(t2) = '5'), "** S42: List Head is not correct");
      assertion(not Is_Shared(t2), "** S43: List is shared");
      Insert(l2, '1');
      Insert(l2, '3', 1);
      Insert(l2, '2', 2);
      assertion((l1 /= l2), "** S44: Lists are equal");
      assertion(not Is_Null(l2), "** S45: List is empty");
      assertion((Length(l2) = 3), "** S46: List Length is not correct");
      assertion((Head(l2) = '3'), "** S47: List Head is not correct");
      assertion(not Is_Shared(l2), "** S48: List is shared");
      l1 := t2;
      T3 := L2;
      l1 := T3;
      Tail(t3);
      assertion((l1 = l2), "** S49: Lists are not equal");
      assertion((l1 /= t2), "** S50: List are equal");
      assertion(Is_Shared(l1), "** S51: List is not shared");
      assertion(not Is_Shared(t2), "** S52: List is not shared");
      assertion(Is_Shared(t3), "** S53: List is not shared");
      Clear(l1);
      Swap_Tail(l2, t2);
      assertion(Is_Null(l1), "** S54: List is not null");
      assertion((Length(l2) = 3), "** S55: List Length is not correct");
      assertion(not Is_Shared(l2), "** S56: List is shared");
      assertion((t2 = t3), "** S55: Lists are not equal");
      assertion((Length(t3) = 2), "** S56: List Length is not correct");
      assertion((Head(t3) = '2'), "** S57: List Head is not correct");
      l1 := t2;
      t2 := l2;
      Tail(t2);
      assertion((l1 /= l2), "** S58: Lists are equal");
      assertion((l1 = t3), "** S59: Lists are not equal");
      assertion((Head(t2) = '5'), "** S60: List Head is not correct");
      assertion((Length(t2) = 2), "** S61: List Length is not correct");
      Set_Item(l1, '7', 1);
      assertion((Head(l1) = '7'), "** S62: List Head is not correct");
      Set_Item(l1, '7', 2);
      assertion((Head(l1) = '7'), "** S63: List Head is not correct");
      Set_Item(l1, '2', 1);
      Set_Item(l1, '1', 2);
      t1 := l1;
      t2 := l2;
      Clear(l1);
      Clear(l2);
      Insert(l1, '1');
      Insert(l1, '5');
      Insert(l2, '2');
      Insert(l2, '3');
      Insert(l2, '4');
      Insert(l1, l2, 2);
      Clear(l2);
      assertion((Item_At(l1, 1) = '5'), "** S64: List Item is not correct");
      assertion((Item_At(l1, 2) = '4'), "** S65: List Item is not correct");
      assertion((Item_At(l1, 3) = '3'), "** S66: List Item is not correct");
      assertion((Item_At(l1, 4) = '2'), "** S67: List Item is not correct");
      assertion((Item_At(l1, 5) = '1'), "** S68: List Item is not correct");
      Clear(t3);
      Append(l2, '7');
      Append(t3, '9');
      Append(t3, '8');
      Insert(l2, t3);
      Clear(t3);
      Append(t3, '2');
      Append(t3, '1');
      Append(l2, t3);
      Clear(t3);
      Append(t3, '6');
      Append(t3, '5');
      Append(t3, '4');
      Append(t3, '3');
      Append(l2, t3, 3);
      Clear(t3);
      assertion((Length(l2) = 9), "** S68: List Length is not correct");
      assertion((Item_At(l2, 1) = '9'), "** S70: List Item is not correct");
      assertion((Item_At(l2, 5) = '5'), "** S71: List Item is not correct");
      assertion((Item_At(l2, 7) = '3'), "** S71: List Item is not correct");
      assertion((Item_At(l2, 8) = '2'), "** S72: List Item is not correct");
      assertion((Item_At(l2, 9) = '1'), "** S74: List Item is not correct");
      Share_Foot(t3, l2);
      assertion((Head(t3) = '1'), "** S75: List Item is not correct");
      Share_Head(t3, l2);
      assertion((Head(t3) = '9'), "** S76: List Item is not correct");
      Share(t3, l2, 1);
      Clear(l2);
      assertion((Head(t3) = '9'), "** S77: List Item is not correct");
      l2 := t3;
      assertion((Head(l2) = '9'), "** S78: List Item is not correct");
      Clear(l1);
      l1 := l2;
      Remove(l1, 1);
      Remove(l1, 8);
      Remove(l1, 4);
      assertion((Length(l1) = 6), "** S79: List Length is not correct");
      assertion((Head(l1) = '8'), "** S80: List Item is not correct");
      assertion((Foot(l1) = '2'), "** S81: List Item is not correct");
      Share(t1, l1, 3);
      Share(t2, l1, 5);
      Purge(l1, 4);
      assertion((Length(l1) = 3), "** S82: List Length is not correct");
      assertion((Length(t1) = 1), "** S83: List Length is not correct");
      assertion((Length(t2) = 2), "** S84: List Length is not correct");
      assertion((Head(l1) = '8'), "** S85: List Item is not correct");
      assertion((Head(t1) = '6'), "** S86: List Item is not correct");
      assertion((Head(t2) = '3'), "** S87: List Item is not correct");
      Append(l1, '5');
      Append(l1, '4');
      Append(l1, '3');
      Append(l1, '2');
      Append(l1, '1');
      Share(t1, l1, 3);
      Share(t2, l1, 5);
      Purge(l1, 4, 3);
      assertion((Length(l1) = 5), "** S88: List Length is not correct");
      assertion((Length(t1) = 3), "** S89: List Length is not correct");
      assertion((Length(t2) = 2), "** S90: List Length is not correct");
      assertion((Head(l1) = '8'), "** S91: List Item is not correct");
      assertion((Item_At(t1, 2) = '2'), "** S92: List Item is not correct");
      assertion((Head(t2) = '4'), "** S93: List Item is not correct");
      Preserve(l1, 1);
      assertion((Length(l1) = 5), "** S94: List Length is not correct");
      Preserve(l1, 2);
      assertion((Length(l1) = 4), "** S95: List Length is not correct");
      Append(l1, '3');
      Append(l1, '4');
      Append(l1, '5');
      Preserve(l1, 3, 4);
      assertion((Length(l1) = 4), "** S96: List Length is not correct");
      assertion((Length(t1) = 5), "** S97: List Length is not correct");
   end Test_Single;

   procedure Test_Single (L1, L2 : in out Single_List) is 
      T1, T2, T3 : Single_List;
   begin
      Test_Single(L1, L2, T1, T2, T3);
   end Test_Single;

   procedure Test_Double(L1, L2, T1, T2, T3 : in out Double_List) is
   begin
      assertion(Is_Null(l1), "** D01: List is not initially null");
      assertion((Length(l1) = 0), "** D02: List Length is not initially zero");
      assertion(not Is_Shared(l1), "** D03: List is initially shared");
      Append(l1, '3');
      Append(l1, '1', 1);
      Append(l1, '2', 2);
      assertion(not Is_Null(l1), "** D04: List is empty");
      assertion((Length(l1) = 3), "** D05: List Length is not correct");
      assertion((Head(l1) = '3'), "** D06: List Head is not correct");
      assertion(not Is_Shared(l1), "** D07: List is shared");
      Clear(l1);
      assertion(Is_Null(l1), "** D08: List is not null");
      assertion((Length(l1) = 0), "** D09: List Length is not zero");
      assertion(not Is_Shared(l1), "** D10: List is shared");
      Insert(l1, '4');
      Insert(l1, '5');
      Insert(l1, '6');
      assertion(not Is_Null(l1), "** D11: List is empty");
      assertion((Length(l1) = 3), "** D12: List Length is not correct");
      assertion((Foot(l1) = '4'), "** D13: List Head is not correct");
      assertion(not Is_Shared(l1), "** D14: List is shared");
      t1 := l1;
      assertion((l1 = t1), "** D15: Lists are not equal");
      assertion(Is_Shared(l1), "** D16: List is not shared");
      assertion(Is_Shared(t1), "** D17: List is not shared");
      Set_Head(t1, '7');
      assertion((l1 = t1), "** D18: Lists are not equal");
      assertion((Item_At(l1, 1) = '7'), "** D19: List Head is not correct");
      assertion((Head(t1) = '7'), "** D20: List Head is not correct");
      Insert(l1, '8');
      assertion((l1 /= t1), "** D21: Lists are equal");
      assertion((Length(l1) = 4), "** D22: List Length is not correct");
      assertion((Item_At(l1, 2) = '7'), "** D23: List Head is not correct");
      assertion((Length(t1) = 3), "** D24: List Length is not correct");
      assertion((Head(t1) = '7'), "** D25: List Head is not correct");
      t2 := t1;
      Tail(t2);
      assertion((t1 /= t2), "** D26: Lists are equal");
      assertion((Length(t2) = 2), "** D27: List Length is not correct");
      assertion((Head(t2) = '5'), "** D28: List Head is not correct");
      Clear(t1);
      assertion((Length(l1) = 4), "** D29: List Length is not correct");
      assertion((Head(l1) = '8'), "** D30: List Head is not correct");
      assertion(not Is_Shared(l1), "** D31: List is not shared");
      assertion(Is_Null(t1), "** D32: List is not null");
      assertion((Length(t1) = 0), "** D33: List Length is not correct");
      assertion(not Is_Shared(t1), "** D34: List is shared");
      assertion((Length(t2) = 2), "** D35: List Length is not correct");
      assertion((Head(t2) = '5'), "** D36: List Head is not correct");
      assertion(Is_Shared(t2), "** D37: List is not shared");
      Clear(l1);
      assertion(Is_Null(l1), "** D38: List is not null");
      assertion((Length(l1) = 0), "** D39: List Length is not correct");
      assertion(not Is_Shared(l1), "** D40: List is shared");
      assertion((Length(t2) = 2), "** D41: List Length is not correct");
      assertion((Head(t2) = '5'), "** D42: List Head is not correct");
      assertion(not Is_Shared(t2), "** D43: List is shared");
      Insert(l2, '1');
      Insert(l2, '3', 1);
      Insert(l2, '2', 2);
      assertion((l1 /= l2), "** D44: Lists are equal");
      assertion(not Is_Null(l2), "** D45: List is empty");
      assertion((Length(l2) = 3), "** D46: List Length is not correct");
      assertion((Head(l2) = '3'), "** D47: List Head is not correct");
      assertion(not Is_Shared(l2), "** D48: List is shared");
      l1 := t2;
      T3 := L2;
      l1 := T3;
      Tail(t3);
      assertion((l1 = l2), "** D49: Lists are not equal");
      assertion((l1 /= t2), "** D50: List are equal");
      assertion(Is_Shared(l1), "** D51: List is not shared");
      assertion(not Is_Shared(t2), "** D52: List is not shared");
      assertion(Is_Shared(t3), "** D53: List is not shared");
      Clear(l1);
      Swap_Tail(l2, t2);
      assertion(Is_Null(l1), "** D54: List is not null");
      assertion((Length(l2) = 3), "** D55: List Length is not correct");
      assertion(not Is_Shared(l2), "** D56: List is shared");
      assertion((t2 = t3), "** D55a: Lists are not equal");
      assertion((Length(t3) = 2), "** D56a: List Length is not correct");
      assertion((Head(t3) = '2'), "** D57: List Head is not correct");
      l1 := t2;
      t2 := l2;
      Tail(t2);
      assertion((l1 /= l2), "** D58: Lists are equal");
      assertion((l1 = t3), "** D59: Lists are not equal");
      assertion((Head(t2) = '5'), "** D60: List Head is not correct");
      assertion((Length(t2) = 2), "** D61: List Length is not correct");
      Set_Item(l1, '7', 1);
      assertion((Head(l1) = '7'), "** D62: List Head is not correct");
      Set_Item(l1, '7', 2);
      assertion((Head(l1) = '7'), "** D63: List Head is not correct");
      Set_Item(l1, '2', 1);
      Set_Item(l1, '1', 2);
      t1 := l1;
      t2 := l2;
      Clear(l1);
      Clear(l2);
      Insert(l1, '1');
      Insert(l1, '5');
      Insert(l2, '2');
      Insert(l2, '3');
      Insert(l2, '4');
      Insert(l1, l2, 2);
      Clear(l2);
      assertion((Item_At(l1, 1) = '5'), "** D64: List Item is not correct");
      assertion((Item_At(l1, 2) = '4'), "** D65: List Item is not correct");
      assertion((Item_At(l1, 3) = '3'), "** D66: List Item is not correct");
      assertion((Item_At(l1, 4) = '2'), "** D67: List Item is not correct");
      assertion((Item_At(l1, 5) = '1'), "** D68: List Item is not correct");
      Clear(t3);
      Append(l2, '7');
      Append(t3, '9');
      Append(t3, '8');
      Insert(l2, t3);
      Clear(t3);
      Append(t3, '2');
      Append(t3, '1');
      Append(l2, t3);
      Clear(t3);
      Append(t3, '6');
      Append(t3, '5');
      Append(t3, '4');
      Append(t3, '3');
      Append(l2, t3, 3);
      Clear(t3);
      assertion((Length(l2) = 9), "** D69: List Length is not correct");
      assertion((Item_At(l2, 1) = '9'), "** D70: List Item is not correct");
      assertion((Item_At(l2, 5) = '5'), "** D71: List Item is not correct");
      assertion((Item_At(l2, 7) = '3'), "** D71: List Item is not correct");
      assertion((Item_At(l2, 8) = '2'), "** D72: List Item is not correct");
      assertion((Item_At(l2, 9) = '1'), "** D74: List Item is not correct");
      Share_Foot(t3, l2);
      assertion((Head(t3) = '1'), "** D75: List Item is not correct");
      Share_Head(t3, l2);
      assertion((Head(t3) = '9'), "** D76: List Item is not correct");
      Share(t3, l2, 1);
      Clear(l2);
      assertion((Head(t3) = '9'), "** D77: List Item is not correct");
      l2 := t3;
      assertion((Head(l2) = '9'), "** D78: List Item is not correct");
      Clear(l1);
      l1 := l2;
      Remove(l1, 1);
      Remove(l1, 8);
      Remove(l1, 4);
      assertion((Length(l1) = 6), "** D79: List Length is not correct");
      assertion((Head(l1) = '8'), "** D80: List Item is not correct");
      assertion((Foot(l1) = '2'), "** D81: List Item is not correct");
      Share(t1, l1, 3);
      Share(t2, l1, 5);
      Purge(l1, 4);
      assertion((Length(l1) = 3), "** D82: List Length is not correct");
      assertion((Length(t1) = 1), "** D83: List Length is not correct");
      assertion((Length(t2) = 2), "** D84: List Length is not correct");
      assertion((Head(l1) = '8'), "** D85: List Item is not correct");
      assertion((Head(t1) = '6'), "** D86: List Item is not correct");
      assertion((Head(t2) = '3'), "** D87: List Item is not correct");
      Append(l1, '5');
      Append(l1, '4');
      Append(l1, '3');
      Append(l1, '2');
      Append(l1, '1');
      Share(t1, l1, 3);
      Share(t2, l1, 5);
      Purge(l1, 4, 3);
      assertion((Length(l1) = 5), "** D88: List Length is not correct");
      assertion((Length(t1) = 3), "** D89: List Length is not correct");
      assertion((Length(t2) = 2), "** D90: List Length is not correct");
      assertion((Head(l1) = '8'), "** D91: List Item is not correct");
      assertion((Item_At(t1, 2) = '2'), "** D92: List Item is not correct");
      assertion((Head(t2) = '4'), "** D93: List Item is not correct");
      Preserve(l1, 1);
      assertion((Length(l1) = 5), "** D94: List Length is not correct");
      Preserve(l1, 2);
      assertion((Length(l1) = 4), "** D95: List Length is not correct");
      Append(l1, '3');
      Append(l1, '4');
      Append(l1, '5');
      Preserve(l1, 3, 4);
      assertion((Length(l1) = 4), "** D96: List Length is not correct");
      assertion((Length(t1) = 5), "** D97: List Length is not correct");
      Share(t1, l1, 2);
      assertion(Is_Head(l1), "** D98: List is Head is not correct");
      assertion(not Is_Head(t1), "** D99: List is Head is not correct");
      Predecessor(t1);
      assertion(Is_Head(t1), "** D100: List is Head is not correct");
      Predecessor(t1);
      assertion(Is_Null(t1), "** D101: List is not null");
   end Test_Double;

   procedure Test_Double (L1, L2 : in out Double_List) is 
      T1, T2, T3 : Double_List;
   begin
      Test_Double(L1, L2, T1, T2, T3);
   end Test_Double;

   procedure Process (C : in Char_Ptr; OK : out Boolean) is
      use Text_IO;
   begin
      Put_Line("Item: " & C.all );
      Ok := True;
   end Process;

   function mod_op is new Modify(Apply=> Process);

    procedure Test_Active_Iterator (L : access Container'Class) is
       Iter : Iterator(L);
       Success : Boolean;
       Temp : Char_Ptr;
    begin
       while not Is_Done(Iter) loop
	  Temp := Current_Item(Iter);
          Process(Temp, Success);
          Next(Iter);
       end loop;
    end Test_Active_Iterator;

    procedure Test_Passive_Iterator (L : access Container'Class) is
       PIter : aliased Passive_Iterator(L);
       Success : Boolean;
    begin
       Success := Mod_op(PIter'access); -- just discard Success for now..
    end Test_Passive_Iterator;

    slist_p1, Slist_P2 : aliased Single_List;

    dlist_p1, Dlist_P2 : aliased Double_List;

begin
  Put_line("Starting list tests");

  Put_line("...Single List");
  test_single(slist_p1, slist_p2);

  Put_line("...Double List");
  test_double(dlist_p1, dlist_p2);

  Put_line("...List Active Iterator");
  Put_line("   Single");
  test_active_iterator(Slist_P1'access);
  Put_line("   Double");
  test_active_iterator(Dlist_P1'access);

  Put_line("...List Passive Iterator");
  Put_line("   Single");
  test_passive_iterator(Slist_P1'access);
  Put_line("   Double");
  test_passive_iterator(Dlist_P1'access);

  Put_line("Completed list tests");
end List_Test;

