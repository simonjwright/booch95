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

with Text_Io;
with Root_Container;
with Root_Trees;
with Root_Trees;
with Root_Bin_Trees;
with Character_References;

procedure Tree_Test is
  use Text_IO;
  use Character_References;

  procedure Process (C : in Char_Ptr; OK : out Boolean) is
    use Text_IO;
  begin
    Put_Line ("Item: " & C.all );
    Ok := True;
  end Process;

  function Mod_Op is new Root_Container.Modify (Apply=> Process);

  procedure Assertion (Cond : Boolean; Message : String) is
  begin
    if not Cond then
      Put_Line (Message);
    end if;
  end Assertion;
  pragma Inline (Assertion);

  procedure Test_Active_Iterator
     (L : access Root_Container.Container'Class) is
    Iter : Root_Container.Iterator (L);
    Success : Boolean;
    Temp : Char_Ptr;
  begin
    while not Root_Container.Is_Done (Iter) loop
      Temp := Root_Container.Current_Item (Iter);
      Process (Temp, Success);
      Root_Container.Next (Iter);
    end loop;
  end Test_Active_Iterator;

  procedure Test_Passive_Iterator
     (L : access Root_Container.Container'Class) is
    PIter : aliased Root_Container.Passive_Iterator (L);
    Success : Boolean;
  begin
    Success := Mod_Op (PIter'access); -- just discard Success for now..
  end Test_Passive_Iterator;

  procedure Print_Tree (T : Root_Bin_Trees.Binary_Tree;
                        Message : String := "";
                        Depth : Natural := 0) is
    use Root_Bin_Trees;
    procedure Indent (To : Natural := Depth) is
    begin
      for N in 1 .. Integer (To) loop
        Put ("  ");
      end loop;
    end Indent;
  begin
    if Depth = 0 then
      Put ("Binary tree: " & Message & " ");
    end if;
    if Is_Null (T) then
      Put_Line ("(null)");
    else
      if Is_Root (T) then
        Put ("(root) ");
      end if;
      Put_Line (":= " & Item_At (T));
      if (Has_Children (T)) then
        declare
          L : Binary_Tree := T;
        begin
          Left_Child (L);
          Indent;
          Put ("L ");
          Print_Tree (L, Depth => Depth + 1);
        end;
        declare
          R : Binary_Tree := T;
        begin
          Right_Child (R);
          Indent;
          Put ("R ");
          Print_Tree (R, Depth => Depth + 1);
        end;
      end if;
    end if;
  end Print_Tree;

  procedure Test_Primitive (T1, T2 : in out Root_Bin_Trees.Binary_Tree) is
    use Root_Bin_Trees;
    Tt1, Tt2, Tt3 : Binary_Tree;
  begin
    Clear (T1);
    Assertion (Is_Null (T1), "** B01: Tree is not initially null");
    Assertion (not Is_Shared(T1), "** B02: Tree is initially shared");
    Insert (T1, '1', Left);
    Insert (T1, '2', Right);
    Insert (T1, '3', Left);
    Insert (T1, '4', Right);
    Assertion (not Is_Null (T1), "** B03: Tree is empty");
    Assertion (Item_At (T1) = '4', "** B04: Tree item is not correct");
    Assertion (not Is_Shared (T1), "** B05: Tree is shared");
    Right_Child (T1);
    Assertion (Item_At (T1) = '3', "** B06: Tree item is not correct");
    Left_Child (T1);
    Assertion (Item_At (T1) = '2', "** B07: Tree item is not correct");
    Right_Child (T1);
    Assertion (Item_At (T1) = '1', "** B08: Tree item is not correct");
    Left_Child (T1);
    Assertion (Is_Null (T1), "** B09: Tree is not empty");
    Append (T1, '4', Left, Left);
    Append (T1, '1', Left, Left);
    Append (T1, '2', Left, Left);
    Append (T1, '3', Right, Left);
    Tt1 := T1;
    Tt2 := T1;
    Clear (Tt1);
    -- I know the next test number is out-of-order; this is a copy of the
    -- C++ test
    Assertion (Item_At (T1) = '4', "** B09: Tree item is not correct");
    Child (T1, Left);
    Assertion (Item_At (T1) = '3', "** B10: Tree item is not correct");
    Child (T1, Right);
    Assertion (Item_At (T1) = '2', "** B11: Tree item is not correct");
    Child (T1, Left);
    Assertion (Item_At (T1) = '1', "** B12: Tree item is not correct");
    Child (T1, Right);
    Assertion (Is_Null (T1), "** B13: Tree is not empty");
    Assertion (Is_Null (Tt1), "** B14: Tree is not empty");
    Assertion (not Is_Null (Tt2), "** B15: Tree is empty");
    T2 := Tt2;
    Share (Tt3, T2, Left);
    Share (Tt3, Tt3, Right);
    Share (Tt3, Tt3, Left);
    Assertion (T2 = Tt2, "** B16: Trees are not equal");
    Assertion (T2 /= Tt3, "** B17: Trees are equal");
    Assertion (Item_At (T2) = '4', "** B18: Tree item is not correct");
    Assertion (Item_At (Tt3) = '1', "** B19: Tree item is not correct");
    Assertion (Has_Children (T2), "** B20: Tree has no children");
    Assertion (not Has_Children (Tt3), "** B21: Tree has children");
    Assertion (Is_Root (T2), "** B22: Tree is not root");
    Assertion (not Is_Root (Tt3), "** B23: Tree is root");
    Clear (Tt2);
    T1 := Tt3;
    Clear (Tt3);
    Parent (T1);
    Assertion (Item_At (T1) = '2', "** B24: Tree item is not correct");
    Parent (T1);
    Assertion (Item_At (T1) = '3', "** B25: Tree item is not correct");
    Parent (T1);
    Assertion (Item_At (T1) = '4', "** B26: Tree item is not correct");
    Set_Item (T1, '7');
    Parent (T1);
    Assertion (Is_Null (T1), "** B27: Tree is not null");
    Assertion (Item_At (T2) = '7', "** B27: Tree item is not correct");
    Insert (T1, '5', Left);
    Append (T1, '6', Left, Left);
    Append (T1, '7', Right, Right);
    Tt2 := T1;
    Assertion (Item_At (Tt2) = '5', "** B28: Tree item is not correct");
    Child (Tt2, Left);
    Assertion (Item_At (Tt2) = '6', "** B29: Tree item is not correct");
    Remove (T1, Left);
    Assertion (Item_At (Tt2) = '6', "** B30: Tree item is not correct");
    Tt2 := T1;
    Child (Tt2, Right);
    Assertion (Item_At (Tt2) = '7', "** B31: Tree item is not correct");
    Remove (T1, Right);
    Assertion (Item_At (Tt2) = '7',  "** B32: Tree item is not correct");
    Clear (Tt2);
    Append (T1, '6', Left, Left);
    Append (T1, '7', Right, Right);
    Swap_Child (T1, T2, Left);
    Tt1 := T1;
    Child (Tt1, Left);
    Assertion (Item_At (T2) = '6', "** B33: Tree item is not correct");
    Assertion (not Has_Children (T2), "** B34: Tree has children");
    Assertion (Is_Root (T2), "** B35: Tree is not root");
    Assertion (Item_At (Tt1) = '7', "** B36: Tree item is not correct");
    Assertion (Has_Children (Tt1), "** B37: Tree has no children");
    Assertion (not Is_Root (Tt1), "** B38: Tree is not root");
    Parent (Tt1);
    Assertion (T1 = Tt1, "** B39: Trees are not equal");
    Child (Tt1, Left);
    Assertion (Item_At (Tt1) = '7', "** B40: Tree item is not correct");
  end Test_Primitive;

  B_Tree_P1, B_Tree_P2 : aliased Root_Bin_Trees.Binary_Tree;

begin

  Put_Line ("Starting Tree tests");

  Put_Line ("...Binary Tree");
  Test_Primitive (B_Tree_P1, B_Tree_P2);

  Put_Line ("Completed Tree tests");

-- //  The C++ Booch Components (Version 2.3)
-- //  (C) Copyright 1990-1994 Grady Booch. All Rights Reserved.
-- //
-- //  TreeT.cpp
-- //
-- //  This file contains tests for the tree classes.

-- #include "Items.h"
-- #include "BCStoreM.h"
-- #include "BCBTree.h"
-- #include "BCMTree.h"
-- #include "BCATree.h"

-- #if __BCPLUSPLUS__ || THINK_CPLUS
-- #include "BCExcept.cpp"
-- #include "BCPool.cpp"
-- #include "BCNodes.cpp"
-- #include "BCBTree.cpp"
-- #include "BCMTree.cpp"
-- #include "BCATree.cpp"
-- #endif

-- BC_CPool memory_pool(1024);
-- BC_CPool& BC_CManaged::fPool = memory_pool;

-- typedef BC_TBinaryNode<Char, BC_CManaged> Binary_Node;

-- typedef BC_TMultiwayNode<Char, BC_CManaged> Multiway_Node;

-- typedef BC_TAVLNode<Char, BC_CManaged> AVL_Node;

-- typedef BC_TBinaryTree<Char, BC_CManaged> Binary_Tree;

-- typedef BC_TMultiwayTree<Char, BC_CManaged> Multiway_Tree;

-- typedef BC_TAVLTree<Char, BC_CManaged> AVL_Tree;

-- void test_binary (BC_TBinaryTree<Char, BC_CManaged>& t1, BC_TBinaryTree<Char, BC_CManaged>& t2,
--                   BC_TBinaryTree<Char, BC_CManaged>& tt1, BC_TBinaryTree<Char, BC_CManaged>& tt2,
--                   BC_TBinaryTree<Char, BC_CManaged>& tt3)
-- {
--   assertion(t1.IsNull(), "** B01: Tree is not initially null");
--   assertion(!t1.IsShared(), "** B02: Tree is initially shared");
--   t1.Insert('1', BC_kLeft);
--   t1.Insert('2', BC_kRight);
--   t1.Insert('3', BC_kLeft);
--   t1.Insert('4', BC_kRight);
--   assertion(!t1.IsNull(), "** B03: Tree is empty");
--   assertion((t1.ItemAt() == '4'), "** B04: Tree item is not correct");
--   assertion(!t1.IsShared(), "** B05: Tree is shared");
--   t1.RightChild();
--   assertion((t1.ItemAt() == '3'), "** B06: Tree item is not correct");
--   t1.LeftChild();
--   assertion((t1.ItemAt() == '2'), "** B07: Tree item is not correct");
--   t1.RightChild();
--   assertion((t1.ItemAt() == '1'), "** B08: Tree item is not correct");
--   t1.LeftChild();
--   assertion(t1.IsNull(), "** B09: Tree is not empty");
--   t1.Append('4', BC_kLeft, BC_kLeft);
--   t1.Append('1', BC_kLeft, BC_kLeft);
--   t1.Append('2', BC_kLeft, BC_kLeft);
--   t1.Append('3', BC_kRight, BC_kLeft);
--   tt1 = t1;
--   tt2 = t1;
--   tt1.Clear();
--   assertion((t1.ItemAt() == '4'), "** B09: Tree item is not correct");
--   t1.Child(BC_kLeft);
--   assertion((t1.ItemAt() == '3'), "** B10: Tree item is not correct");
--   t1.Child(BC_kRight);
--   assertion((t1.ItemAt() == '2'), "** B11: Tree item is not correct");
--   t1.Child(BC_kLeft);
--   assertion((t1.ItemAt() == '1'), "** B12: Tree item is not correct");
--   t1.Child(BC_kRight);
--   assertion(t1.IsNull(), "** B13: Tree is not empty");
--   assertion(tt1.IsNull(), "** B14: Tree is not empty");
--   assertion(!tt2.IsNull(), "** B15: Tree is empty");
--   t2 = tt2;
--   tt3.Share(t2, BC_kLeft);
--   tt3.Share(tt3, BC_kRight);
--   tt3.Share(tt3, BC_kLeft);
--   assertion((t2 == tt2), "** B16: Trees are not equal");
--   assertion((t2 != tt3), "** B17: Trees are equal");
--   assertion((t2.ItemAt() == '4'), "** B18: Tree item is not correct");
--   assertion((tt3.ItemAt() == '1'), "** B19: Tree item is not correct");
--   assertion(t2.HasChildren(), "** B20: Tree has no children");
--   assertion(!tt3.HasChildren(), "** B21: Tree has children");
--   assertion(t2.IsRoot(), "** B22: Tree is not root");
--   assertion(!tt3.IsRoot(), "** B23: Tree is root");
--   tt2.Clear();
--   t1 = tt3;
--   tt3.Clear();
--   t1.Parent();
--   assertion((t1.ItemAt() == '2'), "** B24: Tree item is not correct");
--   t1.Parent();
--   assertion((t1.ItemAt() == '3'), "** B25: Tree item is not correct");
--   t1.Parent();
--   assertion((t1.ItemAt() == '4'), "** B26: Tree item is not correct");
--   t1.SetItem('7');
--   t1.Parent();
--   assertion(t1.IsNull(), "** B27: Tree is not null");
--   assertion((t2.ItemAt() == '7'), "** B27: Tree item is not correct");
--   t1.Insert('5', BC_kLeft);
--   t1.Append('6', BC_kLeft, BC_kLeft);
--   t1.Append('7', BC_kRight, BC_kRight);
--   tt2 = t1;
--   assertion((tt2.ItemAt() == '5'), "** B28: Tree item is not correct");
--   tt2.Child(BC_kLeft);
--   assertion((tt2.ItemAt() == '6'), "** B29: Tree item is not correct");
--   t1.Remove(BC_kLeft);
--   assertion((tt2.ItemAt() == '6'), "** B30: Tree item is not correct");
--   tt2 = t1;
--   tt2.Child(BC_kRight);
--   assertion((tt2.ItemAt() == '7'), "** B31: Tree item is not correct");
--   t1.Remove(BC_kRight);
--   assertion((tt2.ItemAt() == '7'), "** B32: Tree item is not correct");
--   tt2.Clear();
--   t1.Append('6', BC_kLeft, BC_kLeft);
--   t1.Append('7', BC_kRight, BC_kRight);
--   t1.SwapChild(t2, BC_kLeft);
--   tt1 = t1;
--   tt1.Child(BC_kLeft);
--   assertion((t2.ItemAt() == '6'), "** B33: Tree item is not correct");
--   assertion(!t2.HasChildren(), "** B34: Tree has children");
--   assertion(t2.IsRoot(), "** B35: Tree is not root");
--   assertion((tt1.ItemAt() == '7'), "** B36: Tree item is not correct");
--   assertion(tt1.HasChildren(), "** B37: Tree has no children");
--   assertion(!tt1.IsRoot(), "** B38: Tree is not root");
--   tt1.Parent();
--   assertion((t1 == tt1), "** B39: Trees are not equal");
--   tt1.Child(BC_kLeft);
--   assertion((tt1.ItemAt() == '7'), "** B40: Tree item is not correct");
-- }

-- void test_binary (BC_TBinaryTree<Char, BC_CManaged>& t1, BC_TBinaryTree<Char, BC_CManaged>& t2)
-- {
--   BC_TBinaryTree<Char, BC_CManaged> tt1, tt2, tt3;
--   test_binary(t1, t2, tt1, tt2, tt3);
-- }

-- void test_multiway (BC_TMultiwayTree<Char, BC_CManaged>& t1, BC_TMultiwayTree<Char, BC_CManaged>& t2,
--                     BC_TMultiwayTree<Char, BC_CManaged>& tt1, BC_TMultiwayTree<Char, BC_CManaged>& tt2,
--                     BC_TMultiwayTree<Char, BC_CManaged>& tt3)
-- {
--   assertion(t1.IsNull(), "** M01: Tree is not initially null");
--   assertion(!t1.IsShared(), "** M02: Tree is initially shared");
--   t1.Insert('1');
--   t1.Insert('2');
--   t1.Insert('3');
--   t1.Insert('4');
--   assertion(!t1.IsNull(), "** M03: Tree is empty");
--   assertion((t1.ItemAt() == '4'), "** M04: Tree item is not correct");
--   assertion(!t1.IsShared(), "** M05: Tree is shared");
--   assertion((t1.Arity() == 1), "** M06: Tree arity is not correct");
--   t1.Child(0);
--   assertion((t1.ItemAt() == '3'), "** M07: Tree item is not correct");
--   assertion((t1.Arity() == 1), "** M08: Tree arity is not correct");
--   t1.Child(0);
--   assertion((t1.ItemAt() == '2'), "** M09: Tree item is not correct");
--   assertion((t1.Arity() == 1), "** M10: Tree arity is not correct");
--   t1.Child(0);
--   assertion((t1.ItemAt() == '1'), "** M11: Tree item is not correct");
--   assertion((t1.Arity() == 0), "** M12: Tree arity is not correct");
--   t1.Clear();
--   assertion(t1.IsNull(), "** M13: Tree is not empty");
--   t1.Append('6');
--   t1.Append('2');
--   t1.Append('3');
--   t1.Append('5');
--   tt1 = t1;
--   tt2 = t1;
--   tt2.Clear();
--   assertion((t1.ItemAt() == '6'), "** M14: Tree item is not correct");
--   assertion((t1.Arity() == 3), "** M15: Tree arity is not correct");
--   t1.Child(0);
--   assertion((t1.ItemAt() == '5'), "** M16: Tree item is not correct");
--   t1.Parent();
--   assertion((t1.ItemAt() == '6'), "** M17: Tree item is not correct");
--   t1.Child(1);
--   assertion((t1.ItemAt() == '3'), "** M18: Tree item is not correct");
--   t1.Parent();
--   assertion((t1.ItemAt() == '6'), "** M19: Tree item is not correct");
--   t1.Child(2);
--   assertion((t1.ItemAt() == '2'), "** M20: Tree item is not correct");
--   t2 = tt1;
--   t2.Append('4', 0);
--   t2.Append('1', 3);
--   t1.Parent();
--   t1.Child(1);
--   assertion((t1.ItemAt() == '4'), "** M21: Tree item is not correct");
--   t1.Parent();
--   t1.Child(4);
--   assertion((t1.ItemAt() == '1'), "** M22: Tree item is not correct");
--   assertion(!t1.IsRoot(), "** M23: Tree is root");
--   assertion(!t1.HasChildren(), "** M24: Tree has children");
--   t1.Parent();
--   t2.Clear();
--   assertion((tt1 == t1), "** M25: Trees are not equal");
--   tt1.SetItem('7');
--   assertion((t1.ItemAt() == '7'), "** M26: Tree item is not correct");
--   tt1.Clear();
--   assertion((tt1 != t1), "** M27: Trees are equal");
--   assertion(t1.IsRoot(), "** M28: Tree is root");
--   assertion(t1.HasChildren(), "** M29: Tree has children");
--   t2.Share(t1, 2);
--   assertion((t2.ItemAt() == '3'), "** M30: Tree item is not correct");
--   tt1.Share(t1, 0);
--   assertion((tt1.ItemAt() == '5'), "** M31: Tree item is not correct");
--   tt2.Share(t1, 4);
--   assertion((tt2.ItemAt() == '1'), "** M32: Tree item is not correct");
--   t2.Append('8');
--   t2.Append('9');
--   tt3.Insert('1');
--   tt3.Insert('2');
--   tt3.Insert('3');
--   t1.SwapChild(tt3, 0);
--   assertion((tt1 == tt3), "** M33: Trees are not equal");
--   tt1 = t1;
--   tt1.Child(0);
--   assertion((tt1.ItemAt() == '3'), "** M34: Tree item is not correct");
--   assertion(tt1.HasChildren(), "** M35: Tree has no children");
--   assertion(!tt3.HasChildren(), "** M36: Tree has children");
--   assertion((tt3.ItemAt() == '5'), "** M37: Tree item is not correct");
--   tt3.Clear();
--   t1.Remove(1);
--   tt3 = t1;
--   tt3.Child(1);
--   assertion((tt3 == t2), "** M38: Trees are not equal");
--   tt3.Clear();
--   assertion(!tt1.IsRoot(), "** M39: Tree is root");
--   t1.Remove(0);
--   assertion(tt1.IsRoot(), "** M40: Tree is root");
--   assertion((t1.Arity() == 3), "** M41: Tree arity is not correct");
--   tt3 = t1;
--   tt3.Child(0);
--   assertion((tt3 == t2), "** M42: Trees are not equal");
--   tt3.Clear();
--   t1.Clear();
--   assertion(t1.IsNull(), "** M43: Tree is not null");
--   assertion(t2.IsRoot(), "** M44: Tree is not root");
--   assertion((t2.ItemAt() == '3'), "** M45: Tree item is not correct");
--   assertion(tt2.IsRoot(), "** M46: Tree is not root");
--   assertion((tt2.ItemAt() == '1'), "** M47: Tree item is not correct");
-- }

-- void test_multiway (BC_TMultiwayTree<Char, BC_CManaged>& t1, BC_TMultiwayTree<Char, BC_CManaged>& t2)
-- {
--   BC_TMultiwayTree<Char, BC_CManaged> tt1, tt2, tt3;
--   test_multiway(t1, t2, tt1, tt2, tt3);
-- }

-- BC_Boolean process(const Char& item)
-- {
--   cout << "      Item: " << item << "\n";
--   return 1;
-- }

-- void test_avl (BC_TAVLTree<Char, BC_CManaged>& t)
-- {
--   assertion(t.IsNull(), "** A01: Tree is not null");
--   assertion(t.Insert('4'), "** A02: Tree insertion not correct");
--   assertion(t.Insert('5'), "** A03: Tree insertion not correct");
--   assertion(t.Insert('7'), "** A04: Tree insertion not correct");
--   assertion(t.Insert('2'), "** A05: Tree insertion not correct");
--   assertion(t.Insert('1'), "** A06: Tree insertion not correct");
--   assertion(t.Insert('3'), "** A07: Tree insertion not correct");
--   assertion(t.Insert('6'), "** A08: Tree insertion not correct");
--   assertion(t.IsMember('3'), "** A09: Tree membership is not correct");
--   assertion(t.IsMember('7'), "** A10: Tree membership is not correct");
--   assertion(!t.IsMember('g'), "** A11: Tree membership is not correct");
--   assertion(!t.IsNull(), "** A12: Tree is not null");
--   assertion(t.Insert('8'), "** A13: Tree insertion not correct");
--   assertion(t.Insert('9'), "** A14: Tree insertion not correct");
--   assertion(t.Insert('A'), "** A15: Tree insertion not correct");
--   assertion(t.Insert('B'), "** A16: Tree insertion not correct");
--   t.Traverse(process);
--   assertion(t.Delete('3'), "** A17: Tree deletion is not correct");
--   assertion(!t.Delete('g'), "** A18: Tree deletion is not correct");
--   assertion(t.Delete('5'), "** A19: Tree deletion is not correct");
--   assertion(t.Delete('A'), "** A20: Tree deletion is not correct");
--   assertion(!t.Delete('A'), "** A21: Tree deletion is not correct");
--   assertion(t.Delete('8'), "** A22: Tree deletion is not correct");
--   assertion(t.Delete('4'), "** A23: Tree deletion is not correct");
--   assertion(t.Delete('9'), "** A24: Tree deletion is not correct");
--   assertion(t.Delete('2'), "** A25: Tree deletion is not correct");
--   assertion(t.Delete('6'), "** A26: Tree deletion is not correct");
--   assertion(t.Delete('1'), "** A27: Tree deletion is not correct");
--   assertion(t.Delete('B'), "** A28: Tree deletion is not correct");
--   assertion(t.Delete('7'), "** A29: Tree deletion is not correct");
--   assertion(t.IsNull(), "** A30: Tree is not null");
--   t.Insert('4');
--   t.Insert('5');
--   t.Insert('7');
--   t.Insert('2');
--   assertion((t.Extent() == 4), "** A31: Tree extent is not correct");
--   assertion(t.Delete('4'), "** A32: Tree deletion is not correct");
--   t.Clear();
--   assertion((t.Extent() == 0), "** A33: Tree extent is not correct");
--   assertion(t.IsNull(), "** A34: Tree is not null");
-- }

-- main () {

--   Binary_Tree btree_p1, btree_p2;

--   Multiway_Tree mtree_p1, mtree_p2;

--   AVL_Tree atree_p1(IsLessThan);

--   message("Starting tree tests");

--   message("...Binary Tree");
--   test_binary(btree_p1, btree_p2);

--   message("...Multiway Tree");
--   test_multiway(mtree_p1, mtree_p2);

--   message("...AVL Tree");
--   test_avl(atree_p1);

--   message("Completed tree tests");

--   return 0;

-- }

end Tree_Test;
