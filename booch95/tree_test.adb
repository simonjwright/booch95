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
with Root_Mway_Trees;
with Character_References;

procedure Tree_Test is

  use Text_IO;
  use Character_References;

  procedure Assertion (Cond : Boolean; Message : String) is
  begin
    if not Cond then
      Put_Line (Message);
    end if;
  end Assertion;
  pragma Inline (Assertion);

  -- I used this while trying to figure out my booboos. An example
  -- (not good) of how to approach tree traversal.
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

  procedure Test_Primitive (T1, T2 : in out Root_Mway_Trees.Multiway_Tree) is
    use Root_Mway_Trees;
    Tt1, Tt2, Tt3 : Multiway_Tree;
  begin
    Assertion (Is_Null (T1), "** M01: Tree is not initially null");
    Assertion (not Is_Shared(T1), "** M02: Tree is initially shared");
    Insert (T1, '1');
    Insert (T1, '2');
    Insert (T1, '3');
    Insert (T1, '4');
    Assertion (not Is_Null (T1), "** M03: Tree is empty");
    Assertion (Item_At (T1) = '4', "** M04: Tree item is not correct");
    Assertion (not Is_Shared (T1), "** M05: Tree is shared");
    Assertion (Arity (T1) = 1, "** M06: Tree arity is not correct");
    Child (T1, 1);
    Assertion (Item_At (T1) = '3', "** M07: Tree item is not correct");
    Assertion (Arity (T1) = 1, "** M08: Tree arity is not correct");
    Child (T1, 1);
    Assertion (Item_At (T1) = '2', "** M09: Tree item is not correct");
    Assertion (Arity (T1) = 1, "** M10: Tree arity is not correct");
    Child (T1, 1);
    Assertion (Item_At (T1) = '1', "** M11: Tree item is not correct");
    Assertion (Arity (T1) = 0, "** M12: Tree arity is not correct");
    Clear (T1);
    Assertion (Is_Null (T1), "** M13: Tree is not empty");
    Append (T1, '6');
    Append (T1, '2');
    Append (T1, '3');
    Append (T1, '5');
    Tt1 := T1;
    Tt2 := T1;
    Clear (Tt2);
    Assertion (Item_At (T1) = '6', "** M14: Tree item is not correct");
    Assertion (Arity (T1) = 3, "** M15: Tree arity is not correct");
    Child (T1, 1);
    Assertion (Item_At (T1) = '5', "** M16: Tree item is not correct");
    Parent (T1);
    Assertion (Item_At (T1) = '6', "** M17: Tree item is not correct");
    Child (T1, 2);
    Assertion (Item_At (T1) = '3', "** M18: Tree item is not correct");
    Parent (T1);
    Assertion (Item_At (T1) = '6', "** M19: Tree item is not correct");
    Child (T1, 3);
    Assertion (Item_At (T1) = '2', "** M20: Tree item is not correct");
    T2 := Tt1;
    Append (T2, '4', 1);
    Append (T2, '1', 4);
    Parent (T1);
    Child (T1, 2);
    Assertion (Item_At (T1) = '4', "** M21: Tree item is not correct");
    Parent (T1);
    Child (T1, 5);
    Assertion (Item_At (T1) = '1', "** M22: Tree item is not correct");
    Assertion (not Is_Root (T1), "** M23: Tree is root");
    Assertion (not Has_Children (T1), "** M24: Tree has children");
    Parent (T1);
    Clear (T2);
    Assertion (Tt1 = T1, "** M25: Trees are not equal");
    Set_Item (Tt1, '7');
    Assertion (Item_At (T1) = '7', "** M26: Tree item is not correct");
    Clear (Tt1);
    Assertion (Tt1 /= T1, "** M27: Trees are equal");
    Assertion (Is_Root (T1), "** M28: Tree is root");  -- hmm
    Assertion (Has_Children (T1), "** M29: Tree has children");  -- hmm
    Share (T2, T1, 3);
    Assertion (Item_At (T2) = '3',  "** M30: Tree item is not correct");
    Share (Tt1, T1, 1);
    Assertion (Item_At (Tt1) = '5',  "** M31: Tree item is not correct");
    Share (Tt2, T1, 5);
    Assertion (Item_At (Tt2) = '1',  "** M32: Tree item is not correct");
    Append (T2, '8');
    Append (T2, '9');
    Insert (Tt3, '1');
    Insert (Tt3, '2');
    Insert (Tt3, '3');
    Swap_Child (T1, Tt3, 1);
    Assertion (Tt1 = Tt3, "** M33: Trees are not equal");
    Tt1 := T1;
    Child (Tt1, 1);
    Assertion (Item_At (Tt1) = '3',  "** M34: Tree item is not correct");
    Assertion (Has_Children (Tt1), "** M35: Tree has no children");
    Assertion (not Has_Children (Tt3), "** M36: Tree has children");
    Assertion (Item_At (Tt3) = '5',  "** M37: Tree item is not correct");
    Clear (Tt3);
    Remove (T1, 2);
    Tt3 := T1;
    Child (Tt3, 2);
    Assertion (Tt3 = T2, "** M38: Trees are not equal");
    Clear (Tt3);
    Assertion (not Is_Root (Tt1), "** M39: Tree is root");
    Remove (T1, 1);
    Assertion (Is_Root (Tt1), "** M40: Tree is root");  -- hmm
    Assertion (Arity (T1) = 3, "** M41: Tree arity is not correct");
    Tt3 := T1;
    Child (Tt3, 1);
    Assertion (Tt3 = T2, "** M42: Trees are not equal");
    Clear (Tt3);
    Clear (T1);
    Assertion (Is_Null (T1), "** M43: Tree is not null");
    Assertion (Is_Root (T2), "** M44: Tree is not root");
    Assertion (Item_At (T2) = '3', "** M45: Tree item is not correct");
    Assertion (Is_Root (Tt2), "** M46: Tree is not root");
    Assertion (Item_At (Tt2) = '1', "** M47: Tree item is not correct");
  end Test_Primitive;

  M_Tree_P1, M_Tree_P2 : aliased Root_Mway_Trees.Multiway_Tree;

begin

  Put_Line ("Starting Tree tests");

  Put_Line ("...Binary Tree");
  Test_Primitive (B_Tree_P1, B_Tree_P2);

  Put_Line ("...Multiway Tree");
  Test_Primitive (M_Tree_P1, M_Tree_P2);

  Put_Line ("...sorry, no AVL trees yet.");

  Put_Line ("Completed Tree tests");

end Tree_Test;
