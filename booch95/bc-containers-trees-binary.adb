--  Copyright (C) 1994-2001 Grady Booch, David Weller and Simon Wright.
--  All Rights Reserved.
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

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

with Ada.Unchecked_Deallocation;
with BC.Support.Exceptions;

package body BC.Containers.Trees.Binary is

   package BSE renames BC.Support.Exceptions;
   procedure Assert
   is new BSE.Assert ("BC.Containers.Trees.Binary");

   function Create
     (I : Item; Parent, Left, Right : Binary_Node_Ref)
     return Binary_Node_Ref;

   function Create
     (I : Item; Parent, Left, Right : Binary_Node_Ref)
     return Binary_Node_Ref is
      Result : Binary_Node_Ref;
   begin
      Result := new Binary_Node'(Element => I,
                                 Parent => Parent,
                                 Left => Left,
                                 Right => Right,
                                 Count => 1);
      if Left /= null then
         Left.Parent := Result;
      end if;
      if Right /= null then
         Right.Parent := Result;
      end if;
      return Result;
   end Create;

   procedure Delete
   is new Ada.Unchecked_Deallocation (Binary_Node, Binary_Node_Ref);

   function Create (From : Binary_Tree) return Binary_Tree is
      Temp : Binary_Tree := (Ada.Finalization.Controlled with Rep => From.Rep);
   begin
      if From.Rep /= null then
         Temp.Rep.Count := Temp.Rep.Count + 1;
      end if;
      return Temp;
   end Create;

   function "=" (Left, Right : Binary_Tree) return Boolean is
   begin
      return Left.Rep.all = Right.Rep.all;
   end "=";

   procedure Clear (T : in out Binary_Tree) is
   begin
      Purge (T.Rep);
      T.Rep := null;
   end Clear;

   procedure Insert (T : in out Binary_Tree;
                     Elem : in Item;
                     Child : in Child_Branch) is
   begin
      Assert (T.Rep = null or else T.Rep.Parent = null,
              BC.Not_Root'Identity,
              "Insert",
              BSE.Not_Root);
      if Child = Left then
         T.Rep := Create (Elem,
                          Parent => null,
                          Left => T.Rep,
                          Right => null);
      else
         T.Rep := Create (Elem,
                          Parent => null,
                          Left => null,
                          Right => T.Rep);
      end if;
   end Insert;

   procedure Append (T : in out Binary_Tree;
                     Elem : in Item;
                     Child : in Child_Branch;
                     After : in Child_Branch) is
   begin
      if T.Rep = null then
         T.Rep := Create (Elem,
                          Parent => null,
                          Left => null,
                          Right => null);
      else
         if After = Left then
            if Child = Left then
               T.Rep.Left := Create (Elem,
                                     Parent => T.Rep,
                                     Left => T.Rep.Left,
                                     Right => null);
            else
               T.Rep.Left := Create (Elem,
                                     Parent => T.Rep,
                                     Left => null,
                                     Right => T.Rep.Left);
            end if;
         else
            if Child = Left then
               T.Rep.Right := Create (Elem,
                                      Parent => T.Rep,
                                      Left => T.Rep.Right,
                                      Right => null);
            else
               T.Rep.Right := Create (Elem,
                                      Parent => T.Rep,
                                      Left => null,
                                      Right => T.Rep.Right);
            end if;
         end if;
      end if;
   end Append;

   procedure Remove (T : in out Binary_Tree; Child : in Child_Branch) is
   begin
      Assert (T.Rep /= null,
              BC.Is_Null'Identity,
              "Remove",
              BSE.Is_Null);
      if Child = Left then
         Purge (T.Rep.Left);
         T.Rep.Left := null;
      else
         Purge (T.Rep.Right);
         T.Rep.Right := null;
      end if;
   end Remove;

   procedure Share (T : in out Binary_Tree;
                    Share_With : in Binary_Tree;
                    Child : in Child_Branch) is
      Temp : Binary_Node_Ref :=  Share_With.Rep;
   begin
      Assert (Share_With.Rep /= null,
              BC.Is_Null'Identity,
              "Share",
              BSE.Is_Null);
      if Child = Left then
         Temp := Share_With.Rep.Left;
      else
         Temp := Share_With.Rep.Right;
      end if;
      Clear (T);
      T.Rep := Temp;
      T.Rep.Count := T.Rep.Count + 1;
   end Share;

   procedure Swap_Child (T : in out Binary_Tree;
                         Swap_With : in out Binary_Tree;
                         Child : in Child_Branch) is
      Curr : Binary_Node_Ref;
   begin
      Assert (T.Rep /= null,
              BC.Is_Null'Identity,
              "Swap_Child",
              BSE.Is_Null);
      Assert (Swap_With.Rep = null or else Swap_With.Rep.Parent = null,
              BC.Not_Root'Identity,
              "Swap_Child",
              BSE.Not_Root);
      if Child = Left then
         Curr := T.Rep.Left;
         T.Rep.Left := Swap_With.Rep;
      else
         Curr := T.Rep.Right;
         T.Rep.Right := Swap_With.Rep;
      end if;
      if Swap_With.Rep /= null then
         Swap_With.Rep.Parent := T.Rep;
      end if;
      Swap_With.Rep := Curr;
      if Swap_With.Rep /= null then
         Swap_With.Rep.Parent := null;
      end if;
   end Swap_Child;

   procedure Child (T : in out Binary_Tree; Child : in Child_Branch) is
   begin
      if Child = Left then
         Left_Child (T);
      else
         Right_Child (T);
      end if;
   end Child;

   procedure Left_Child (T : in out Binary_Tree) is
      Curr : Binary_Node_Ref;
   begin
      Assert (T.Rep /= null,
              BC.Is_Null'Identity,
              "Left_Child",
              BSE.Is_Null);
      Curr := T.Rep;
      T.Rep := T.Rep.Left;
      if Curr.Count > 1 then
         Curr.Count := Curr.Count - 1;
         if T.Rep /= null then
            T.Rep.Count := T.Rep.Count + 1;
         end if;
      else
         if T.Rep /= null then
            T.Rep.Parent := null;
         end if;
         if Curr.Right /= null then
            Curr.Right.Parent := null;
         end if;
         Delete (Curr);
      end if;
   end Left_Child;

   function Left_Child (T : Binary_Tree) return Binary_Tree is
      Result : Binary_Tree;
   begin
      Result := T;
      Left_Child (Result);
      return Result;
   end Left_Child;

   procedure Right_Child (T : in out Binary_Tree) is
      Curr : Binary_Node_Ref;
   begin
      Assert (T.Rep /= null,
              BC.Is_Null'Identity,
              "Right_Child",
              BSE.Is_Null);
      Curr := T.Rep;
      T.Rep := T.Rep.Right;
      if Curr.Count > 1 then
         Curr.Count := Curr.Count - 1;
         if T.Rep /= null then
            T.Rep.Count := T.Rep.Count + 1;
         end if;
      else
         if T.Rep /= null then
            T.Rep.Parent := null;
         end if;
         if Curr.Left /= null then
            Curr.Left.Parent := null;
         end if;
         Delete (Curr);
      end if;
   end Right_Child;

   function Right_Child (T : Binary_Tree) return Binary_Tree is
      Result : Binary_Tree;
   begin
      Result := T;
      Right_Child (Result);
      return Result;
   end Right_Child;

   procedure Parent (T : in out Binary_Tree) is
   begin
      Assert (T.Rep /= null,
              BC.Is_Null'Identity,
              "Parent",
              BSE.Is_Null);
      if T.Rep.Parent = null then
         Clear (T);
      else
         T.Rep.Count := T.Rep.Count - 1;
         T.Rep := T.Rep.Parent;
         if T.Rep /= null then
            T.Rep.Count := T.Rep.Count + 1;
         end if;
      end if;
   end Parent;

   procedure Set_Item (T : in out Binary_Tree; Elem : in Item) is
   begin
      Assert (T.Rep /= null,
              BC.Is_Null'Identity,
              "Set_Item",
              BSE.Is_Null);
      T.Rep.Element := Elem;
   end Set_Item;

   function Has_Children (T : in Binary_Tree) return Boolean is
   begin
      return (T.Rep /= null and then
              (T.Rep.Left /= null or else T.Rep.Right /= null));
   end Has_Children;

   function Is_Null (T : in Binary_Tree) return Boolean is
   begin
      return T.Rep = null;
   end Is_Null;

   function Is_Shared (T : in Binary_Tree) return Boolean is
   begin
      return T.Rep /= null and then T.Rep.Count > 1;
   end Is_Shared;

   function Is_Root (T : in Binary_Tree) return Boolean is
   begin
      return T.Rep = null or else T.Rep.Parent = null;
   end Is_Root;

   function Item_At (T : in Binary_Tree) return Item is
   begin
      Assert (T.Rep /= null,
              BC.Is_Null'Identity,
              "Item_At",
              BSE.Is_Null);
      return T.Rep.Element;
   end Item_At;

   procedure Purge (Node : in out Binary_Node_Ref) is
   begin
      if Node /= null then
         if Node.Count > 1 then
            Node.Count := Node.Count - 1;
         else
            Purge (Node.Left);
            if Node.Left /= null then
               Node.Left.Parent := null;
            end if;
            Purge (Node.Right);
            if Node.Right /= null then
               Node.Right.Parent := null;
            end if;
            Delete (Node);
         end if;
      end if;
   end Purge;

   procedure Initialize (T : in out Binary_Tree) is
   begin
      null;
   end Initialize;

   procedure Adjust (T : in out Binary_Tree) is
   begin
      if T.Rep /= null then
         T.Rep.Count := T.Rep.Count + 1;
      end if;
   end Adjust;

   procedure Finalize (T : in out Binary_Tree) is
   begin
      Clear (T);
   end Finalize;

end BC.Containers.Trees.Binary;
