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

package body BC.Containers.Trees.Binary is

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

  procedure Clear (Obj : in out Binary_Tree) is
  begin
    Purge (Obj.Rep);
    Obj.Rep := null;
  end Clear;

  procedure Insert (Obj : in out Binary_Tree;
                    Elem : in Item;
                    Child : in Child_Branch) is
  begin
    pragma Assert (Obj.Rep = null or else Obj.Rep.Parent = null,
                   "Tried to insert when not at root");
    if Child = Left then
      Obj.Rep := Create (Elem,
                         Parent => null,
                         Left => Obj.Rep,
                         Right => null);
    else
      Obj.Rep := Create (Elem,
                         Parent => null,
                         Left => null,
                         Right => Obj.Rep);
    end if;
  end Insert;

  procedure Append (Obj : in out Binary_Tree;
                    Elem : in Item;
                    Child : in Child_Branch;
                    After : in Child_Branch) is
  begin
    if Obj.Rep = null then
      Obj.Rep := Create (Elem,
                         Parent => null,
                         Left => null,
                         Right => null);
    else
      if After = Left then
        if Child = Left then
          Obj.Rep.Left := Create (Elem,
                                  Parent => Obj.Rep,
                                  Left => Obj.Rep.Left,
                                  Right => null);
        else
          Obj.Rep.Left := Create (Elem,
                                  Parent => Obj.Rep,
                                  Left => null,
                                  Right => Obj.Rep.Left);
        end if;
      else
        if Child = Left then
          Obj.Rep.Right := Create (Elem,
                                   Parent => Obj.Rep,
                                   Left => Obj.Rep.Right,
                                   Right => null);
        else
          Obj.Rep.Right := Create (Elem,
                                   Parent => Obj.Rep,
                                   Left => null,
                                   Right => Obj.Rep.Right);
        end if;
      end if;
    end if;
  end Append;

  procedure Remove (Obj : in out Binary_Tree; Child : in Child_Branch) is
  begin
    pragma Assert (Obj.Rep /= null, "Tried to Remove from null tree");
    if Child = Left then
      Purge (Obj.Rep.Left);
      Obj.Rep.Left := null;
    else
      Purge (Obj.Rep.Right);
      Obj.Rep.Right := null;
    end if;
  end Remove;

  procedure Share (Obj : in out Binary_Tree;
                   Share_With : in Binary_Tree;
                   Child : in Child_Branch) is
    Temp : Binary_Node_Ref :=  Share_With.Rep;
  begin
    pragma Assert (Share_With.Rep /= null,
                   "Attempt to Share with null pointer");
    if Child = Left then
      Temp := Share_With.Rep.Left;
    else
      Temp := Share_With.Rep.Right;
    end if;
    Clear (Obj);
    Obj.Rep := Temp;
    Obj.Rep.Count := Obj.Rep.Count + 1;
  end Share;

  procedure Swap_Child (Obj : in out Binary_Tree;
                        Swap_With : in out Binary_Tree;
                        Child : in Child_Branch) is
    Curr : Binary_Node_Ref;
  begin
    pragma Assert (Obj.Rep /= null, "Attempt to Swap with null Tree");
    pragma Assert (Swap_With.Rep = null or else Swap_With.Rep.Parent = null,
                   "Attempt to Swap with non root Tree");
    if Child = Left then
      Curr := Obj.Rep.Left;
      Obj.Rep.Left := Swap_With.Rep;
    else
      Curr := Obj.Rep.Right;
      Obj.Rep.Right := Swap_With.Rep;
    end if;
    if Swap_With.Rep /= null then
      Swap_With.Rep.Parent := Obj.Rep;
    end if;
    Swap_With.Rep := Curr;
    if Swap_With.Rep /= null then
      Swap_With.Rep.Parent := null;
    end if;
  end Swap_Child;

  procedure Child (Obj : in out Binary_Tree; Child : in Child_Branch) is
  begin
    if Child = Left then
      Left_Child (Obj);
    else
      Right_Child (Obj);
    end if;
  end Child;

  procedure Left_Child (Obj : in out Binary_Tree) is
    Curr : Binary_Node_Ref;
  begin
    pragma Assert (Obj.Rep /= null, "Attempt to Left_Child a null tree");
    Curr := Obj.Rep;
    Obj.Rep := Obj.Rep.Left;
    if Curr.Count > 1 then
      Curr.Count := Curr.Count - 1;
      if Obj.Rep /= null then
        Obj.Rep.Count := Obj.Rep.Count + 1;
      end if;
    else
      if Obj.Rep /= null then
        Obj.Rep.Parent := null;
      end if;
      if Curr.Right /= null then
        Curr.Right.Parent := null;
      end if;
      Delete (Curr);
    end if;
  end Left_Child;

  procedure Right_Child (Obj : in out Binary_Tree) is
    Curr : Binary_Node_Ref;
  begin
    pragma Assert (Obj.Rep /= null, "Attempt to Right_Child a null tree");
    Curr := Obj.Rep;
    Obj.Rep := Obj.Rep.Right;
    if Curr.Count > 1 then
      Curr.Count := Curr.Count - 1;
      if Obj.Rep /= null then
        Obj.Rep.Count := Obj.Rep.Count + 1;
      end if;
    else
      if Obj.Rep /= null then
        Obj.Rep.Parent := null;
      end if;
      if Curr.Left /= null then
        Curr.Left.Parent := null;
      end if;
      Delete (Curr);
    end if;
  end Right_Child;

  procedure Parent (Obj : in out Binary_Tree) is
  begin
    pragma Assert (Obj.Rep /= null, "Attempt to Parent a null tree");
    if Obj.Rep.Parent = null then
      Clear (Obj);
    else
      Obj.Rep.Count := Obj.Rep.Count - 1;
      Obj.Rep := Obj.Rep.Parent;
      if Obj.Rep /= null then
        Obj.Rep.Count := Obj.Rep.Count + 1;
      end if;
    end if;
  end Parent;

  procedure Set_Item (Obj : in out Binary_Tree; Elem : in Item) is
  begin
    pragma Assert (Obj.Rep /= null, "Attempt to Set_Item on null tree");
    Obj.Rep.Element := Elem;
  end Set_Item;

  function Has_Children (Obj : in Binary_Tree) return Boolean is
  begin
    return (Obj.Rep /= null and then
            (Obj.Rep.Left /= null or else Obj.Rep.Right /= null));
  end Has_Children;

  function Is_Null (Obj : in Binary_Tree) return Boolean is
  begin
    return Obj.Rep = null;
  end Is_Null;

  function Is_Shared (Obj : in Binary_Tree) return Boolean is
  begin
    return Obj.Rep /= null and then Obj.Rep.Count > 1;
  end Is_Shared;

  function Is_Root (Obj : in Binary_Tree) return Boolean is
  begin
    -- XXX can you be the root of a tree if you are null?
    return Obj.Rep /= null and then Obj.Rep.Parent = null;
  end Is_Root;

  function Item_At (Obj : in Binary_Tree) return Item is
  begin
    pragma Assert (Obj.Rep /= null, "Attempt to take Item_At with null tree");
    return Obj.Rep.Element;
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
        pragma Assert (Node = null, "Purge should have deleted the Node");
      end if;
    end if;
  end Purge;

  procedure Initialize (Obj : in out Binary_Tree) is
  begin
    null;
  end Initialize;

  procedure Adjust (Obj : in out Binary_Tree) is
  begin
    if Obj.Rep /= null then
      Obj.Rep.Count := Obj.Rep.Count + 1;
    end if;
  end Adjust;

  procedure Finalize (Obj : in out Binary_Tree) is
  begin
    Clear (Obj);
  end;

end BC.Containers.Trees.Binary;
