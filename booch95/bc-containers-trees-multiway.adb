-- Copyright (C) 1994-1999 Grady Booch, David Weller and Simon Wright.
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

with BC.Support.Exceptions;

package body BC.Containers.Trees.Multiway is

  package BSE renames BC.Support.Exceptions;
  procedure Assert
  is new BSE.Assert ("BC.Containers.Trees.Multiway");

  use type Nodes.Multiway_Node;
  use type Nodes.Multiway_Node_Ref;

  procedure Mend (T : in out Nodes.Multiway_Node_Ref) is
  begin
    if T.Child /= null then
      T.Child.Parent := T;
    end if;
  end Mend;

  procedure Purge (Curr : in out Nodes.Multiway_Node_Ref) is
  begin
    if Curr /= null then
      if Curr.Count > 1 then
        Curr.Count := Curr.Count - 1;
      else
        declare
          Ptr : Nodes.Multiway_Node_Ref := Curr.Child;
          Next: Nodes.Multiway_Node_Ref;
        begin
          while Ptr /= null loop
            Next := Ptr.Sibling;
            Ptr.Sibling := null;
            Purge (Ptr);
            if Ptr /= null then
              Ptr.Parent := null;
            end if;
            Ptr := Next;
          end loop;
          Nodes.Delete (Curr);
        end;
      end if;
    end if;
  end Purge;

  function Create (From : Multiway_Tree) return Multiway_Tree is
    Temp : Multiway_Tree := (Ada.Finalization.Controlled with Rep => From.Rep);
  begin
    if From.Rep /= null then
      Temp.Rep.Count := Temp.Rep.Count + 1;
    end if;
    return Temp;
  end Create;

  function "=" (Left, Right : Multiway_Tree) return Boolean is
  begin
    return Left.Rep = Right.Rep;
  end "=";

  procedure Clear (T : in out Multiway_Tree) is
  begin
    Purge (T.Rep);
    T.Rep := null;
  end Clear;

  procedure Insert (T : in out Multiway_Tree; Elem : in Item) is
  begin
    Assert (T.Rep = null or else T.Rep.Parent = null,
       BC.Not_Root'Identity,
       "Insert",
       BSE.Not_Root);
    T.Rep := Nodes.Create (Elem,
                           Parent => null,
                           Child => T.Rep,
                           Sibling => null);
  end Insert;

  procedure Append (T : in out Multiway_Tree; Elem : in Item) is
  begin
    if T.Rep = null then
      T.Rep := Nodes.Create (Elem,
                             Parent => null,
                             Child => T.Rep,
                             Sibling => null);
    else
      T.Rep.Child := Nodes.Create (Elem,
                                   Parent => T.Rep,
                                   Child => null,
                                   Sibling => T.Rep.Child);
    end if;
  end Append;

  procedure Append (T : in out Multiway_Tree;
                    Elem : in Item;
                    After : Positive) is
  begin
    if T.Rep = null then
      T.Rep := Nodes.Create (Elem,
                             Parent => null,
                             Child => T.Rep,
                             Sibling => null);
    else
      declare
        Curr : Nodes.Multiway_Node_Ref := T.Rep.Child;
      begin
        if Curr = null then
          T.Rep.Child := Nodes.Create (Elem,
                                       Parent => T.Rep,
                                       Child => null,
                                       Sibling => T.Rep.Child);
        else
          declare
            I : Positive := 1;
          begin
            while Curr /= null and then I < After loop
              Curr := Curr.Sibling;
              I := I + 1;
            end loop;
            Assert (Curr /= null,
                    BC.Range_Error'Identity,
                    "Append",
                    BSE.Invalid_Index);
            Curr.Sibling := Nodes.Create (Elem,
                                          Parent => T.Rep,
                                          Child => null,
                                          Sibling => Curr.Sibling);
          end;
        end if;
      end;
    end if;
  end Append;

  procedure Append (T : in out Multiway_Tree;
                    From_Tree : in out Multiway_Tree) is
  begin
    if From_Tree.Rep = null then
      return;
    end if;
    Assert(From_Tree.Rep.Parent = null,
           BC.Not_Root'Identity,
           "Append",
           BSE.Not_Root);
    if T.Rep = null then
      T.Rep := From_Tree.Rep;
      T.Rep.Count := T.Rep.Count + 1;
    else
      From_Tree.Rep.Sibling := T.Rep.Child;
      From_Tree.Rep.Parent := T.Rep;
      From_Tree.Rep.Count := From_Tree.Rep.Count + 1;
      T.Rep.Child := From_Tree.Rep;
    end if;
  end Append;

  procedure Remove (T : in out Multiway_Tree; Index : Positive) is
  begin
    Assert (T.Rep /= null,
            BC.Is_Null'Identity,
            "Remove",
            BSE.Is_Null);
    declare
      I : Positive := 1;
      Prev : Nodes.Multiway_Node_Ref;
      Curr : Nodes.Multiway_Node_Ref := T.Rep.Child;
    begin
      while Curr /= null and then I < Index loop
        Prev := Curr;
        Curr := Curr.Sibling;
        I := I + 1;
      end loop;
      Assert (Curr /= null,
              BC.Range_Error'Identity,
              "Remove",
              BSE.Invalid_Index);
      if Prev = null then
        T.Rep.Child := Curr.Sibling;
      else
        Prev.Sibling := Curr.Sibling;
      end if;
      Curr.Parent := null;
      Curr.Sibling := null;
      Purge (Curr);
    end;
  end Remove;

  procedure Share (T : in out Multiway_Tree;
                   Share_With : in Multiway_Tree;
                   Child : Positive) is
    Ptr : Nodes.Multiway_Node_Ref := Share_With.Rep;
    I : Positive := 1;
  begin
    Assert (Ptr /= null,
            BC.Is_Null'Identity,
            "Share",
            BSE.Is_Null);
    Ptr := Ptr.Child;
    while Ptr /= null and then I < Child loop
      Ptr := Ptr.Sibling;
      I := I + 1;
    end loop;
    Assert (Ptr /= null,
            BC.Range_Error'Identity,
            "Share",
            BSE.Invalid_Index);
    Clear (T);
    T.Rep := Ptr;
    T.Rep.Count := T.Rep.Count + 1;
  end Share;

  procedure Swap_Child (T : in out Multiway_Tree;
                        Swap_WIth : in out Multiway_Tree;
                        Child : in Positive) is
    Prev : Nodes.Multiway_Node_Ref;
    Curr : Nodes.Multiway_Node_Ref := T.Rep;
    I : Positive := 1;
  begin
    Assert (T.Rep /= null,
            BC.Is_Null'Identity,
            "Swap_Child",
            BSE.Is_Null);
    Assert (Swap_With.Rep = null or else Swap_With.Rep.Parent = null,
       BC.Not_Root'Identity,
       "Swap_Child",
       BSE.Not_Root);
    Curr := Curr.Child;
    while Curr /= null and then I < Child loop
      Prev := Curr;
      Curr := Curr.Sibling;
      I := I + 1;
    end loop;
    Assert (Curr /= null,
            BC.Range_Error'Identity,
            "Swap_Child",
            BSE.Invalid_Index);
    Swap_With.Rep.Sibling := Curr.Sibling;
    if Prev = null then
      T.Rep.Child := Swap_With.Rep;
    else
      Prev.Sibling := Swap_With.Rep;
    end if;
    if Swap_With.Rep /= null then
      Swap_With.Rep.Parent := T.Rep;
    end if;
    Swap_With.Rep := Curr;
    Swap_With.Rep.Sibling := null;
    Swap_With.Rep.Parent := null;
  end Swap_Child;


  procedure Child (T : in out Multiway_Tree; Child : in Positive) is
    Curr : Nodes.Multiway_Node_Ref := T.Rep;
    I : Positive := 1;
  begin
    Assert (T.Rep /= null,
            BC.Is_Null'Identity,
            "Child",
            BSE.Is_Null);
    Curr := Curr.Child;
    while Curr /= null and then I < Child loop
      Curr := Curr.Sibling;
      I := I + 1;
    end loop;
    Assert (Curr /= null,
            BC.Range_Error'Identity,
            "Child",
            BSE.Invalid_Index);
    Curr.Count := Curr.Count + 1;
    Purge (T.Rep);
    T.Rep := Curr;
  end Child;

  procedure Parent (T : in out Multiway_Tree) is
  begin
    Assert (T.Rep /= null,
            BC.Is_Null'Identity,
            "Parent",
            BSE.Is_Null);
    if T.Rep.Parent = null then
      Clear (T);
    else
      T.Rep.Count := T.Rep.Count -1 ;
      T.Rep := T.Rep.Parent;
      T.Rep.Count := T.Rep.Count + 1;
    end if;
  end Parent;

  procedure Set_Item (T : in out Multiway_Tree; Elem : in Item) is
  begin
    Assert (T.Rep /= null,
            BC.Is_Null'Identity,
            "Set_Item",
            BSE.Is_Null);
    T.Rep.Element := Elem;
  end Set_Item;

  function Arity (T : Multiway_Tree) return Natural is
  begin
    Assert (T.Rep /= null,
            BC.Is_Null'Identity,
            "Arity",
            BSE.Is_Null);
    declare
      Count : Natural := 0;
      Ptr : Nodes.Multiway_Node_Ref := T.Rep.Child;
    begin
      while Ptr /= null loop
        Count := Count + 1;
        Ptr := Ptr.Sibling;
      end loop;
      return Count;
    end;
  end Arity;

  function Has_Children (T : in Multiway_Tree) return Boolean is
  begin
    return T.Rep /= null and then T.Rep.Child /= null;
  end Has_Children;

  function Is_Null (T : in Multiway_Tree) return Boolean is
  begin
    return T.Rep = null;
  end Is_Null;

  function Is_Shared (T : in Multiway_Tree) return Boolean is
  begin
    return T.Rep /= null and then T.Rep.Count > 1;
  end Is_Shared;

  function Is_Root (T : in Multiway_Tree) return Boolean is
  begin
    return T.Rep = null or else T.Rep.Parent = null;
  end Is_Root;

  function Item_At (T : in Multiway_Tree) return Item is
  begin
    Assert (T.Rep /= null,
            BC.Is_Null'Identity,
            "Item_At",
            BSE.Is_Null);
    return T.Rep.Element;
  end Item_At;

  function Item_At (T : in Multiway_Tree) return Item_Ptr is
  begin
    Assert (T.Rep /= null,
            BC.Is_Null'Identity,
            "Item_At",
            BSE.Is_Null);
    return T.Rep.Element'access;
  end Item_At;

  procedure Initialize (T : in out Multiway_Tree) is
  begin
    null;
  end Initialize;

  procedure Adjust (T : in out Multiway_Tree) is
  begin
    if T.Rep /= null then
      T.Rep.Count := T.Rep.Count + 1;
    end if;
  end Adjust;

  procedure Finalize (T : in out Multiway_Tree) is
  begin
    Clear(T);
  end Finalize;

end BC.Containers.Trees.Multiway;
