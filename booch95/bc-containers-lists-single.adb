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

with BC.Support.Exceptions;
with System.Address_To_Access_Conversions;

package body BC.Containers.Lists.Single is

  package BSE renames BC.Support.Exceptions;
  procedure Assert
  is new BSE.Assert ("BC.Containers.Lists.Single");

  use type Single_Nodes.Single_Node_Ref;

  function "=" (L, R : Single_List) return Boolean is
  begin
    return L.Rep = R.Rep;
  end "=";

  procedure Clear (Obj : in out Single_List) is
    Curr : Single_Nodes.Single_Node_Ref := Obj.Rep;
    Ptr : Single_Nodes.Single_Node_Ref;
  begin
    while Curr /= null loop
      Ptr := Curr;
      Curr := Curr.Next;
      if Ptr.Count > 1 then
        Ptr.Count := Ptr.Count - 1;
        exit;
      else
        Single_Nodes.Delete (Ptr);
      end if;
    end loop;
    Obj.Rep := null;
  end Clear;

  procedure Insert (Obj : in out Single_List; Elem : Item) is
  begin
    Obj.Rep := Single_Nodes.Create (Elem, Next => Obj.Rep);
  end Insert;

  procedure Insert (Obj : in out Single_List; From_List : in Single_List) is
    Ptr : Single_Nodes.Single_Node_Ref := From_List.Rep;
  begin
    if Ptr /= null then
      while Ptr.Next /= null loop
        Ptr := Ptr.Next;
      end loop;
    end if;
    Ptr.Next := Obj.Rep;
    Obj.Rep := From_List.Rep;
    Obj.Rep.Count := Obj.Rep.Count + 1;
  end Insert;

  procedure Insert (Obj : in out Single_List;
                    Elem : Item;
                    Before : Positive) is
    Prev : Single_Nodes.Single_Node_Ref;
    Curr : Single_Nodes.Single_Node_Ref := Obj.Rep;
    Index: Positive := 1;
  begin
    if Curr = null or else Before = 1 then
      Insert(Obj, Elem);
    else
      while Curr /= null and then Index < Before loop
        Prev := Curr;
        Curr := Curr.Next;
        Index := Index + 1;
      end loop;
      Assert (Curr /= null,
              BC.Range_Error'Identity,
              "Insert",
              BSE.Invalid_Index);
      Prev.Next := Single_Nodes.Create (Elem, Next => Curr);
    end if;
  end Insert;

  procedure Insert (Obj : in out Single_List;
                    From_List: in out Single_List;
                    Before : Positive) is
    Prev : Single_Nodes.Single_Node_Ref;
    Curr : Single_Nodes.Single_Node_Ref := Obj.Rep;
    Ptr  : Single_Nodes.Single_Node_Ref := From_List.Rep;
    Index: Positive := 1;
  begin
    if Ptr /= null then
      if Curr = null or else Before = 1 then
        Insert(Obj, From_List);
      else
        while Curr /= null and then Index < Before loop
          Prev := Curr;
          Curr := Curr.Next;
          Index := Index + 1;
        end loop;
        Assert (Curr /= null,
                BC.Range_Error'Identity,
                "Insert",
                BSE.Invalid_Index);
        while Ptr.Next /= null loop
          Ptr := Ptr.Next;
        end loop;
        Ptr.Next := Curr;
        Prev.Next := From_List.Rep;
        From_List.Rep.Count := From_List.Rep.Count + 1;
      end if;
    end if;
  end Insert;

  procedure Append (Obj : in out Single_List; Elem : Item) is
    Curr : Single_Nodes.Single_Node_Ref := Obj.Rep;
  begin
    if Curr /= null then
      while Curr.Next /= null loop
        Curr := Curr.Next;
      end loop;
      Curr.Next := Single_Nodes.Create (Elem, Next => null);
    else
      Obj.Rep := Single_Nodes.Create (Elem, Next => null);
    end if;
  end Append;

  procedure Append (Obj : in out Single_List; From_List : in Single_List) is
    Curr : Single_Nodes.Single_Node_Ref := Obj.Rep;
  begin
    if From_List.Rep /= null then
      if Curr /= null then
        while Curr.Next /= null loop
          Curr := Curr.Next;
        end loop;
      end if;
      if Curr /= null then
        Curr.Next := From_List.Rep;
      else
        Obj.Rep := From_List.Rep;
      end if;
      From_List.Rep.Count := From_List.Rep.Count + 1;
    end if;
  end Append;

  procedure Append (Obj : in out Single_List; Elem : Item; After : Positive) is
    Curr : Single_Nodes.Single_Node_Ref := Obj.Rep;
    Index: Positive := 1;
  begin
    if Curr = null then
      Append(Obj, Elem);
    else
      while Curr /= null and then Index < After loop
        Curr := Curr.Next;
        Index := Index + 1;
      end loop;
      Assert (Curr /= null,
              BC.Range_Error'Identity,
              "Append",
              BSE.Invalid_Index);
      Curr.Next := Single_Nodes.Create (Elem, Next => Curr.Next);
    end if;
  end Append;

  procedure Append (Obj : in out Single_List;
                    From_List : in Single_List;
                    After : Positive) is
    Curr : Single_Nodes.Single_Node_Ref := Obj.Rep;
    Ptr  : Single_Nodes.Single_Node_Ref := From_List.Rep;
    Index: Positive := 1;
  begin
    if Ptr /= null then
      if Curr = null then
        Append(Obj, From_List);
      else
        while Curr /= null and then Index < After loop
          Curr := Curr.Next;
          Index := Index + 1;
        end loop;
        Assert (Curr /= null,
                BC.Range_Error'Identity,
                "Append",
                BSE.Invalid_Index);
        while Ptr.Next /= null loop
          Ptr := Ptr.Next;
        end loop;
        Ptr.Next := Curr.Next;
        Curr.Next := From_List.Rep;
        From_List.Rep.Count := From_List.Rep.Count + 1;
      end if;
    end if;
  end Append;

  procedure Remove (Obj : in out Single_List; From : Positive) is
    Prev : Single_Nodes.Single_Node_Ref;
    Curr : Single_Nodes.Single_Node_Ref := Obj.Rep;
    Index: Positive := 1;
  begin
    while Curr /= null and then Index < From loop
      Prev := Curr;
      Curr := Curr.Next;
      Index := Index + 1;
    end loop;
    Assert (Curr /= null,
            BC.Range_Error'Identity,
            "Remove",
            BSE.Invalid_Index);
    if Prev /= null then
      Prev.Next := Curr.Next;
    else
      Obj.Rep := Curr.Next;
    end if;
    if Curr.Count > 1 then
      Curr.Count := Curr.Count - 1;
      Obj.Rep := Curr.Next;
      Curr.Next := null;
    else
      Single_Nodes.Delete (Curr);
    end if;
  end Remove;

  procedure Purge (Obj : in out Single_List; From : Positive) is
    Prev : Single_Nodes.Single_Node_Ref;
    Curr : Single_Nodes.Single_Node_Ref := Obj.Rep;
    Ptr : Single_Nodes.Single_Node_Ref;
    Index : Positive := 1;
  begin
    while Curr /= null and then Index < From loop
      Prev := Curr;
      Curr := Curr.Next;
      Index := Index + 1;
    end loop;
    Assert (Curr /= null,
            BC.Range_Error'Identity,
            "Purge",
            BSE.Invalid_Index);
    if Prev /= null then
      Prev.Next := null;
    else
      Obj.Rep := null;
    end if;
    while Curr /= null loop
      Ptr := Curr;
      Curr := Curr.Next;
      if Ptr.Count > 1 then
        Ptr.Count := Ptr.Count - 1;
        exit;
      else
        Single_Nodes.Delete (Ptr);
      end if;
    end loop;
  end Purge;

  procedure Purge (Obj : in out Single_List;
                   From : Positive;
                   Count : Positive) is
    Prev, Ptr : Single_Nodes.Single_Node_Ref;
    Curr : Single_Nodes.Single_Node_Ref := Obj.Rep;
    Index : Positive := 1;
    Cut : Boolean := True;
  begin
    while Curr /= null and then Index < From loop
      Prev := Curr;
      Curr := Curr.Next;
      Index := Index + 1;
    end loop;
    Assert (Curr /= null,
            BC.Range_Error'Identity,
            "Purge",
            BSE.Invalid_Index);
    if Prev /= null then
      Prev.Next := null;
    else
      Obj.Rep := null;
    end if;
    Index := 1;
    while Curr /= null and then Index <= Count loop
      Ptr := Curr;
      Curr := Curr.Next;
      if Cut then
        if Ptr.Count > 1 then
          Ptr.Count := Ptr.Count - 1;
          Cut := False;
        else
          Single_Nodes.Delete (Ptr);
        end if;
      end if;
      Index := Index + 1;
    end loop;
    Ptr.Next := null;
    if Curr /= null then
      if Prev /= null then
        Prev.Next := Curr;
      else
        Obj.Rep := Curr;
      end if;
    end if;
  end Purge;

  procedure Preserve (Obj : in out Single_List; From : Positive) is
    Temp : Single_List;
  begin
    Share(Temp, Obj, From);
    Share_Head(Obj, Temp);
  end Preserve;

  procedure Preserve (Obj: in out Single_List;
                      From : Positive;
                      Count : Positive) is
  begin
    Preserve (Obj, From);
    if Length (Obj) > Count then
      Purge (Obj, Count + 1); -- we start at 1, remember!
    end if;
  end Preserve;

  procedure Share (Obj : in out Single_List;
                   With_List: Single_List;
                   Starting_At : Positive) is
    Ptr : Single_Nodes.Single_Node_Ref := With_List.Rep;
    Index : Positive := 1;
  begin
    Assert (Ptr /= null,
            BC.Is_Null'Identity,
            "Share",
            BSE.Is_Null);
    while Ptr /= null and then Index < Starting_At loop
      Ptr := Ptr.Next;
      Index := Index + 1;
    end loop;
    Assert (Ptr /= null,
            BC.Range_Error'Identity,
            "Share",
            BSE.Invalid_Index);
    Clear (Obj);
    Obj.Rep := Ptr;
    Obj.Rep.Count := Obj.Rep.Count + 1;
  end Share;

  procedure Share_Head (Obj : in out Single_List;
                        With_List : in Single_List) is
  begin
    Assert (With_List.Rep /= null,
            BC.Is_Null'Identity,
            "Share_Head",
            BSE.Is_Null);
    Clear (Obj);
    Obj.Rep := With_List.Rep;
    Obj.Rep.Count := Obj.Rep.Count + 1;
  end Share_Head;

  procedure Share_Foot (Obj : in out Single_List;
                        With_List : in Single_List) is
    Ptr : Single_Nodes.Single_Node_Ref := With_List.Rep;
  begin
    Assert (Ptr /= null,
            BC.Is_Null'Identity,
            "Share_Foot",
            BSE.Is_Null);
    Clear (Obj);
    while Ptr.Next /= null loop
      Ptr := Ptr.Next;
    end loop;
    Obj.Rep := Ptr;
    Obj.Rep.Count := Obj.Rep.Count + 1;
  end Share_Foot;

  procedure Swap_Tail (Obj : in out Single_List;
                       With_List: in out Single_List) is
    Curr : Single_Nodes.Single_Node_Ref;
  begin
    Assert (Obj.Rep /= null,
            BC.Is_Null'Identity,
            "Swap_Tail",
            BSE.Is_Null);
    Curr := Obj.Rep.Next;
    Obj.Rep.Next := With_List.Rep;
    With_List.Rep := Curr;
  end Swap_Tail;

  procedure Tail(Obj : in out Single_List) is
    Curr : Single_Nodes.Single_Node_Ref := Obj.Rep;
  begin
    Assert (Obj.Rep /= null,
            BC.Is_Null'Identity,
            "Tail",
            BSE.Is_Null);
    Obj.Rep := Obj.Rep.Next;
    if Obj.Rep /= null then
      Obj.Rep.Count := Obj.Rep.Count + 1;
    end if;
    if Curr.Count > 1 then
      Curr.Count := Curr.Count - 1;
    else
      Single_Nodes.Delete (Curr);
    end if;
  end Tail;

  procedure Set_Head (Obj : in out Single_List; Elem : Item) is
  begin
    Assert (Obj.Rep /= null,
            BC.Is_Null'Identity,
            "Set_Head",
            BSE.Is_Null);
    Obj.Rep.Element := Elem;
  end Set_Head;

  procedure Set_Item (Obj : in out Single_List;
                      Elem : Item;
                      At_Loc : Positive) is
    Curr : Single_Nodes.Single_Node_Ref := Obj.Rep;
    Index: Positive := 1;
  begin
    while Curr /= null and then Index < At_Loc loop
      Curr := Curr.Next;
      Index := Index + 1;
    end loop;
    Assert (Curr /= null,
            BC.Range_Error'Identity,
            "Set_Item",
            BSE.Invalid_Index);
    Curr.Element := Elem;
  end Set_Item;

  function Length (Obj : Single_List) return Natural is
    Curr : Single_Nodes.Single_Node_Ref := Obj.Rep;
    Count : Natural := 0;
  begin
    while Curr /= null loop
      Curr := Curr.Next;
      Count := Count + 1;
    end loop;
    return Count;
  end Length;

  function Is_Null (Obj : Single_List) return Boolean is
  begin
    return Obj.Rep = null;
  end Is_Null;

  function Is_Shared (Obj : Single_List) return Boolean is
  begin
    return Obj.Rep /= null and then Obj.Rep.Count > 1;
  end Is_Shared;

  function Head (Obj : Single_List) return Item is
  begin
    Assert (Obj.Rep /= null,
            BC.Is_Null'Identity,
            "Head",
            BSE.Is_Null);
    return Obj.Rep.Element;
  end Head;

  function Foot (Obj : Single_List) return Item is
    Curr : Single_Nodes.Single_Node_Ref := Obj.Rep;
  begin
    Assert (Obj.Rep /= null,
            BC.Is_Null'Identity,
            "Foot",
            BSE.Is_Null);
    while Curr.Next /= null loop
      Curr := Curr.Next;
    end loop;
    return Curr.Element;
  end Foot;

  function Item_At (Obj : Single_List; Index : Positive) return Item is
--      Prev : Single_Nodes.Single_Node_Ref;
--      Curr : Single_Nodes.Single_Node_Ref := Obj.Rep;
--      Loc : Positive := 1;
--    begin
--      Assert (Obj.Rep /= null,
--              BC.Is_Null'Identity,
--              "Item_At",
--              BSE.Is_Null);
--      while Curr /= null and then Loc < Index loop
--        Curr := Curr.Next;
--        Loc := Loc + 1;
--      end loop;
--      Assert (Curr /= null,
--              BC.Range_Error'Identity,
--              "Item_At",
--              BSE.Invalid_Index);
--      return Curr.Element;
  begin
    return Item_At (Obj, Index).all;
  end Item_At;

  package Address_Conversions
  is new System.Address_To_Access_Conversions (Single_List);

  function New_Iterator (For_The_List : Single_List) return Iterator is
    P : Address_Conversions.Object_Pointer
       := Address_Conversions.To_Pointer (For_The_List'Address);
  begin
    return Iterator (SP.Create (new Single_List_Iterator (P)));
  end New_Iterator;

  function Item_At (Obj : Single_List; Index : Positive) return Item_Ptr is
    Prev : Single_Nodes.Single_Node_Ref;  -- XXX what's this for?
    Curr : Single_Nodes.Single_Node_Ref := Obj.Rep;
    Loc : Positive := 1;
  begin
    Assert (Obj.Rep /= null,
            BC.Is_Null'Identity,
            "Item_At",
            BSE.Is_Null);
    while Curr /= null and then Loc < Index loop
      Curr := Curr.Next;
      Loc := Loc + 1;
    end loop;
    Assert (Curr /= null,
            BC.Range_Error'Identity,
            "Item_At",
            BSE.Invalid_Index);
    return Curr.Element'access;
  end Item_At;

  function Cardinality (Obj : Single_List) return Natural is
  begin
    return Length (Obj);
  end Cardinality;

  procedure Initialize (Obj : in out Single_List) is
  begin
    null;
  end Initialize;

  procedure Adjust (Obj : in out Single_List) is
  begin
    if Obj.Rep /= null then
      Obj.Rep.Count := Obj.Rep.Count + 1;
    end if;
  end Adjust;

  procedure Finalize (Obj : in out Single_List) is
  begin
    Clear (Obj);
  end Finalize;

  procedure Initialize (It : in out Single_List_Iterator) is
  begin
    Reset (It);
  end Initialize;

  procedure Reset (It : in out Single_List_Iterator) is
  begin
    if Cardinality (It.L.all) = 0 then
      It.Index := 0;
    else
      It.Index := 1;
    end if;
  end Reset;

  procedure Next (It : in out Single_List_Iterator) is
  begin
    It.Index := It.Index + 1;
  end Next;

  function Is_Done (It : Single_List_Iterator) return Boolean is
  begin
    return It.Index = 0 or else It.Index > Cardinality (It.L.all);
  end Is_Done;

  function Current_Item (It : Single_List_Iterator) return Item is
  begin
    if Is_Done (It) then
      raise BC.Not_Found;
    end if;
    return Item_At (It.L.all, It.Index).all;
  end Current_Item;

  function Current_Item (It : Single_List_Iterator) return Item_Ptr is
  begin
    if Is_Done (It) then
      raise BC.Not_Found;
    end if;
    return Item_At (It.L.all, It.Index);
  end Current_Item;

end BC.Containers.Lists.Single;
