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
with System.Address_To_Access_Conversions;

package body BC.Containers.Lists.Double is

  package BSE renames BC.Support.Exceptions;
  procedure Assert
  is new BSE.Assert ("BC.Containers.Lists.Double");

  use type Double_Nodes.Double_Node_Ref;

  function "=" (L, R : Double_List) return Boolean is
  begin
    return L.Rep = R.Rep;
  end "=";

  procedure Clear (L : in out Double_List) is
    Curr : Double_Nodes.Double_Node_Ref := L.Rep;
    Ptr  : Double_Nodes.Double_Node_Ref;
  begin
    while Curr /= null loop
      Ptr := Curr;
      Curr := Curr.Next;
      if Ptr.Count > 1 then
        Ptr.Count := Ptr.Count - 1;
        exit;
      else
        if Curr /= null then
          Curr.Previous := null;
        end if;
        Double_Nodes.Delete (Ptr);
      end if;
    end loop;
    L.Rep := null;
  end Clear;

  procedure Insert (L : in out Double_List; Elem : Item) is
  begin
    Assert (L.Rep = null or else L.Rep.Previous = null,
            BC.Not_Root'Identity,
            "Insert",
            BSE.Not_Root);
    L.Rep := Double_Nodes.Create (Elem, Previous => null, Next => L.Rep);
  end Insert;

  procedure Insert (L : in out Double_List; From_List : in Double_List) is
    Ptr : Double_Nodes.Double_Node_Ref := From_List.Rep;
  begin
    Assert (L.Rep = null or else L.Rep.Previous = null,
            BC.Not_Root'Identity,
            "Insert",
            BSE.Not_Root);
    if Ptr /= null then
      while Ptr.Next /= null loop
        Ptr := Ptr.Next;
      end loop;
    end if;
    Ptr.Next := L.Rep;
    if L.Rep /= null then
      L.Rep.Previous := Ptr;
    end if;
    L.Rep := From_List.Rep;
    L.Rep.Count := L.Rep.Count + 1;
  end Insert;

  procedure Insert (L : in out Double_List;
                    Elem : Item;
                    Before : Positive) is
    Prev : Double_Nodes.Double_Node_Ref;
    Curr : Double_Nodes.Double_Node_Ref := L.Rep;
    Index : Positive := 1;
  begin
    if Curr = null or else Before = 1 then
      Insert (L, Elem);
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
      Prev.Next := Double_Nodes.Create (Elem, Previous => Prev, Next => Curr);
    end if;
  end Insert;

  procedure Insert (L : in out Double_List;
                    From_List: in out Double_List;
                    Before : Positive) is
    Prev : Double_Nodes.Double_Node_Ref;
    Curr : Double_Nodes.Double_Node_Ref := L.Rep;
    Ptr : Double_Nodes.Double_Node_Ref := From_List.Rep;
    Index : Positive := 1;
  begin
    if Ptr /= null then
      if Curr = null or else Before = 1 then
        Insert (L, From_List);
      else
        Assert (Ptr /= null or else Ptr.Previous = null,
                BC.Not_Root'Identity,
                "Insert",
                BSE.Not_Root);
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
        Curr.Previous := Ptr;
        Prev.Next := From_List.Rep;
        From_List.Rep.Previous := Prev;
        From_List.Rep.Count := From_List.Rep.Count + 1;
      end if;
    end if;
  end Insert;

  procedure Append (L : in out Double_List; Elem : Item) is
    Curr : Double_Nodes.Double_Node_Ref := L.Rep;
  begin
    if Curr /= null then
      while Curr.Next /= null loop
        Curr := Curr.Next;
      end loop;
      Curr.Next := Double_Nodes.Create (Elem, Previous => Curr, Next => null);
    else
      L.Rep := Double_Nodes.Create (Elem, Previous => null, Next => null);
    end if;
  end Append;

  procedure Append (L : in out Double_List; From_List : in Double_List) is
    Curr : Double_Nodes.Double_Node_Ref := L.Rep;
  begin
    Assert (From_List.Rep = null or else From_List.Rep.Previous = null,
            BC.Not_Root'Identity,
            "Append",
            BSE.Not_Root);
    if From_List.Rep /= null then
      if Curr /= null then
        while Curr.Next /= null loop
          Curr := Curr.Next;
        end loop;
      end if;
      if Curr /= null then
        Curr.Next := From_List.Rep;
        From_List.Rep.Previous := Curr;
      else
        L.Rep := From_List.Rep;
      end if;
      From_List.Rep.Count := From_List.Rep.Count + 1;
    end if;
  end Append;

  procedure Append (L : in out Double_List; Elem : Item; After : Positive) is
    Curr : Double_Nodes.Double_Node_Ref := L.Rep;
    Index : Positive := 1;
  begin
    if Curr = null then
      Append (L, ELem);
    else
      while Curr /= null and then Index < After loop
        Curr := Curr.Next;
        Index := Index + 1;
      end loop;
      Assert (Curr /= null,
              BC.Range_Error'Identity,
              "Append",
              BSE.Invalid_Index);
      Curr.Next := Double_Nodes.Create (Elem,
                                        Previous => Curr,
                                        Next => Curr.Next);
    end if;
  end Append;

  procedure Append (L : in out Double_List;
                    From_List : in Double_List;
                    After : Positive) is
    Curr : Double_Nodes.Double_Node_Ref := L.Rep;
    Ptr : Double_Nodes.Double_Node_Ref := From_List.Rep;
    Index : Positive := 1;
  begin
    if Ptr /= null then
      if Curr = null then
        Append (L, From_List);
      else
        Assert (From_List.Rep /= null or else
                From_List.Rep.Previous = null,
                BC.Not_Root'Identity,
                "Append",
                BSE.Not_Root);
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
        if Curr.Next /= null then
          Curr.Next.Previous := Ptr;
        end if;
        Curr.Next := From_List.Rep;
        From_List.Rep.Previous := Curr;
        From_List.Rep.Count := From_List.Rep.Count + 1;
      end if;
    end if;
  end Append;

  procedure Remove (L : in out Double_List; From : Positive) is
    Prev : Double_Nodes.Double_Node_Ref;
    Curr : Double_Nodes.Double_Node_Ref := L.Rep;
    Index : Positive := 1;
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
      L.Rep := Curr.Next;
    end if;
    if Curr.Next /= null then
      Curr.Next.Previous := Prev;
    end if;
    if Curr.Count > 1 then
      Curr.Count := Curr.Count - 1;
    else
      Double_Nodes.Delete (Curr);
    end if;
  end Remove;

  procedure Purge (L : in out Double_LIst; From : Positive) is
    Prev : Double_Nodes.Double_Node_Ref;
    Curr : Double_Nodes.Double_Node_Ref := L.Rep;
    Ptr : Double_Nodes.Double_Node_Ref;
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
      L.Rep := null;
    end if;
    while Curr /= null loop
      Curr.Previous := null;
      Ptr := Curr;
      Curr := Curr.Next;
      if Ptr.Count > 1 then
        Ptr.Count := Ptr.Count - 1;
        exit;
      else
        Double_Nodes.Delete (Ptr);
      end if;
    end loop;
  end Purge;

  procedure Purge (L : in out Double_List;
                   From : Positive;
                   Count : Positive) is
    Prev, Ptr : Double_Nodes.Double_Node_Ref;
    Curr : Double_Nodes.Double_Node_Ref := L.Rep;
    Index : Positive := 1;
    Shared_Node_Found : Boolean := False;
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
      L.Rep := null;
    end if;
    Index := 1;
    while Curr /= null and then Index <= Count loop
      Ptr := Curr;
      Curr := Curr.Next;
      if not Shared_Node_Found then
        if Ptr.Count > 1 then
          Ptr.Count := Ptr.Count - 1;
          Shared_Node_Found := True;
        else
          if Curr /= null then
            Curr.Previous := null;
            Double_Nodes.Delete (Ptr);
          end if;
        end if;
      end if;
      Index := Index + 1;
    end loop;
    if Shared_Node_Found then
      Ptr.Next := null;
    end if;
    if Curr /= null then
      Curr.Previous := Prev;
      if Prev /= null then
        Prev.Next := Curr;
      else
        L.Rep := Curr;
      end if;
    end if;
  end Purge;

  procedure Preserve (L : in out Double_List; From : Positive) is
    Temp : Double_List;
  begin
    Share (Temp, L, From);
    Share_Head (L, Temp);
  end Preserve;

  procedure Preserve (L: in out Double_List;
                      From : Positive;
                      Count : Positive) is
  begin
    Preserve (L, From);
    if Length (L) > Count then
      Purge (L, Count + 1); -- we start at 1, remember!
    end if;
  end Preserve;

  procedure Share (L : in out Double_List;
                   With_List: Double_List;
                   Starting_At : Positive) is
    Ptr : Double_Nodes.Double_Node_Ref := With_List.Rep;
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
    Clear (L);
    L.Rep := Ptr;
    L.Rep.Count := L.Rep.Count + 1;
  end Share;

  procedure Share_Head (L : in out Double_List;
                        With_List : in Double_List) is
  begin
    Assert (With_List.Rep /= null,
            BC.Is_Null'Identity,
            "Share_Head",
            BSE.Is_Null);
    Clear (L);
    L.Rep := With_List.Rep;
    L.Rep.Count := L.Rep.Count + 1;
  end Share_Head;

  procedure Share_Foot (L : in out Double_List;
                        With_List : in Double_List) is
    Ptr : Double_Nodes.Double_Node_Ref := With_List.Rep;
  begin
    Assert (Ptr /= null,
            BC.Is_Null'Identity,
            "Share_Foot",
            BSE.Is_Null);
    Clear (L);
    while Ptr.Next /= null loop
      Ptr := Ptr.Next;
    end loop;
    L.Rep := Ptr;
    L.Rep.Count := L.Rep.Count + 1;
  end Share_Foot;

  procedure Swap_Tail (L : in out Double_List;
                       With_List : in out Double_List) is
    Curr : Double_Nodes.Double_Node_Ref;
  begin
    Assert (L.Rep /= null,
            BC.Is_Null'Identity,
            "Swap_Tail",
            BSE.Is_Null);
    Assert (With_List.Rep = null or else With_List.Rep.Previous = null,
            BC.Not_Root'Identity,
            "Swap_Tail",
            BSE.Not_Root);
    Curr := L.Rep.Next;
    L.Rep.Next := With_List.Rep;
    With_List.Rep.Previous := L.Rep;
    With_List.Rep := Curr;
    if With_List.Rep /= null then
      With_List.Rep.Previous := null;
    end if;
  end Swap_Tail;

  procedure Tail (L : in out Double_List) is
    Curr : Double_Nodes.Double_Node_Ref := L.Rep;
  begin
    Assert (L.Rep /= null,
            BC.Is_Null'Identity,
            "Tail",
            BSE.Is_Null);
    L.Rep := L.Rep.Next;
    if L.Rep /= null then
      L.Rep.Count := L.Rep.Count + 1;
    end if;
    if Curr.Count > 1 then
      Curr.Count := Curr.Count - 1;
    else
      if L.Rep /= null then
        L.Rep.Count := L.Rep.Count - 1;
        L.Rep.Previous := null;
      end if;
      Double_Nodes.Delete (Curr);
    end if;
  end Tail;

  procedure Predecessor (L : in out Double_List) is
  begin
    Assert (L.Rep /= null,
            BC.Is_Null'Identity,
            "Predecessor",
            BSE.Is_Null);
    if L.Rep.Previous = null then
      Clear (L);
    else
      L.Rep.Count := L.Rep.Count - 1;
      L.Rep := L.Rep.Previous;
      L.Rep.Count := L.Rep.Count + 1;
    end if;
  end Predecessor;

  procedure Set_Head (L : in out Double_List; Elem : Item) is
  begin
    Assert (L.Rep /= null,
            BC.Is_Null'Identity,
            "Set_Head",
            BSE.Is_Null);
    L.Rep.Element := ELem;
  end Set_Head;

  procedure Set_Item (L : in out Double_List;
                      Elem : Item;
                      At_Loc : Positive) is
    Curr : Double_Nodes.Double_Node_Ref := L.Rep;
    Index : Positive := 1;
  begin
    while Curr /= null and then Index < At_Loc loop
      Curr := Curr.Next;
      Index := Index + 1;
    end loop;
    Assert (Curr /= null,
            BC.Range_Error'Identity,
            "Set_Item",
            BSE.Invalid_Index);
    Curr.Element := ELem;
  end Set_Item;

  function Length (L : Double_List) return Natural is
    Curr : Double_Nodes.Double_Node_Ref := L.Rep;
    Count : Natural := 0;
  begin
    while Curr /= null loop
      Curr := Curr.Next;
      Count := Count + 1;
    end loop;
    return Count;
  end Length;

  function Is_Null (L : Double_List) return Boolean is
  begin
    return L.Rep = null;
  end Is_Null;

  function Is_Shared (L : Double_List) return Boolean is
  begin
    if L.Rep /= null then
      return L.Rep.Count > 1;
    else
      return False;
    end if;
  end Is_Shared;

  function Is_Head (L : Double_List) return Boolean is
  begin
    return L.Rep = null or else L.Rep.Previous = null;
  end Is_Head;

  function Head (L : Double_List) return Item is
  begin
    Assert (L.Rep /= null,
            BC.Is_Null'Identity,
            "Head",
            BSE.Is_Null);
    return L.Rep.Element;
  end Head;

  function Foot (L : Double_List) return Item is
    Curr : Double_Nodes.Double_Node_Ref := L.Rep;
  begin
    Assert (L.Rep /= null,
            BC.Is_Null'Identity,
            "Foot",
            BSE.Is_Null);
    while Curr.Next /= null loop
      Curr := Curr.Next;
    end loop;
    return Curr.Element;
  end Foot;

  function Item_At (L : Double_List; Index : Positive) return Item is
  begin
    return Item_At (L, Index).all;
  end Item_At;

  package Address_Conversions
  is new System.Address_To_Access_Conversions (Double_List);

  function New_Iterator (For_The_List : Double_List) return Iterator is
    P : Address_Conversions.Object_Pointer
       := Address_Conversions.To_Pointer (For_The_List'Address);
  begin
    return Iterator (SP.Create (new Double_List_Iterator (P)));
  end New_Iterator;

  function Cardinality (L : Double_List) return Natural is
  begin
    return Length (L);
  end Cardinality;

  function Item_At (L : Double_List; Index : Positive) return Item_Ptr is
    Curr : Double_Nodes.Double_Node_Ref := L.Rep;
    Loc : Positive := 1;
  begin
    Assert (L.Rep /= null,
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

  procedure Initialize (L : in out Double_List) is
  begin
    null;
  end Initialize;

  procedure Adjust (L : in out Double_List) is
  begin
    if L.Rep /= null then
      L.Rep.Count := L.Rep.Count + 1;
    end if;
  end Adjust;

  procedure Finalize (L : in out Double_List) is
  begin
    Clear (L);
  end Finalize;

  procedure Initialize (It : in out Double_List_Iterator) is
  begin
    Reset (It);
  end Initialize;

  procedure Reset (It : in out Double_List_Iterator) is
  begin
    It.Index := It.L.Rep;
  end Reset;

  procedure Next (It : in out Double_List_Iterator) is
  begin
    if It.Index /= null then
      It.Index := It.Index.Next;
    end if;
  end Next;

  function Is_Done (It : Double_List_Iterator) return Boolean is
  begin
    return It.Index = null;
  end Is_Done;

  function Current_Item (It : Double_List_Iterator) return Item is
  begin
    if Is_Done (It) then
      raise BC.Not_Found;
    end if;
    return It.Index.Element;
  end Current_Item;

  function Current_Item (It : Double_List_Iterator) return Item_Ptr is
  begin
    if Is_Done (It) then
      raise BC.Not_Found;
    end if;
    return It.Index.Element'Access;
  end Current_Item;

  procedure Delete_Item_At (It : Double_List_Iterator) is
    Prev : Double_Nodes.Double_Node_Ref;
    Curr : Double_Nodes.Double_Node_Ref := It.L.Rep;
  begin
    if Is_Done (It) then
      raise BC.Not_Found;
    end if;
    while Curr /= null and then Curr /= It.Index loop
      Prev := Curr;
      Curr := Curr.Next;
    end loop;
    Assert (Curr /= null,
            BC.Range_Error'Identity,
            "Delete_Item_At",
            BSE.Invalid_Index);
    It.Relay.Reference.Index := Curr.Next;
    if Prev /= null then
      Prev.Next := Curr.Next;
    else
      It.L.Rep := Curr.Next;
    end if;
    if Curr.Next /= null then
      Curr.Next.Previous := Prev;
    end if;
    if Curr.Count > 1 then
      Curr.Count := Curr.Count - 1;
    else
      Double_Nodes.Delete (Curr);
    end if;
  end Delete_Item_At;

end BC.Containers.Lists.Double;
