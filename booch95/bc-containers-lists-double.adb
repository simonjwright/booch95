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

package body BC.Containers.Lists.Double is

  package BSE renames BC.Support.Exceptions;
  procedure Assert
  is new BSE.Assert ("BC.Containers.Lists.Double");

  use type Double_Nodes.Double_Node_Ref;

  function "=" (L, R : Double_List) return Boolean is
  begin
    return L.Rep = R.Rep;
  end "=";

  procedure Clear (Obj : in out Double_List) is
    Curr : Double_Nodes.Double_Node_Ref := Obj.Rep;
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
    Obj.Rep := null;
  end Clear;

  procedure Insert (Obj : in out Double_List; Elem : Item) is
  begin
    Assert (Obj.Rep = null or else Obj.Rep.Previous = null,
            BC.Not_Root'Identity,
            "Insert",
            BSE.Not_Root);
    Obj.Rep := Double_Nodes.Create (Elem, Previous => null, Next => Obj.Rep);
  end Insert;

  procedure Insert (Obj : in out Double_List; From_List : in Double_List) is
    Ptr : Double_Nodes.Double_Node_Ref := From_List.Rep;
  begin
    Assert (Obj.Rep = null or else Obj.Rep.Previous = null,
            BC.Not_Root'Identity,
            "Insert",
            BSE.Not_Root);
    if Ptr /= null then
      while Ptr.Next /= null loop
        Ptr := Ptr.Next;
      end loop;
    end if;
    Ptr.Next := Obj.Rep;
    if Obj.Rep /= null then
      Obj.Rep.Previous := Ptr;
    end if;
    Obj.Rep := From_List.Rep;
    Obj.Rep.Count := Obj.Rep.Count + 1;
  end Insert;

  procedure Insert (Obj : in out Double_List;
                    Elem : Item;
                    Before : Positive) is
    Prev : Double_Nodes.Double_Node_Ref;
    Curr : Double_Nodes.Double_Node_Ref := Obj.Rep;
    Index : Positive := 1;
  begin
    if Curr = null or else Before = 1 then
      Insert (Obj, Elem);
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

  procedure Insert (Obj : in out Double_List;
                    From_List: in out Double_List;
                    Before : Positive) is
    Prev : Double_Nodes.Double_Node_Ref;
    Curr : Double_Nodes.Double_Node_Ref := Obj.Rep;
    Ptr : Double_Nodes.Double_Node_Ref := From_List.Rep;
    Index : Positive := 1;
  begin
    if Ptr /= null then
      if Curr = null or else Before = 1 then
        Insert (Obj, From_List);
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

  procedure Append (Obj : in out Double_List; Elem : Item) is
    Curr : Double_Nodes.Double_Node_Ref := Obj.Rep;
  begin
    if Curr /= null then
      while Curr.Next /= null loop
        Curr := Curr.Next;
      end loop;
      Curr.Next := Double_Nodes.Create (Elem, Previous => Curr, Next => null);
    else
      Obj.Rep := Double_Nodes.Create (Elem, Previous => null, Next => null);
    end if;
  end Append;

  procedure Append (Obj : in out Double_List; From_List : in Double_List) is
    Prev : Double_Nodes.Double_Node_Ref;
    Curr : Double_Nodes.Double_Node_Ref := Obj.Rep;
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
        Obj.Rep := From_List.Rep;
      end if;
      From_List.Rep.Count := From_List.Rep.Count + 1;
    end if;
  end Append;

  procedure Append (Obj : in out Double_List; Elem : Item; After : Positive) is
    Curr : Double_Nodes.Double_Node_Ref := Obj.Rep;
    Index : Positive := 1;
  begin
    if Curr = null then
      Append (Obj, ELem);
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

  procedure Append (Obj : in out Double_List;
                    From_List : in Double_List;
                    After : Positive) is
    Curr : Double_Nodes.Double_Node_Ref := Obj.Rep;
    Ptr : Double_Nodes.Double_Node_Ref := From_List.Rep;
    Index : Positive := 1;
  begin
    if Ptr /= null then
      if Curr = null then
        Append (Obj, From_List);
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

  procedure Remove (Obj : in out Double_List; From : Positive) is
    Prev : Double_Nodes.Double_Node_Ref;
    Curr : Double_Nodes.Double_Node_Ref := Obj.Rep;
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
      Obj.Rep := Curr.Next;
    end if;
    if Curr.Next /= null then
      Curr.Next.Previous := Prev;
    end if;
    if Curr.Count > 1 then
      Curr.Count := Curr.Count - 1;
      Obj.Rep := Curr.Next;
      Curr.Next := null;
    else
      Double_Nodes.Delete (Curr);
    end if;
  end Remove;

  procedure Purge (Obj : in out Double_LIst; From : Positive) is
    Prev : Double_Nodes.Double_Node_Ref;
    Curr : Double_Nodes.Double_Node_Ref := Obj.Rep;
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
      Obj.Rep := null;
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

  procedure Purge (Obj : in out Double_List;
                   From : Positive;
                   Count : Positive) is
    Prev, Ptr : Double_Nodes.Double_Node_Ref;
    Curr : Double_Nodes.Double_Node_Ref := Obj.Rep;
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
          if Curr /= null then
            Curr.Previous := null;
            Double_Nodes.Delete (Ptr);
          end if;
        end if;
      end if;
      Index := Index + 1;
    end loop;
    Ptr.Next := null;
    if Curr /= null then
      Curr.Previous := Prev;
      if Prev /= null then
        Prev.Next := Curr;
      else
        Obj.Rep := Curr;
      end if;
    end if;
  end Purge;

  procedure Preserve (Obj : in out Double_List; From : Positive) is
    Temp : Double_List;
  begin
    Share (Temp, Obj, From);
    Share_Head (Obj, Temp);
  end Preserve;

  procedure Preserve (Obj: in out Double_List;
                      From : Positive;
                      Count : Positive) is
  begin
    Preserve (Obj, From);
    if Length (Obj) > Count then
      Purge (Obj, Count + 1); -- we start at 1, remember!
    end if;
  end Preserve;

  procedure Share (Obj : in out Double_List;
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
    Clear (Obj);
    Obj.Rep := Ptr;
    Obj.Rep.Count := Obj.Rep.Count + 1;
  end Share;

  procedure Share_Head (Obj : in out Double_List;
                        With_List : in Double_List) is
  begin
    Assert (With_List.Rep /= null,
            BC.Is_Null'Identity,
            "Share_Head",
            BSE.Is_Null);
    Clear (Obj);
    Obj.Rep := With_List.Rep;
    Obj.Rep.Count := Obj.Rep.Count + 1;
  end Share_Head;

  procedure Share_Foot (Obj : in out Double_List;
                        With_List : in Double_List) is
    Ptr : Double_Nodes.Double_Node_Ref := With_List.Rep;
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

  procedure Swap_Tail (Obj : in out Double_List;
                       With_List : in out Double_List) is
    Curr : Double_Nodes.Double_Node_Ref;
  begin
    Assert (Obj.Rep /= null,
            BC.Is_Null'Identity,
            "Swap_Tail",
            BSE.Is_Null);
    Assert (With_List.Rep = null or else With_List.Rep.Previous = null,
            BC.Not_Root'Identity,
            "Swap_Tail",
            BSE.Not_Root);
    Curr := Obj.Rep.Next;
    Obj.Rep.Next := With_List.Rep;
    With_List.Rep.Previous := Obj.Rep;
    With_List.Rep := Curr;
    if With_List.Rep /= null then
      With_List.Rep.Previous := null;
    end if;
  end Swap_Tail;

  procedure Tail (Obj : in out Double_List) is
    Curr : Double_Nodes.Double_Node_Ref := Obj.Rep;
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
      if Obj.Rep /= null then
	Obj.Rep.Count := Obj.Rep.Count - 1;
        Obj.Rep.Previous := null;
      end if;
      Double_Nodes.Delete (Curr);
    end if;
  end Tail;

  procedure Predecessor (Obj : in out Double_List) is
  begin
    Assert (Obj.Rep /= null,
            BC.Is_Null'Identity,
            "Predecessor",
            BSE.Is_Null);
    if Obj.Rep.Previous = null then
      Clear (Obj);
    else
      Obj.Rep.Count := Obj.Rep.Count - 1;
      Obj.Rep := Obj.Rep.Previous;
      Obj.Rep.Count := Obj.Rep.Count + 1;
    end if;
  end Predecessor;

  procedure Set_Head (Obj : in out Double_List; Elem : Item) is
  begin
    Assert (Obj.Rep /= null,
            BC.Is_Null'Identity,
            "Set_Head",
            BSE.Is_Null);
    Obj.Rep.Element := ELem;
  end Set_Head;

  procedure Set_Item (Obj : in out Double_List;
                      Elem : Item;
                      At_Loc : Positive) is
    Curr : Double_Nodes.Double_Node_Ref := Obj.Rep;
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

  function Length (Obj : Double_List) return Natural is
    Curr : Double_Nodes.Double_Node_Ref := Obj.Rep;
    Count : Natural := 0;
  begin
    while Curr /= null loop
      Curr := Curr.Next;
      Count := Count + 1;
    end loop;
    return Count;
  end Length;

  function Is_Null (Obj : Double_List) return Boolean is
  begin
    return Obj.Rep = null;
  end Is_Null;

  function Is_Shared (Obj : Double_List) return Boolean is
  begin
    if Obj.Rep /= null then
      return Obj.Rep.Count > 1;
    else
      return False;
    end if;
  end Is_Shared;

  function Is_Head (Obj : Double_List) return Boolean is
  begin
    return Obj.Rep = null or else Obj.Rep.Previous = null;
  end Is_Head;

  function Head (Obj : Double_List) return Item is
  begin
    Assert (Obj.Rep /= null,
            BC.Is_Null'Identity,
            "Head",
            BSE.Is_Null);
    return Obj.Rep.Element;
  end Head;

  function Head (Obj : Double_List) return Item_Ptr is
  begin
    Assert (Obj.Rep /= null,
            BC.Is_Null'Identity,
            "Head",
            BSE.Is_Null);
    return Obj.Rep.Element'access;
  end Head;

  function Foot (Obj : Double_List) return Item is
    Curr : Double_Nodes.Double_Node_Ref := Obj.Rep;
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

  function Foot (Obj : Double_List) return Item_Ptr is
    Curr : Double_Nodes.Double_Node_Ref := Obj.Rep;
  begin
    Assert (Obj.Rep /= null,
            BC.Is_Null'Identity,
            "Foot",
            BSE.Is_Null);
    while Curr.Next /= null loop
      Curr := Curr.Next;
    end loop;
    return Curr.Element'access;
  end Foot;

  function Item_At (Obj : Double_List; Index : Positive) return Item is
    Curr : Double_Nodes.Double_Node_Ref := Obj.Rep;
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
    return Curr.Element;
  end Item_At;

  function Item_At (Obj : Double_List; Index : Natural) return Item_Ptr is
    Curr : Double_Nodes.Double_Node_Ref := Obj.Rep;
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

  function Cardinality (Obj : Double_List) return Integer is
  begin
    return Length (Obj);
  end Cardinality;

  procedure Initialize (Obj : in out Double_List) is
  begin
    null;
  end Initialize;

  procedure Adjust (Obj : in out Double_List) is
  begin
    if Obj.Rep /= null then
      Obj.Rep.Count := Obj.Rep.Count + 1;
    end if;
  end Adjust;

  procedure Finalize (Obj : in out Double_List) is
  begin
    Clear (Obj);
  end Finalize;

end Bc.Containers.Lists.Double;
