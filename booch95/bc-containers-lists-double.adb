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

package body Bc.Containers.Lists.Double is

  use Double_Nodes;

  function "=" (L, R : Double_List) return Boolean is
  begin
    return L.Rep = R.Rep;
  end "=";

  procedure Clear (Obj : in out Double_List) is
    Curr : Double_Node_Ref := Obj.Rep;
    Ptr  : Double_Node_Ref;
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
        Delete (Ptr);
      end if;
    end loop;
    Obj.Rep := null;
  end Clear;

  procedure Insert (Obj : in out Double_List; Elem : Item) is
  begin
    pragma Assert (Obj.Rep = null or else Obj.Rep.Previous = null,
                   "Attempt to Insert when not at Root");
    Obj.Rep := Create (Elem, Previous => null, Next => Obj.Rep);
  end Insert;

  procedure Insert (Obj : in out Double_List; From_List : in Double_List) is
    Ptr : Double_Node_Ref := From_List.Rep;
  begin
    pragma Assert (Obj.Rep = null or else Obj.Rep.Previous = null,
                   "Attempt to Insert when not at Root");
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
                    Before : Natural) is
    Prev : Double_Node_Ref;
    Curr : Double_Node_Ref := Obj.Rep;
    Index: Natural := 0;
  begin
    if Curr = null or else Before = 0 then
      Insert (Obj, Elem);
    else
      while Curr /= null and then Index < Before loop
        Prev := Curr;
        Curr := Curr.Next;
        Index := Index + 1;
      end loop;
      pragma Assert (Curr /= null, "Attempt to Insert at NULL location");
      Prev.Next := Create (Elem, Previous => Prev, Next => Curr);
    end if;
  end Insert;

  procedure Insert (Obj : in out Double_List;
                    From_List: in out Double_List;
                    Before : Natural) is
    Prev : Double_Node_Ref;
    Curr : Double_Node_Ref := Obj.Rep;
    Ptr : Double_Node_Ref := From_List.Rep;
    Index: Natural := 0;
  begin
    if Ptr /= null then
      if Curr = null or else Before = 0 then
        Insert (Obj, From_List);
      else
        pragma Assert (Ptr /= null or else Ptr.Previous = null,
                       "Attempt to Insert when Tree isn't root");
        while Curr /= null and then Index < Before loop
          Prev := Curr;
          Curr := Curr.Next;
          Index := Index + 1;
        end loop;
        pragma Assert (Curr /= null, "Attempt to Insert at NULL location");
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
    Curr : Double_Node_Ref := Obj.Rep;
  begin
    if Curr /= null then
      while Curr.Next /= null loop
        Curr := Curr.Next;
      end loop;
    end if;
    if Curr /= null then
      Curr.Next := Create (Elem, Previous => Curr, Next => null);
    else
      Obj.Rep := Create (Elem, Previous => null, Next => null);
    end if;
  end Append;

  procedure Append (Obj : in out Double_List; From_List : in Double_List) is
    Prev : Double_Node_Ref;
    Curr : Double_Node_Ref := Obj.Rep;
  begin
    pragma Assert (From_List.Rep /= null
                   or else From_List.Rep.Previous /= null,
                   "Attempt to Append to a location that is not ROOT");
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

  procedure Append (Obj : in out Double_List; Elem : Item; After : Natural) is
    Curr : Double_Node_Ref := Obj.Rep;
    Index : Natural := 0;
  begin
    if Curr = null then
      Append (Obj, ELem);
    else
      while Curr /= null and then Index < After loop
        Curr := Curr.Next;
        Index := Index + 1;
      end loop;
      pragma Assert (Curr /= null, "Attempt to Insert at NULL location");
      Curr.Next := Create (Elem, Previous => Curr, Next => Curr.Next);
    end if;
  end Append;

  procedure Append (Obj : in out Double_List;
                    From_List : in Double_List;
                    After : Natural) is
    Curr : Double_Node_Ref := Obj.Rep;
    Ptr : Double_Node_Ref := From_List.Rep;
    Index : Natural := 0;
  begin
    if Ptr /= null then
      if Curr = null then
        Append (Obj, From_List);
      else
        pragma Assert (From_List.Rep /= null or else
                       From_List.Rep.Previous = null,
                       "Attempt to Insert at NULL location");
        while Curr /= null and then Index < After loop
          Curr := Curr.Next;
          Index := Index + 1;
        end loop;
        pragma Assert (Curr /= null, "Attempt to Insert with invalid Index");
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

  procedure Remove (Obj : in out Double_List; From : Natural) is
    Prev : Double_Node_Ref;
    Curr : Double_Node_Ref := Obj.Rep;
    Index : Natural := 0;
  begin
    while Curr /= null and then Index < From loop
      Prev := Curr;
      Curr := Curr.Next;
      Index := Index + 1;
    end loop;
    pragma Assert(Curr /= null, "Attempt to Remove from Invalid location");
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
      Delete (Curr);
    end if;
  end Remove;

  procedure Purge (Obj : in out Double_LIst; From : Natural) is
    Prev : Double_Node_Ref;
    Curr : Double_Node_Ref := Obj.Rep;
    Ptr : Double_Node_Ref;
    Index : Natural := 0;
  begin
    while Curr /= null and then Index < From loop
      Prev := Curr;
      Curr := Curr.Next;
      Index := Index + 1;
    end loop;
    pragma Assert (Curr /= null, "Attempt to Purge with Invalid Index");
    if Prev /= null then
      Prev.Next := null;
    else
      Obj.Rep := null;
    end if;
    while Curr /= null loop
      Curr.Previous := null;
      Ptr := CUrr;
      Curr := Curr.Next;
      if Ptr.Count > 1 then
        Ptr.Count := Ptr.Count - 1;
        exit;
      else
        Delete (Ptr);
      end if;
    end loop;
  end Purge;

  procedure Purge (Obj : in out Double_List;
                   From : Natural;
                   Count : Positive) is
    Prev, Ptr : Double_Node_Ref;
    Curr : Double_Node_Ref := Obj.Rep;
    Index : Natural := 0;
    Cut : Boolean := True;
  begin
    while Curr /= null and then Index < From loop
      Prev := Curr;
      Curr := Curr.Next;
      Index := Index + 1;
    end loop;
    pragma Assert (Curr /= null, "Attempt to Purge with Invalid Index");
    if Prev /= null then
      Prev.Next := null;
    else
      Obj.Rep := null;
    end if;
    Index := 0;
    while Curr /= null and then Index < Count loop
      Ptr := Curr;
      Curr := Curr.Next;
      if Cut then
        if Ptr.Count > 1 then
          Ptr.Count := Ptr.Count - 1;
          Cut := False;
        else
          if Curr /= null then
            Curr.Previous := null;
            Delete (Ptr);
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

  procedure Preserve (Obj : in out Double_List; From : Natural) is
    Temp : Double_List;
  begin
    Share (Temp, Obj, From);
    Share_Head (Obj, Temp);
  end Preserve;

  procedure Preserve (Obj: in out Double_List;
                      From : Natural;
                      Count : Positive) is
  begin
    Preserve (Obj, From);
    if Length (Obj) > Count then
      Purge (Obj, Count);
    end if;
  end Preserve;

  procedure Share (Obj : in out Double_List;
                   With_List: Double_List;
                   Starting_At : Natural) is
    Ptr : Double_Node_Ref := With_List.Rep;
    Index : Natural := 0;
  begin
    pragma Assert (Ptr /= null, "Attempt to Share with NULL pointer");
    while Ptr /= null and then Index < Starting_At loop
      Ptr := Ptr.Next;
      Index := Index + 1;
    end loop;
    pragma Assert (Ptr /= null, "Share: NULL pointer problems");
    Clear (Obj);
    Obj.Rep := Ptr;
    Obj.Rep.Count := Obj.Rep.Count + 1;
  end Share;

  procedure Share_Head (Obj : in out Double_List;
                        With_List : in Double_List) is
  begin
    pragma Assert (With_List.Rep /= null, "Attempt to Share with NULL list");
    Clear (Obj);
    Obj.Rep := With_List.Rep;
    Obj.Rep.Count := Obj.Rep.Count + 1;
  end Share_Head;

  procedure Share_Foot (Obj : in out Double_List;
                        With_List : in Double_List) is
    Ptr : Double_Node_Ref := With_List.Rep;
  begin
    pragma Assert (Ptr /= null, "Attempt to Share_Foot with NULL list");
    Clear (Obj);
    while Ptr.Next /= null loop
      Ptr := Ptr.Next;
    end loop;
    Obj.Rep := Ptr;
    Obj.Rep.Count := Obj.Rep.Count + 1;
  end Share_Foot;

  procedure Swap_Tail (Obj : in out Double_List;
                       With_List : in out Double_List) is
    pragma Assert (Obj.Rep /= null, "Attempt to Swap_Tail with NULL tree");
    pragma Assert (With_List.Rep = null or else With_List.Rep.Previous = null,
                   "Attempt to Swap_Tail with non-root new List");
    Curr : Double_Node_Ref := Obj.Rep.Next;
  begin
    Obj.Rep.Next := With_List.Rep;
    With_List.Rep.Previous := Obj.Rep;
    With_List.Rep := Curr;
    if With_List.Rep /= null then
      With_List.Rep.Previous := null;
    end if;
  end Swap_Tail;

  procedure Tail (Obj : in out Double_List) is
    Curr : Double_Node_Ref := Obj.Rep;
  begin
    pragma Assert (Obj.Rep /= null, "Attempt to Tail with NULL tree");
    Obj.Rep := Obj.Rep.Next;
    if Obj.Rep /= null then
      Obj.Rep.Count := Obj.Rep.Count + 1;
    end if;
    if Curr.Count > 1 then
      Curr.Count := Curr.Count - 1;
    else
      if Obj.Rep /= null then
        Obj.Rep.Previous := null;
      end if;
      Delete (Curr);
    end if;
  end Tail;

  procedure Predecessor (Obj : in out Double_List) is
  begin
    pragma Assert (Obj.Rep /= null, "Attempt to Predecessor with NULL tree");
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
    pragma Assert (Obj.Rep /= null, "Attempt to Set_Head with NULL tree");
    Obj.Rep.Element := ELem;
  end Set_Head;

  procedure Set_Item (Obj : in out Double_List;
                      Elem : Item;
                      At_Loc : Natural) is
    Curr : Double_Node_Ref := Obj.Rep;
    Index : Natural := 0;
  begin
    while Curr /= null and then Index < At_Loc loop
      Curr := Curr.Next;
      Index := Index + 1;
    end loop;
    pragma Assert (Curr /= null, "Attempt to Set_Item with invalid index");
    Curr.Element := ELem;
  end Set_Item;

  function Length (Obj : Double_List) return Natural is
    Curr : Double_Node_Ref := Obj.Rep;
    Index : Natural := 0;
  begin
    while Curr /= null loop
      Curr := Curr.Next;
      Index := Index + 1;
    end loop;
    return Index;
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
    pragma Assert (Obj.Rep /= null, "Attempt to get Head with NULL tree");
    return Obj.Rep.Element;
  end Head;

  function Head (Obj : Double_List) return Item_Ptr is
  begin
    pragma Assert (Obj.Rep /= null, "Attempt to get Head with NULL tree");
    return Obj.Rep.Element'access;
  end Head;

  function Foot (Obj : Double_List) return Item is
    Curr : Double_Node_Ref := Obj.Rep;
  begin
    pragma Assert (Obj.Rep /= null, "Attempt to get Foot with NULL tree");
    while Curr.Next /= null loop
      Curr := Curr.Next;
    end loop;
    return Curr.Element;
  end Foot;

  function Foot (Obj : Double_List) return Item_Ptr is
    Curr : Double_Node_Ref := Obj.Rep;
  begin
    pragma Assert (Obj.Rep /= null, "Attempt to get Foot with NULL tree");
    while Curr.Next /= null loop
      Curr := Curr.Next;
    end loop;
    return Curr.Element'access;
  end Foot;

  function Item_At (Obj : Double_List; Index : Natural) return Item is
    Curr : Double_Node_Ref := Obj.Rep;
    Loc : Natural := 0;
  begin
    pragma Assert (Obj.Rep /= null, "Attempt to get Item with NULL tree");
    while Curr /= null and then Loc < Index loop
      Curr := Curr.Next;
      Loc := Loc + 1;
    end loop;
    pragma Assert (Curr /= null, "Attempt to get Item with Invalid Index");
    return Curr.Element;
  end Item_At;

  function Item_At (Obj : Double_List; Index : Natural) return Item_Ptr is
    Curr : Double_Node_Ref := Obj.Rep;
    Loc : Natural := 0;
  begin
    pragma Assert (Obj.Rep /= null, "Attempt to get Item with NULL tree");
    while Curr /= null and then Loc < Index loop
      Curr := Curr.Next;
      Loc := Loc + 1;
    end loop;
    pragma Assert (Curr /= null, "Attempt to get Item with Invalid Index");
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
