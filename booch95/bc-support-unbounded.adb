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

with Ada.Unchecked_Deallocation;

package body BC.Support.Unbounded is

  use type Nodes.Node_Ref;

  procedure Free is new
     Ada.Unchecked_Deallocation (Nodes.Node, Nodes.Node_Ref);

  function Create (From : Unb_Node) return Unb_Node is
    Obj : Unb_Node := From;
    Tmp : Nodes.Node_Ref := Obj.Last;
  begin
    if Tmp /= null then
      Obj.Last := new Nodes.Node'(Element => Tmp.Element,
                                  Next => null,
                                  Previous => null);
      Obj.Rep := Obj.Last;
      Tmp := Tmp.Previous;  -- move to previous node from orig list
      while Tmp /= null loop
        Obj.Rep := new Nodes.Node'(Element => Tmp.Element,
                                   Next => Obj.Rep,
                                   Previous => null);
        Tmp := Tmp.Previous;
      end loop;
    end if;
    return Obj;
  end Create;

  function "=" (Left, Right : in Unb_Node) return Boolean is
  begin
    if Left.Size = Right.Size then
      declare
        Temp_L : Nodes.Node_Ref := Left.Rep;
        Temp_R : Nodes.Node_Ref := Right.Rep;
      begin
        while Temp_L /= null loop
          if Temp_L.Element /= Temp_R.Element then
            return False;
          end if;
          Temp_L := Temp_L.Next;
          Temp_R := Temp_R.Next;
        end loop;
        return True;
      end;
    else
      return False;
    end if;
  end "=";

  procedure Clear (Obj : in out Unb_Node) is
    Empty_Node : Unb_Node;
    Ptr : Nodes.Node_Ref;
  begin
    while Obj.Rep /= null loop
      Ptr := Obj.Rep;
      Obj.Rep := Obj.Rep.Next;
      Free (Ptr);
    end loop;
    Obj := Empty_Node;
  end Clear;

  procedure Insert (Obj : in out Unb_Node; Elem : Item) is
  begin
    Obj.Rep := new Nodes.Node'(Elem, Obj.Rep, null);
    if Obj.Last.Previous /= null then
      Obj.Last.Previous.Next := Obj.Last;
    end if;
    if Obj.Last = null then
      Obj.Last := Obj.Rep;
    end if;
    Obj.Size := Obj.Size + 1;
    Obj.Cache := Obj.Rep;
    Obj.Cache_Index := 1;
  end Insert;

  procedure Insert (Obj : in out Unb_Node; Elem : Item; Before : Natural) is
  begin
    pragma Assert (Before <= Obj.Size);
    if (Obj.Size = 0) or else (Before <= 1) then
      Insert (Obj,Elem);
    else
      declare
        AObj      : aliased Unb_Node := Obj;
        Temp_Item : Item := Item_At (AObj'access, Before);  -- loads cache
        Temp_Node : Nodes.Node_Ref;
      begin
        Temp_Node := new Nodes.Node'(Elem, Obj.Cache, Obj.Cache.Previous);
        if Temp_Node.Previous /= null then
          Temp_Node.Previous.Next := Temp_Node;
        else
          Obj.Rep := Temp_Node;
        end if;
        Obj.Size := Obj.Size + 1;
        Obj.Cache := Temp_Node;
      end;
    end if;
  end Insert;

  procedure Append (Obj : in out Unb_Node; Elem : Item) is
  begin
    Obj.Last := new Nodes.Node'(Elem, null, Obj.Last);
    if Obj.Last.Previous /= null then
      Obj.Last.Previous.Next := Obj.Last;
    end if;
    if Obj.Rep = null then
      Obj.Rep := Obj.Last;
    end if;
    Obj.Size := Obj.Size + 1;
    Obj.Cache := Obj.Last;
    Obj.Cache_Index := Obj.Size;
  end Append;

  procedure Append (Obj : in out Unb_Node; Elem : Item; After : Natural) is
  begin
    pragma Assert (After <= Obj.Size);
    if (Obj.Size = 0) or else (After <= 1) then
      Append(Obj, Elem);
    else
      declare
        AObj      : aliased Unb_Node := Obj;
        Temp_Item : Item := Item_At (AObj'access, After);  -- loads cache
        Temp_Node : Nodes.Node_Ref;
      begin
        Temp_Node := new Nodes.Node'(Elem, Obj.Cache.Next, Obj.Cache);
        if Temp_Node.Previous /= null then
          Temp_Node.Previous.Next := Temp_Node;
        end if;
        if Temp_Node.Next = null then
          Obj.Last := Temp_Node;
        end if;
        Obj.Size := Obj.Size + 1;
        Obj.Cache := Temp_Node;
        Obj.Cache_Index := Obj.Cache_Index + 1;
      end;
    end if;
  end Append;

  procedure Remove (Obj : in out Unb_Node; From : Natural) is
  begin
    pragma Assert (From <= Obj.Size );
    pragma Assert (Obj.Size > 0 );
    if Obj.Size = 1 then
      Clear (Obj);
    else
      declare
        AObj      : aliased Unb_Node := Obj;
        Temp_Item : Item := Item_At (AObj'access, From);
        Ptr : Nodes.Node_Ref := AObj.Cache;
      begin
        if Ptr.Previous = null then
          AObj.Rep := Ptr.Next;
        else
          Ptr.Previous.Next := Ptr.Next;
        end if;
        if Ptr.Next = null then
          AObj.Last := Ptr.Previous;
        else
          Ptr.Next.Previous := Ptr.Previous;
        end if;
        AObj.Size := AObj.Size - 1;
        if Ptr.Next /= null then
          AObj.Cache := Ptr.Next;
        elsif Ptr.Previous /= null then
          AObj.Cache := Ptr.Previous;
          AObj.Cache_Index := AObj.Cache_Index - 1;
        else
          AObj.Cache := null;
          AObj.Cache_Index := 0;
        end if;
        Free (Ptr);
        Obj := AObj;
      end;
    end if;
  end Remove;

  procedure Replace (Obj : in out Unb_Node; Index : Positive; Elem : Item) is
  begin
    pragma Assert (Index <= Obj.Size );
    if not ((Obj.Cache /= null) and then (Index = Obj.Cache_Index)) then
      declare
        Ptr : Nodes.Node_Ref := Obj.Rep;
      begin
        for I in 1..Obj.Size loop
          if I = Index then
            Obj.Cache := Ptr;
            Obj.Cache_Index := I;
            exit;
          else
            Ptr := Ptr.Next;
          end if;
        end loop;
      end;
    end if;
    Obj.Cache.Element := Elem;
  end Replace;

  function Length (Obj : Unb_Node) return Natural is
  begin
    return Obj.Size;
  end Length;

  function First (Obj : Unb_Node) return Item is
  begin
    pragma Assert (Obj.Size > 0 );
    return Obj.Rep.Element;
  end First;

  function First (Obj : access Unb_Node) return Item_Ptr is
  begin
    pragma Assert (Obj.Size > 0 );
    return Obj.Rep.Element'access;
  end First;

  function Last (Obj : Unb_Node) return Item is
  begin
    pragma Assert (Obj.Size > 0 );
    return Obj.Last.Element;
  end ;

  function Last (Obj : access Unb_Node) return Item_Ptr is
  begin
    pragma Assert (Obj.Size > 0 );
    return  Obj.Last.Element'access;
  end ;

  function Item_At (Obj : access Unb_Node; Index : Positive) return Item is
    Tmp : Item_Ptr;
  begin
    Tmp := Item_At (Obj,Index);
    return Tmp.all;
  end Item_At;

  function Item_At (Obj : access Unb_Node; Index : Positive) return Item_Ptr is
  begin
    if Obj.Cache /= null then
      if Index = Obj.Cache_Index then
        return Obj.Cache.Element'access;
      elsif Index = Obj.Cache_Index + 1 then
        Obj.Cache := Obj.Cache.Next;
        Obj.Cache_Index := Obj.Cache_Index + 1;
        return Obj.Cache.Element'access;
      elsif Index = Obj.Cache_Index - 1 then
        Obj.Cache := Obj.Cache.Previous;
        Obj.Cache_Index := Obj.Cache_Index - 1;
        return Obj.Cache.Element'access;
      end if;
    end if;
    declare
      Ptr : Nodes.Node_Ref := Obj.Rep;
    begin
      for I in 1..Obj.Size loop
        if I = Index then
          Obj.Cache := Ptr;
          Obj.Cache_Index := I;
          return Ptr.Element'access;
        else
          Ptr := Ptr.Next;
        end if;
      end loop;
      return Ptr.Element'access;
    end;
  end Item_At;

  function Location (Obj : access Unb_Node; Elem : Item; Start : Positive := 1)
                     return Natural is
    Ptr : Nodes.Node_Ref := Obj.Rep;
  begin
    pragma Assert (Start < Obj.Size );
    if (Start = Obj.Cache_Index) and then (Elem = Obj.Cache.Element) then
      return Obj.Cache_Index;
    end if;
    for I in 1..Start-1 loop
      Ptr := Ptr.Next; -- advance to Start point
    end loop;
    for I in Start..Obj.Size loop
      if Ptr.Element = Elem then
        Obj.Cache := Ptr;
        Obj.Cache_Index := I;
        return I;
      else
        Ptr := Ptr.Next;
      end if;
    end loop;
    return 0;
  end Location;

end BC.Support.Unbounded;
