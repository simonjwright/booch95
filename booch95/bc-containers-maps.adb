-- Copyright (C) 1994-1998 Grady Booch and Simon Wright.
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

package body BC.Containers.Maps is

  package BSE renames BC.Support.Exceptions;
  procedure Assert
  is new BSE.Assert ("BC.Containers.Maps");

  function Are_Equal (L, R : Map'Class) return Boolean is
    It : Iterator := New_Iterator (L);
  begin
    -- XXX left out the optimisation which checks whether L, R are
    -- identical.
    if Cardinality (L) /= Cardinality (R) then
      return False;
    end if;
    while not Is_Done (It) loop
      if not Exists (R, Current_Item (It)) then
        return False;
      end if;
      -- XXX what about the Value?
      Next (It);
    end loop;
    return True;
  end Are_Equal;

  procedure Initialize (It : in out Map_Iterator) is
  begin
    It.Index := 0;
    if Cardinality (It.M.all) = 0 then
      It.Bucket_Index := 0;
    else
      It.Bucket_Index := 1;
      while It.Bucket_Index <= Number_Of_Buckets (It.M.all) loop
        if Length (It.M.all, It.Bucket_Index) > 0 then
          It.Index := 1;
          exit;
        end if;
        It.Bucket_Index := It.Bucket_Index + 1;
      end loop;
    end if;
  end Initialize;

  procedure Reset (It : in out Map_Iterator) is
  begin
    It.Index := 0;
    if Cardinality (It.M.all) = 0 then
      It.Bucket_Index := 0;
    else
      It.Bucket_Index := 1;
      while It.Bucket_Index <= Number_Of_Buckets (It.M.all) loop
        if Length (It.M.all, It.Bucket_Index) > 0 then
          It.Index := 1;
          exit;
        end if;
        It.Bucket_Index := It.Bucket_Index + 1;
      end loop;
    end if;
  end Reset;

  procedure Next (It : in out Map_Iterator) is
  begin
    if It.Bucket_Index <= Number_Of_Buckets (It.M.all) then
      if It.Index < Length (It.M.all, It.Bucket_Index) then
        It.Index := It.Index + 1;
      else
        It.Bucket_Index := It.Bucket_Index + 1;
        It.Index := 0;
        while It.Bucket_Index <= Number_Of_Buckets (It.M.all) loop
          if Length (It.M.all, It.Bucket_Index) > 0 then
            It.Index := 1;
            exit;
          end if;
          It.Bucket_Index := It.Bucket_Index + 1;
        end loop;
      end if;
    end if;
  end Next;

  function Is_Done (It : Map_Iterator) return Boolean is
  begin
    if It.Bucket_Index = 0
       or else It.Bucket_Index > Number_Of_Buckets (It.M.all) then
      return True;
    end if;
    if It.Index <= Length (It.M.all, It.Bucket_Index) then
      return False;
    end if;
    declare
      package Conversions is new System.Address_To_Access_Conversions
         (Map_Iterator'Class);
      P : Conversions.Object_Pointer := Conversions.To_Pointer (It'Address);
    begin
      P.Bucket_Index := P.Bucket_Index + 1;
      P.Index := 0;
      while P.Bucket_Index <= Number_Of_Buckets (P.M.all) loop
        if Length (P.M.all, P.Bucket_Index) > 0 then
          P.Index := 1;
          return False;
        end if;
        P.Bucket_Index := P.Bucket_Index + 1;
      end loop;
    end;
    return True;
  end Is_Done;

  function Current_Item (It : Map_Iterator) return Item is
  begin
    if Is_Done (It) then
      raise BC.Not_Found;
    end if;
    return Item_At (It.M.all, It.Bucket_Index, It.Index).all;
  end Current_Item;

  function Current_Item (It : Map_Iterator) return Item_Ptr is
    -- XXX this should probably not be permitted!
  begin
    if Is_Done (It) then
      raise BC.Not_Found;
    end if;
    return Item_At (It.M.all, It.Bucket_Index, It.Index);
  end Current_Item;

  function Current_Value (It : Iterator) return Value is
    Map_Iter : Map_Iterator
       renames Map_Iterator (SP.Value (SP.Pointer (It)).all);
  begin
    if Is_Done (Map_Iter) then
      raise BC.Not_Found;
    end if;
    return Value_At (Map_Iter.M.all, Map_Iter.Bucket_Index, Map_Iter.Index).all;
  end Current_Value;

  procedure Visit is
    Iter : Iterator := New_Iterator (Over_The_Container);
    Map_Iter : Map_Iterator
       renames Map_Iterator (SP.Value (SP.Pointer (Iter)).all);
    Status : Boolean;
  begin
    while not Is_Done (Iter) loop
      Apply (Item_At (Over_The_Container,
                      Map_Iter.Bucket_Index,
                      Map_Iter.Index).all,
             Value_At (Over_The_Container,
                       Map_Iter.Bucket_Index,
                       Map_Iter.Index).all,
             Status);
      exit when not Status;
      Next (Iter);
    end loop;
  end Visit;

  procedure Modify is
    Iter : Iterator := New_Iterator (Over_The_Container);
    Map_Iter : Map_Iterator
       renames Map_Iterator (SP.Value (SP.Pointer (Iter)).all);
    Status : Boolean;
  begin
    while not Is_Done (Iter) loop
      Apply (Item_At (Over_The_Container,
                      Map_Iter.Bucket_Index,
                      Map_Iter.Index).all,
             Value_At (Over_The_Container,
                       Map_Iter.Bucket_Index,
                       Map_Iter.Index).all,
             Status);
      exit when not Status;
      Next (Iter);
    end loop;
  end Modify;

  -- Subprograms to be overridden

  procedure Attach (M : in out Map; I : Item; V : Value) is
  begin
    raise Should_Have_Been_Overridden;
  end Attach;

  function Number_Of_Buckets (M : Map) return Natural is
  begin
    raise Should_Have_Been_Overridden;
    return 0;
  end Number_Of_Buckets;

  function Length (M : Map; Bucket : Positive) return Natural is
  begin
    raise Should_Have_Been_Overridden;
    return 0;
  end Length;

  function Exists (M : Map; I : Item) return Boolean is
  begin
    raise Should_Have_Been_Overridden;
    return False;
  end Exists;

  function Item_At (M : Map; Bucket, Index : Positive) return Item_Ptr is
  begin
    raise Should_Have_Been_Overridden;
    return null;
  end Item_At;

  function Value_At (M : Map; Bucket, Index : Positive) return Value_Ptr is
  begin
    raise Should_Have_Been_Overridden;
    return null;
  end Value_At;

end BC.Containers.Maps;
