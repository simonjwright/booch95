-- Copyright (C) 1994-1999 Grady Booch and Simon Wright.
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

package body BC.Containers.Sets is

  package BSE renames BC.Support.Exceptions;
  procedure Assert
  is new BSE.Assert ("BC.Containers.Sets");

  function Are_Equal (L, R : Set'Class) return Boolean is
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
       Next (It);
    end loop;
    return True;
  end Are_Equal;

  procedure Union (S : in out Set'Class; O : Set'Class) is
    It : Iterator := New_Iterator (O);
  begin
    -- XXX left out the optimisation which checks whether L, R are
    -- identical.
    while not Is_Done (It) loop
      declare
        This_Item : Item renames Current_Item (It);
      begin
        if not Is_Member (S, This_Item) then
          Attach (S, This_Item);
        end if;
      end;
      Next (It);
    end loop;
  end Union;

  procedure Intersection (S : in out Set'Class; O : Set'Class) is
    It : Iterator := New_Iterator (S);
  begin
    -- XXX left out the optimisation which checks whether L, R are
    -- identical.
    while not Is_Done (It) loop
      declare
        This_Item : Item renames Current_Item (It);
      begin
        if not Exists (O, This_Item) then
          Detach (S, This_Item);
        else
          Next (It);
        end if;
      end;
    end loop;
  end Intersection;

  procedure Difference (S : in out Set'Class; O : Set'Class) is
    It : Iterator := New_Iterator (O);
  begin
    -- XXX left out the optimisation which checks whether L, R are
    -- identical.
    while not Is_Done (It) loop
      declare
        This_Item : Item renames Current_Item (It);
      begin
        if Exists (S, This_Item) then
          Detach (S, This_Item);
        end if;
      end;
      Next (It);
    end loop;
  end Difference;

  function Is_Subset (S : Set'Class; O : Set'Class) return Boolean is
    It : Iterator := New_Iterator (S);
  begin
    -- XXX left out the optimisation which checks whether L, R are
    -- identical.
    if Cardinality (S) > Cardinality (O) then
      return False;
    end if;
    while not Is_Done (It) loop
      if not Exists (O, Current_Item (It)) then
        return False;
      end if;
      Next (It);
    end loop;
    return True;
  end Is_Subset;

  function Is_Proper_Subset (S : Set'Class; O : Set'Class) return Boolean is
    It : Iterator := New_Iterator (S);
  begin
    -- XXX left out the optimisation which checks whether L, R are
    -- identical.
    if Cardinality (S) >= Cardinality (O) then
      return False;
    end if;
    while not Is_Done (It) loop
      if not Exists (O, Current_Item (It)) then
        return False;
      end if;
      Next (It);
    end loop;
    return True;
  end Is_Proper_Subset;

  procedure Initialize (It : in out Set_Iterator) is
  begin
    It.Index := 0;
    if Cardinality (It.S.all) = 0 then
      It.Bucket_Index := 0;
    else
      It.Bucket_Index := 1;
      while It.Bucket_Index <= Number_Of_Buckets (It.S.all) loop
        if Length (It.S.all, It.Bucket_Index) > 0 then
          It.Index := 1;
          exit;
        end if;
        It.Bucket_Index := It.Bucket_Index + 1;
      end loop;
    end if;
  end Initialize;

  procedure Reset (It : in out Set_Iterator) is
  begin
    It.Index := 0;
    if Cardinality (It.S.all) = 0 then
      It.Bucket_Index := 0;
    else
      It.Bucket_Index := 1;
      while It.Bucket_Index <= Number_Of_Buckets (It.S.all) loop
        if Length (It.S.all, It.Bucket_Index) > 0 then
          It.Index := 1;
          exit;
        end if;
        It.Bucket_Index := It.Bucket_Index + 1;
      end loop;
    end if;
  end Reset;

  procedure Next (It : in out Set_Iterator) is
  begin
    if It.Bucket_Index <= Number_Of_Buckets (It.S.all) then
      if It.Index < Length (It.S.all, It.Bucket_Index) then
        It.Index := It.Index + 1;
      else
        It.Bucket_Index := It.Bucket_Index + 1;
        It.Index := 0;
        while It.Bucket_Index <= Number_Of_Buckets (It.S.all) loop
          if Length (It.S.all, It.Bucket_Index) > 0 then
            It.Index := 1;
            exit;
          end if;
          It.Bucket_Index := It.Bucket_Index + 1;
        end loop;
      end if;
    end if;
  end Next;

  function Is_Done (It : Set_Iterator) return Boolean is
  begin
    if It.Bucket_Index = 0
       or else It.Bucket_Index > Number_Of_Buckets (It.S.all) then
      return True;
    end if;
    if It.Index <= Length (It.S.all, It.Bucket_Index) then
      return False;
    end if;
    declare
      package Conversions is new System.Address_To_Access_Conversions
         (Set_Iterator'Class);
      P : Conversions.Object_Pointer := Conversions.To_Pointer (It'Address);
    begin
      P.Bucket_Index := P.Bucket_Index + 1;
      P.Index := 0;
      while P.Bucket_Index <= Number_Of_Buckets (P.S.all) loop
        if Length (P.S.all, P.Bucket_Index) > 0 then
          P.Index := 1;
          return False;
        end if;
        P.Bucket_Index := P.Bucket_Index + 1;
      end loop;
    end;
    return True;
  end Is_Done;

  function Current_Item (It : Set_Iterator) return Item is
  begin
    if Is_Done (It) then
      raise BC.Not_Found;
    end if;
    return Item_At (It.S.all, It.Bucket_Index, It.Index).all;
  end Current_Item;

  function Current_Item (It : Set_Iterator) return Item_Ptr is
    -- XXX this should probably not be permitted!
  begin
    if Is_Done (It) then
      raise BC.Not_Found;
    end if;
    return Item_At (It.S.all, It.Bucket_Index, It.Index);
  end Current_Item;

  -- Subprograms to be overridden

  procedure Attach (S : in out Set; I : Item) is
  begin
    raise Should_Have_Been_Overridden;
  end Attach;

  procedure Detach (S : in out Set; I : Item) is
  begin
    raise Should_Have_Been_Overridden;
  end Detach;

  function Number_Of_Buckets (S : Set) return Natural is
  begin
    raise Should_Have_Been_Overridden;
    return 0;
  end Number_Of_Buckets;

  function Length (S : Set; Bucket : Positive) return Natural is
  begin
    raise Should_Have_Been_Overridden;
    return 0;
  end Length;

  function Exists (S : Set; I : Item) return Boolean is
  begin
    raise Should_Have_Been_Overridden;
    return False;
  end Exists;

  function Item_At (S : Set; Bucket, Index : Positive) return Item_Ptr is
  begin
    raise Should_Have_Been_Overridden;
    return null;
  end Item_At;

end BC.Containers.Sets;
