-- Copyright (C) 1994-2001 Grady Booch and Simon Wright.
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

  function Are_Equal (L, R : Abstract_Map'Class) return Boolean is
    It : Map_Iterator'Class := Map_Iterator'Class (New_Iterator (L));
  begin
    -- XXX left out the optimisation which checks whether L, R are
    -- identical.
    if Extent (L) /= Extent (R) then
      return False;
    end if;
    while not Is_Done (It) loop
      if not Is_Bound (R, Current_Key (It)) then
        return False;
      end if;
      -- XXX what about the Value?
      Next (It);
    end loop;
    return True;
  end Are_Equal;

  function Available (M : Abstract_Map) return Natural is
  begin
    return Natural'Last;
  end Available;

  procedure Reset (It : in out Map_Iterator) is
    M : Abstract_Map'Class
       renames Abstract_Map'Class (It.For_The_Container.all);
  begin
    It.Index := 0;
    if Extent (M) = 0 then
      It.Bucket_Index := 0;
    else
      It.Bucket_Index := 1;
      while It.Bucket_Index <= Number_Of_Buckets (M) loop
        if Length (M, It.Bucket_Index) > 0 then
          It.Index := 1;
          exit;
        end if;
        It.Bucket_Index := It.Bucket_Index + 1;
      end loop;
    end if;
  end Reset;

  procedure Next (It : in out Map_Iterator) is
    M : Abstract_Map'Class
       renames Abstract_Map'Class (It.For_The_Container.all);
  begin
    if It.Bucket_Index <= Number_Of_Buckets (M) then
      if It.Index < Length (M, It.Bucket_Index) then
        It.Index := It.Index + 1;
      else
        It.Bucket_Index := It.Bucket_Index + 1;
        It.Index := 0;
        while It.Bucket_Index <= Number_Of_Buckets (M) loop
          if Length (M, It.Bucket_Index) > 0 then
            It.Index := 1;
            exit;
          end if;
          It.Bucket_Index := It.Bucket_Index + 1;
        end loop;
      end if;
    end if;
  end Next;

  function Is_Done (It : Map_Iterator) return Boolean is
    M : Abstract_Map'Class
    renames Abstract_Map'Class (It.For_The_Container.all);
  begin
    if It.Bucket_Index = 0
       or else It.Bucket_Index > Number_Of_Buckets (M) then
      return True;
    end if;
    if It.Index <= Length (M, It.Bucket_Index) then
      return False;
    end if;
    declare
      package Conversions is new System.Address_To_Access_Conversions
         (Map_Iterator'Class);
      P : Conversions.Object_Pointer := Conversions.To_Pointer (It'Address);
    begin
      P.Bucket_Index := P.Bucket_Index + 1;
      P.Index := 0;
      while P.Bucket_Index <= Number_Of_Buckets (M) loop
        if Length (M, P.Bucket_Index) > 0 then
          P.Index := 1;
          return False;
        end if;
        P.Bucket_Index := P.Bucket_Index + 1;
      end loop;
    end;
    return True;
  end Is_Done;

  function Current_Item (It : Map_Iterator) return Item is
    M : Abstract_Map'Class
    renames Abstract_Map'Class (It.For_The_Container.all);
  begin
    if Is_Done (It) then
      raise BC.Not_Found;
    end if;
    return Item_At (M, It.Bucket_Index, It.Index).all;
  end Current_Item;

  function Current_Item (It : Map_Iterator) return Item_Ptr is
    -- XXX this should probably not be permitted!
    M : Abstract_Map'Class
    renames Abstract_Map'Class (It.For_The_Container.all);
  begin
    if Is_Done (It) then
      raise BC.Not_Found;
    end if;
    return Item_At (M, It.Bucket_Index, It.Index);
  end Current_Item;

  procedure Delete_Item_At (It : in out Map_Iterator) is
  begin
    if Is_Done (It) then
      raise BC.Not_Found;
    end if;
    raise BC.Not_Yet_Implemented;
  end Delete_Item_At;

  function Current_Key (It : Map_Iterator) return Key is
    M : Abstract_Map'Class
    renames Abstract_Map'Class (It.For_The_Container.all);
  begin
    if Is_Done (It) then
      raise BC.Not_Found;
    end if;
    return Key_At (M,
                   It.Bucket_Index,
                   It.Index).all;
  end Current_Key;

  procedure Visit (Using : in out Map_Iterator'Class) is
    M : Abstract_Map'Class
       renames Abstract_Map'Class (Using.For_The_Container.all);
    Status : Boolean;
  begin
    Reset (Using);
    while not Is_Done (Using) loop
      Apply (Key_At (M,
                     Using.Bucket_Index,
                     Using.Index).all,
             Item_At (M,
                      Using.Bucket_Index,
                      Using.Index).all,
             Status);
      exit when not Status;
      Next (Using);
    end loop;
  end Visit;

  procedure Modify (Using : in out Map_Iterator'Class) is
    M : Abstract_Map'Class
       renames Abstract_Map'Class (Using.For_The_Container.all);
    Status : Boolean;
  begin
    Reset (Using);
    while not Is_Done (Using) loop
      Apply (Key_At (M,
                     Using.Bucket_Index,
                     Using.Index).all,
             Item_At (M,
                      Using.Bucket_Index,
                      Using.Index).all,
             Status);
      exit when not Status;
      Next (Using);
    end loop;
  end Modify;

  -- Subprograms to be overridden

  procedure Attach (M : in out Abstract_Map; K : Key; I : Item) is
  begin
    raise Should_Have_Been_Overridden;
  end Attach;

  function Number_Of_Buckets (M : Abstract_Map) return Natural is
  begin
    raise Should_Have_Been_Overridden;
    return 0;
  end Number_Of_Buckets;

  function Length (M : Abstract_Map; Bucket : Positive) return Natural is
  begin
    raise Should_Have_Been_Overridden;
    return 0;
  end Length;

  function Item_At (M : Abstract_Map;
                    Bucket, Index : Positive) return Item_Ptr is
  begin
    raise Should_Have_Been_Overridden;
    return null;
  end Item_At;

  function Key_At (M : Abstract_Map; Bucket, Index : Positive) return Key_Ptr is
  begin
    raise Should_Have_Been_Overridden;
    return null;
  end Key_At;

end BC.Containers.Maps;
