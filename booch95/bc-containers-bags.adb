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

package body BC.Containers.Bags is

  package BSE renames BC.Support.Exceptions;
  procedure Assert
  is new BSE.Assert ("BC.Containers.Bags");

  function Are_Equal (L, R : Abstract_Bag'Class) return Boolean is
    It : Iterator'Class := New_Iterator (L);
  begin
    -- XXX left out the optimisation which checks whether L, R are
    -- identical.
    if Extent (L) /= Extent (R) then
      return False;
    end if;
    while not Is_Done (It) loop
      if not Is_Member (R, Current_Item (It)) then
        return False;
      end if;
      if Count (L, Current_Item (It)) /= Count (R, Current_Item (It)) then
        return False;
      end if;
      Next (It);
    end loop;
    return True;
  end Are_Equal;

  procedure Add (B : in out Abstract_Bag'Class; I : Item) is
    Dummy : Boolean;
  begin
    Add (B, I, Added => Dummy);
  end Add;

  procedure Union (B : in out Abstract_Bag'Class; O : Abstract_Bag'Class) is
    It : Iterator'Class := New_Iterator (O);
  begin
    -- XXX left out the optimisation which checks whether L, R are
    -- identical.
    while not Is_Done (It) loop
      declare
        This_Item : Item renames Current_Item (It);
        This_Count : Positive := Count (O, This_Item);
      begin
        if not Is_Member (B, This_Item) then
          Attach (B, This_Item, This_Count);
        else
          Set_Value (B, This_Item, Count (B, This_Item) + This_Count);
        end if;
      end;
      Next (It);
    end loop;
  end Union;

  procedure Intersection
     (B : in out Abstract_Bag'Class; O : Abstract_Bag'Class) is
    It : Iterator'Class := New_Iterator (B);
  begin
    -- XXX left out the optimisation which checks whether L, R are
    -- identical.
    while not Is_Done (It) loop
      declare
        This_Item : Item renames Current_Item (It);
        B_Count : Positive := Count (B, This_Item);
      begin
        if not Is_Member (O, This_Item) then
          Detach (B, This_Item);
--           Delete_Item_At (It);
        else
          declare
            O_Count : Positive := Count (O, This_Item);
          begin
            if B_Count > O_Count then
              Set_Value (B, This_Item, O_Count);
            end if;
          end;
          Next (It);
        end if;
      end;
    end loop;
  end Intersection;

  procedure Difference
     (B : in out Abstract_Bag'Class; O : Abstract_Bag'Class) is
    It : Iterator'Class := New_Iterator (O);
  begin
    -- XXX left out the optimisation which checks whether L, R are
    -- identical.
    while not Is_Done (It) loop
      declare
        This_Item : Item renames Current_Item (It);
      begin
        if Is_Member (B, This_Item) then
          declare
            B_Count : Positive := Count (B, This_Item);
            O_Count : Positive := Count (O, This_Item);
          begin
            if B_Count <= O_Count then
              Detach (B, This_Item);
            else
              Set_Value (B, This_Item, B_Count - O_Count);
            end if;
          end;
        end if;
      end;
      Next (It);
    end loop;
  end Difference;

  function Total_Size (B : Abstract_Bag'Class) return Natural is
    It : Iterator'Class := New_Iterator (B);
    Result : Natural := 0;
  begin
    while not Is_Done (It) loop
      Result := Result + Count (B, Current_Item (It));
      Next (It);
    end loop;
    return Result;
  end Total_Size;

  function Is_Subset
     (B : Abstract_Bag'Class; O : Abstract_Bag'Class) return Boolean is
    It : Iterator'Class := New_Iterator (B);
  begin
    -- XXX left out the optimisation which checks whether L, R are
    -- identical.
    if Extent (B) > Extent (O) then
      return False;
    end if;
    while not Is_Done (It) loop
      declare
        This_Item : Item := Current_Item (It);
      begin
        -- why don't I just do "or else Count (B, This_Item) > Count (O,
        -- This_Item)"? .. because it triggered a compiler bug in GNAT
        -- 3.11p (or was it 3.11b2?)
        if not Is_Member (O, This_Item) then
          return False;
        else
          declare
            B_Count : Positive := Count (B, This_Item);
            O_Count : Positive := Count (O, This_Item);
          begin
            if B_Count > O_Count then
              return False;
            end if;
          end;
        end if;
      end;
      Next (It);
    end loop;
    return True;
  end Is_Subset;

  function Is_Proper_Subset
     (B : Abstract_Bag'Class; O : Abstract_Bag'Class) return Boolean is
    It : Iterator'Class := New_Iterator (B);
    Is_Proper : Boolean := False;
  begin
    -- XXX left out the optimisation which checks whether L, R are
    -- identical.
    if Extent (B) > Extent (O) then
      return False;
    end if;
    while not Is_Done (It) loop
      declare
        This_Item : Item renames Current_Item (It);
      begin
        if not Is_Member (O, This_Item) then
          return False;
        else
          declare
            B_Count : Positive := Count (B, This_Item);
            O_Count : Positive := Count (O, This_Item);
          begin
            if B_Count > O_Count then
              return False;
            elsif B_Count < O_Count then
                Is_Proper := True;
            end if;
          end;
        end if;
      end;
      Next (It);
    end loop;
    return Is_Proper or else Extent (B) < Extent (O);
  end Is_Proper_Subset;

  function Available (B : Abstract_Bag) return Natural is
  begin
    return Natural'Last;
  end Available;


  -- Subprograms to be overridden

  procedure Attach (B : in out Abstract_Bag; I : Item; C : Positive) is
  begin
    raise Should_Have_Been_Overridden;
  end Attach;

  procedure Detach (B : in out Abstract_Bag; I : Item) is
  begin
    raise Should_Have_Been_Overridden;
  end Detach;

  procedure Set_Value (B : in out Abstract_Bag; I : Item; C : Positive) is
  begin
    raise Should_Have_Been_Overridden;
  end Set_Value;

  function Multiplicity (B : Abstract_Bag'Class) return Natural is
    It : Iterator'Class := New_Iterator (B);
    Result : Natural := 0;
  begin
    while not Is_Done (It) loop
      Result := Result + Count (B, Current_Item (It));
      Next (It);
    end loop;
    return Result;
  end Multiplicity;

  function Number_Of_Buckets (B : Abstract_Bag) return Natural is
  begin
    raise Should_Have_Been_Overridden;
    return 0;
  end Number_Of_Buckets;

  function Length (B : Abstract_Bag; Bucket : Positive) return Natural is
  begin
    raise Should_Have_Been_Overridden;
    return 0;
  end Length;

  function Item_At
     (B : Abstract_Bag; Bucket, Index : Positive) return Item_Ptr is
  begin
    raise Should_Have_Been_Overridden;
    return null;
  end Item_At;

  function Value_At
     (B : Abstract_Bag; Bucket, Index : Positive) return Positive is
  begin
    raise Should_Have_Been_Overridden;
    return 1;
  end Value_At;

  -- Iterators

  procedure Reset (It : in out Bag_Iterator) is
    B : Abstract_Bag'Class
       renames Abstract_Bag'Class (It.For_The_Container.all);
  begin
    It.Index := 0;
    if Extent (B) = 0 then
      It.Bucket_Index := 0;
    else
      It.Bucket_Index := 1;
      while It.Bucket_Index <= Number_Of_Buckets (B) loop
        if Length (B, It.Bucket_Index) > 0 then
          It.Index := 1;
          exit;
        end if;
        It.Bucket_Index := It.Bucket_Index + 1;
      end loop;
    end if;
  end Reset;

  procedure Next (It : in out Bag_Iterator) is
    B : Abstract_Bag'Class
       renames Abstract_Bag'Class (It.For_The_Container.all);
  begin
    if It.Bucket_Index <= Number_Of_Buckets (B) then
      if It.Index < Length (B, It.Bucket_Index) then
        It.Index := It.Index + 1;
      else
        It.Bucket_Index := It.Bucket_Index + 1;
        It.Index := 0;
        while It.Bucket_Index <= Number_Of_Buckets (B) loop
          if Length (B, It.Bucket_Index) > 0 then
            It.Index := 1;
            exit;
          end if;
          It.Bucket_Index := It.Bucket_Index + 1;
        end loop;
      end if;
    end if;
  end Next;

  function Is_Done (It : Bag_Iterator) return Boolean is
    B : Abstract_Bag'Class
    renames Abstract_Bag'Class (It.For_The_Container.all);
  begin
    if It.Bucket_Index = 0
       or else It.Bucket_Index > Number_Of_Buckets (B)
    then
      return True;
    end if;
    if It.Index <= Length (B, It.Bucket_Index) then
      return False;
    end if;
    -- At the point where we call Is_Done, it's possible that the
    -- Iterator may not be completely synchronised (for example, see
    -- Intersection, where we've either called Next or Detach).
    --
    -- This means we may need to write to the Iterator; but it's an in
    -- parameter.
    declare
      package Conversions is new System.Address_To_Access_Conversions
         (Bag_Iterator'Class);
      It_W : Bag_Iterator'Class renames Conversions.To_Pointer (It'Address).all;
    begin
      It_W.Bucket_Index := It_W.Bucket_Index + 1;
      It_W.Index := 0;
      while It_W.Bucket_Index <= Number_Of_Buckets (B)
      loop
        if Length (B, It_W.Bucket_Index) > 0 then
          It_W.Index := 1;
          return False;
        end if;
        It_W.Bucket_Index := It_W.Bucket_Index + 1;
      end loop;
    end;
    return True;
  end Is_Done;

  function Current_Item (It : Bag_Iterator) return Item is
    B : Abstract_Bag'Class
    renames Abstract_Bag'Class (It.For_The_Container.all);
  begin
    if Is_Done (It) then
      raise BC.Not_Found;
    end if;
    return Item_At (B, It.Bucket_Index, It.Index).all;
  end Current_Item;

  function Current_Item_Ptr (It : Bag_Iterator) return Item_Ptr is
    -- XXX this should probably not be permitted!
    B : Abstract_Bag'Class
    renames Abstract_Bag'Class (It.For_The_Container.all);
  begin
    if Is_Done (It) then
      raise BC.Not_Found;
    end if;
    return Item_At (B, It.Bucket_Index, It.Index);
  end Current_Item_Ptr;

  procedure Delete_Item_At (It : in out Bag_Iterator) is
  begin
    if Is_Done (It) then
      raise BC.Not_Found;
    end if;
    raise BC.Not_Yet_Implemented;
  end Delete_Item_At;

end BC.Containers.Bags;
