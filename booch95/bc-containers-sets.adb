--  Copyright (C) 1994-2002 Grady Booch and Simon Wright.
--  All Rights Reserved.
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

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

package body BC.Containers.Sets is

   function Are_Equal (L, R : Abstract_Set'Class) return Boolean is
      It : Iterator'Class := New_Iterator (L);
   begin
      --  XXX left out the optimisation which checks whether L, R are
      --  identical.
      if Extent (L) /= Extent (R) then
         return False;
      end if;
      while not Is_Done (It) loop
         if not Is_Member (R, Current_Item (It)) then
            return False;
         end if;
         Next (It);
      end loop;
      return True;
   end Are_Equal;

   procedure Union (S : in out Abstract_Set'Class; O : Abstract_Set'Class) is
      It : Iterator'Class := New_Iterator (O);
   begin
      --  XXX left out the optimisation which checks whether L, R are
      --  identical.
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

   procedure Intersection (S : in out Abstract_Set'Class;
                           O : Abstract_Set'Class) is
      Tmp : Abstract_Set'Class := S;
      It : Iterator'Class := New_Iterator (Tmp);
   begin
      --  XXX left out the optimisation which checks whether L, R are
      --  identical.
      while not Is_Done (It) loop
         declare
            This_Item : Item renames Current_Item (It);
         begin
            if not Is_Member (O, This_Item) then
               Detach (S, This_Item);
            end if;
            Next (It);
         end;
      end loop;
   end Intersection;

   procedure Difference (S : in out Abstract_Set'Class;
                         O : Abstract_Set'Class) is
      It : Iterator'Class := New_Iterator (O);
   begin
      --  XXX left out the optimisation which checks whether L, R are
      --  identical.
      while not Is_Done (It) loop
         declare
            This_Item : Item renames Current_Item (It);
         begin
            if Is_Member (S, This_Item) then
               Detach (S, This_Item);
            end if;
         end;
         Next (It);
      end loop;
   end Difference;

   function Is_Subset (S : Abstract_Set'Class;
                       O : Abstract_Set'Class) return Boolean is
      It : Iterator'Class := New_Iterator (S);
   begin
      --  XXX left out the optimisation which checks whether L, R are
      --  identical.
      if Extent (S) > Extent (O) then
         return False;
      end if;
      while not Is_Done (It) loop
         if not Is_Member (O, Current_Item (It)) then
            return False;
         end if;
         Next (It);
      end loop;
      return True;
   end Is_Subset;

   function Is_Proper_Subset (S : Abstract_Set'Class;
                              O : Abstract_Set'Class) return Boolean is
      It : Iterator'Class := New_Iterator (S);
   begin
      --  XXX left out the optimisation which checks whether L, R are
      --  identical.
      if Extent (S) >= Extent (O) then
         return False;
      end if;
      while not Is_Done (It) loop
         if not Is_Member (O, Current_Item (It)) then
            return False;
         end if;
         Next (It);
      end loop;
      return True;
   end Is_Proper_Subset;

   function Available (S : in Abstract_Set) return Natural is
      pragma Warnings (Off, S);
   begin
      return Natural'Last;
   end Available;

   --  Subprograms to be overridden

   procedure Attach (S : in out Abstract_Set; I : Item) is
   begin
      raise Should_Have_Been_Overridden;
   end Attach;

   procedure Detach (S : in out Abstract_Set; I : Item) is
   begin
      raise Should_Have_Been_Overridden;
   end Detach;

   function Number_Of_Buckets (S : Abstract_Set) return Natural is
   begin
      raise Should_Have_Been_Overridden;
      return 0;
   end Number_Of_Buckets;

   function Length (S : Abstract_Set; Bucket : Positive) return Natural is
   begin
      raise Should_Have_Been_Overridden;
      return 0;
   end Length;

   function Item_At (S : Abstract_Set;
                     Bucket, Index : Positive) return Item_Ptr is
   begin
      raise Should_Have_Been_Overridden;
      return null;
   end Item_At;

end BC.Containers.Sets;
