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

package body BC.Containers.Bags is

   function Are_Equal (L, R : Abstract_Bag'Class) return Boolean is
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
      --  XXX left out the optimisation which checks whether L, R are
      --  identical.
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
      Tmp : Abstract_Bag'Class := B;
      It : Iterator'Class := New_Iterator (Tmp);
   begin
      --  XXX left out the optimisation which checks whether L, R are
      --  identical.
      while not Is_Done (It) loop
         declare
            This_Item : Item renames Current_Item (It);
            B_Count : Positive := Count (B, This_Item);
         begin
            if not Is_Member (O, This_Item) then
               Detach (B, This_Item);
            else
               declare
                  O_Count : Positive := Count (O, This_Item);
               begin
                  if B_Count > O_Count then
                     Set_Value (B, This_Item, O_Count);
                  end if;
               end;
            end if;
            Next (It);
         end;
      end loop;
   end Intersection;

   procedure Difference
     (B : in out Abstract_Bag'Class; O : Abstract_Bag'Class) is
      It : Iterator'Class := New_Iterator (O);
   begin
      --  XXX left out the optimisation which checks whether L, R are
      --  identical.
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
      --  XXX left out the optimisation which checks whether L, R are
      --  identical.
      if Extent (B) > Extent (O) then
         return False;
      end if;
      while not Is_Done (It) loop
         declare
            This_Item : Item := Current_Item (It);
         begin
            --  why don't I just do "or else Count (B, This_Item) >
            --  Count (O, This_Item)"? .. because it triggered a
            --  compiler bug in GNAT 3.11p (or was it 3.11b2?)
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
      --  XXX left out the optimisation which checks whether L, R are
      --  identical.
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
      pragma Warnings (Off, B);
   begin
      return Natural'Last;
   end Available;


   --  Subprograms to be overridden

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

end BC.Containers.Bags;
