--  Copyright (C) 2001 Simon Wright.
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

package body BC.Simple_Collections is

   Empty_Container : Collection;
   pragma Warnings (Off, Empty_Container);

   function Null_Container return Collection is
   begin
      return Empty_Container;
   end Null_Container;

   function Null_Collection return Collection is
   begin
      return Null_Container;
   end Null_Collection;

   function "=" (Left, Right : in Collection) return Boolean is
   begin
      return Collections."=" (Collections.Collection (Left),
                              Collections.Collection (Right));
   end "=";

   procedure Clear (C : in out Collection) is
   begin
      Collections.Clear (Collections.Collection (C));
   end Clear;

   procedure Insert (C : in out Collection; Elem : Item) is
   begin
      Collections.Insert (Collections.Collection (C), Elem);
   end Insert;

   procedure Insert (C : in out Collection;
                     Elem : Item; Before : Positive) is
   begin
      Collections.Insert (Collections.Collection (C), Elem, Before);
   end Insert;

   procedure Append (C : in out Collection; Elem : Item) is
   begin
      Collections.Append (Collections.Collection (C), Elem);
   end Append;

   procedure Append (C : in out Collection;
                     Elem : Item;
                     After : Positive) is
   begin
      Collections.Append (Collections.Collection (C), Elem, After);
   end Append;

   procedure Remove (C : in out Collection;
                     At_Index : Positive) is
   begin
      Collections.Remove (Collections.Collection (C), At_Index);
   end Remove;

   procedure Replace (C : in out Collection;
                      At_Index : Positive; Elem : Item) is
   begin
      Collections.Replace (Collections.Collection (C), At_Index, Elem);
   end Replace;

   function Length (C : Collection) return Natural is
   begin
      return Collections.Length (Collections.Collection (C));
   end Length;

   function Is_Empty (C : Collection) return Boolean is
   begin
      return Collections.Is_Empty (Collections.Collection (C));
   end Is_Empty;

   function First (C : Collection) return Item is
   begin
      return Collections.First (Collections.Collection (C));
   end First;

   function Last (C : Collection) return Item is
   begin
      return Collections.Last (Collections.Collection (C));
   end Last;

   function Item_At (C : Collection;
                     At_Index : Positive) return Item is
   begin
      return Collections.Item_At (Collections.Collection (C), At_Index);
   end Item_At;

   function Location (C : Collection;
                      Elem : Item) return Natural is
   begin
      return Collections.Location (Collections.Collection (C), Elem);
   end Location;

   function New_Iterator
     (For_The_Collection : Collection) return Iterator'Class is
   begin
      return Collections.New_Iterator
        (Collections.Collection (For_The_Collection));
   end New_Iterator;

   procedure Reset (It : in out Iterator) is
   begin
      Abstract_Containers.Reset (It);
   end Reset;

   procedure Next (It : in out Iterator) is
   begin
      Abstract_Containers.Next (It);
   end Next;

   function Is_Done (It : Iterator) return Boolean is
   begin
      return Abstract_Containers.Is_Done (It);
   end Is_Done;

   function Current_Item (It : Iterator) return Item is
   begin
      return Abstract_Containers.Current_Item (It);
   end Current_Item;

end BC.Simple_Collections;
