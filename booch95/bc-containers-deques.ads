--  Copyright (C) 1994-2001 Grady Booch and Simon Wright.
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

generic
package BC.Containers.Deques is

   pragma Elaborate_Body;

   type Deque_End is (Front, Back);

   type Abstract_Deque is abstract new Container with private;

   --  A deque denotes a sequence of items, in which items may be
   --  added and removed from either end of the sequence.

   procedure Clear (D : in out Abstract_Deque) is abstract;
   --  Empty the deque of all items.

   procedure Append (D : in out Abstract_Deque;
                     Elem : Item;
                     Location : Deque_End := Back) is abstract;
   --  Add the item to the deque at the given location; the item
   --  itself is copied.

   procedure Pop (D : in out Abstract_Deque;
                  Location : Deque_End := Front) is abstract;
   --  Remove the item from the deque at the given location.

   procedure Remove (D : in out Abstract_Deque; From : Positive) is abstract;
   --  Remove the item at the given index (may be a balking operation).

   function Available (D : in Abstract_Deque) return Natural;
   --  Indicates number of empty "Item slots" left in Deque

   function Length (D : in Abstract_Deque) return Natural is abstract;
   --  Return the number of items in the deque.

   function Is_Empty (D : in Abstract_Deque) return Boolean is abstract;
   --  Return True if and only if there are no items in the deque.

   function Front (D : in Abstract_Deque) return Item is abstract;
   --  Return a copy of the item at the front of the deque.

   generic
      with procedure Process (Elem : in out Item);
   procedure Process_Front (D : in out Abstract_Deque'Class);
   --  Access the item at the front of the deque.

   function Back (D : in Abstract_Deque) return Item is abstract;
   --  Return a copy of the item at the back of the deque.

   generic
      with procedure Process (Elem : in out Item);
   procedure Process_Back (D : in out Abstract_Deque'Class);
   --  Access the item at the back of the deque.

   function Location (D : in Abstract_Deque; Elem : in Item) return Natural
      is abstract;
   --  Return the first index at which the item is found; return 0 if
   --  the item does not exist in the deque.

   function Are_Equal (Left, Right : Abstract_Deque'Class) return Boolean;
   --  Return True if and only if both deques have the same length and
   --  the same items in the same order; return False otherwise.

   procedure Copy (From : Abstract_Deque'Class;
                   To : in out Abstract_Deque'Class);
   --  This operation MUST be called for dissimilar Deques in place of
   --  assignment.

private

   type Abstract_Deque is abstract new Container with null record;

   type Deque_Iterator is new Iterator with record
      Index : Natural;
   end record;

   --  Overriding primitive supbrograms of the concrete actual Iterator.

   procedure Reset (It : in out Deque_Iterator);

   procedure Next (It : in out Deque_Iterator);

   function Is_Done (It : Deque_Iterator) return Boolean;

   function Current_Item (It : Deque_Iterator) return Item;

   function Current_Item_Ptr (It : Deque_Iterator) return Item_Ptr;

   procedure Delete_Item_At (It : in out Deque_Iterator);

end BC.Containers.Deques;
