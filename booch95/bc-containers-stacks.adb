--The C++ Booch Components (Version 2.3)
--(C) Copyright 1990-1994 Grady Booch. All Rights Reserved..
--
--BCStac.h
--
--This file contains the declaration of the stack abstract base class
--and its iterators.

package body BC.Containers.Stacks is 

   function "="(L, R : Stack) return Boolean is
   begin
   end "=";
    
   procedure Clear  (Obj : in out Stack) is abstract;
   procedure Push   (Obj : in out Stack; Elem : Item) is abstract;
   procedure Pop    (Obj : in out Stack) is abstract;
   function Depth  (Obj : Stack) return Natural is abstract;
   function Is_Empty(Obj : Stack) return Boolean is abstract;
   function Top     (Obj : Stack) return Item is abstract;

   -- expected to be implemented as private methods.  Not permitted
   -- in private part because they are abstract.

   function Item_At(Obj : Stack; Index : Natural) return Item is abstract;
   procedure Purge(Obj : in out Stack) is abstract;
   procedure Add(Obj : in out Stack; Elem : in out Item) is abstract;
   function Cardinality(Obj : Stack) return Integer is abstract;

   -- Returning to normal

   function "="(Left, Right : access Stack'Class) return Boolean;
   procedure Copy(From : access Stack'Class; To : access Stack'Class);

   procedure Reset(Obj : in out Iterator);
   procedure Next(Obj : in out Iterator);
   function Is_Done(Obj : Iterator) return Boolean;
   function Current_Item(Obj : Iterator) return Item;

   procedure Set_Apply_Method(Obj : in out Passive_Iterator;
			      Iter : in Apply_Method);

   procedure Apply( Obj : in out Passive_Iterator) is abstract;

end BC.Containers.Stacks;
