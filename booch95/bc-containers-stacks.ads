--The C++ Booch Components (Version 2.3)
--(C) Copyright 1990-1994 Grady Booch. All Rights Reserved..
--
--BCStac.h
--
--This file contains the declaration of the stack abstract base class
--and its iterators.

generic
package BC.Containers.Stacks is 

   type Stack is abstract tagged private;
    
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

private

   type Stack is abstract tagged null record;

end BC.Containers.Stacks;
